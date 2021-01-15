//! Supporting Slash Commands for `mbot`.
use ::serde::{Deserialize, Serialize};
use ::serde_repr::Serialize_repr;
use ::std::fs::File;
use ::std::io::{self, BufReader, BufWriter};
use ::std::path::{Path, PathBuf};
use ::std::time::Duration;
use ::structopt::StructOpt;
// Reqwest still uses Tokio 0.2,
// but tokio-rustls uses Tokio 0.3.
use ::futures::sink::SinkExt;
use ::thiserror::Error;
use ::tokio::stream::StreamExt;
use ::tokio::task;
use ::tokio::time;
use ::tokio_compat_02::FutureExt;

use ::reqwest::Url;
mod url_opt {
    use ::reqwest::Url;
    use ::serde::{Deserialize, Deserializer, Serializer};
    pub(crate) fn serialize<S>(url: &Option<Url>, ser: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match url {
            Some(url) => ser.serialize_some(url.as_str()),
            None => ser.serialize_none(),
        }
    }
    pub(crate) fn deserialize<'de, D>(d: D) -> Result<Option<Url>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let string: Option<String> = Deserialize::deserialize(d)?;
        match string {
            Some(string) => {
                let url = Url::parse(&string);
                match url {
                    Ok(url) => Ok(Some(url)),
                    Err(_) => Ok(None),
                }
            }
            None => Ok(None),
        }
    }
}

mod api {
    use ::reqwest::Url;
    /// Discord API base URI.
    pub(crate) const BASE: &str = "https://discord.com/api";
    /// Discord API version against which this is written.
    // I'm not going to attempt to support older versions of the Discord API,
    // since nobody but Discord provides it.
    pub(crate) const VERSION: u8 = 8;
    pub(crate) mod endpoint {
        pub(crate) fn guild_slash(application_id: u64, guild_id: u64) -> String {
            format!(
                "{}/v{}/applications/{}/guilds/{}/commands",
                super::BASE,
                super::VERSION,
                application_id,
                guild_id
            )
        }
        pub(crate) fn global_slash(application_id: u64) -> String {
            format!(
                "{}/v{}/applications/{}/commands",
                super::BASE,
                super::VERSION,
                application_id
            )
        }
        pub(crate) fn gateway() -> String {
            format!("{}/v{}/gateway", super::BASE, super::VERSION)
        }
    }
    /// Calling this with a Slash Command that uses an already used name
    /// for your application will updated the existing command.
    pub(crate) async fn upsert_slash(
        client: &::reqwest::Client,
        application_id: super::ApplicationId,
        auth: &super::Auth,
        slash: &super::Slash,
    ) -> Result<String, ::reqwest::Error> {
        use super::{Auth, GuildId, Slash};
        match slash {
            Slash::Guild {
                schema,
                guild_id: GuildId { id: guild_id },
            } => {
                client
                    .post(&endpoint::guild_slash(application_id.id, *guild_id))
                    .json(&schema)
                    .header(
                        "Authorization",
                        match auth {
                            Auth::BotToken(token) => format!("Bot {}", token),
                            Auth::BearerToken(token) => format!("Bearer {}", token),
                        },
                    )
                    .send()
                    .await?
                    .text()
                    .await
            }
            // This is the same as above, but with a different endpoint and no guild_id.
            Slash::Global { .. } => todo!("registering global slashes"),
        }
    }
    /// Returns a single valid WSS URI, which we can use for connecting to the Gateway.
    /// Clients *should* cache this value, and only call this if they are unable to
    /// properly establish a connection using the cached version of the URI.
    pub(crate) async fn get_gateway(client: &::reqwest::Client) -> Result<Url, ::reqwest::Error> {
        #[derive(::serde::Deserialize)]
        struct GetGateway {
            url: String,
        }
        client
            .get(&endpoint::gateway())
            .send()
            .await?
            .json()
            .await
            .map(|x: GetGateway| {
                // This could be handled, but I don't care.
                Url::parse(&x.url).expect("discord is sending us bad data")
            })
    }
    pub(crate) async fn reply_gateway_interaction(
        client: &::reqwest::Client,
        interaction: super::gateway::Snowflake,
        token: String,
        data: String,
    ) -> Result<(), ::reqwest::Error> {
        client
            .post(&format!(
                "{}/v{}/interactions/{}/{}/callback",
                BASE, VERSION, interaction.0 .0, token
            ))
            .json(&::serde_json::json!({
                "type": 4,
                "data": {
                    "content": data
                }
            }))
            .send()
            .await
            .map(|_| ())
    }
    use ::serde_aux::field_attributes::deserialize_number_from_string;
    /// A 64 bit integer type that deserializes Discord's stringed up integers just fine.
    #[derive(serde::Serialize, serde::Deserialize)]
    #[serde(transparent)]
    pub(crate) struct U64(#[serde(deserialize_with = "deserialize_number_from_string")] pub u64);
    impl ::core::fmt::Debug for U64 {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            write!(f, "{:?}", self.0)
        }
    }
}

// Note that Discord sort of distinguishes between omission of fields and null values
// for those fields, although they appear to have the same semantics on their end.
// So, we skip serialization of all nulled optional fields where omission is allowed.
mod gateway {
    use super::api::U64;
    use ::serde::{Deserialize, Serialize};
    use ::serde_repr::{Deserialize_repr, Serialize_repr};
    /// The Discord Gateway is versioned separately from the HTTP APIs.
    /// This is the Gateway version against which this is written.
    pub(crate) const VERSION: u8 = 8;
    /// Port to connect to for our WebSockets WSS connection.
    pub(crate) const PORT: u16 = 443;
    // Payloads are limited to a maximum of 4096 bytes sent, going over this will
    // cause a connection termination with error code 4002.
    #[derive(Serialize, Deserialize, Debug)]
    pub(crate) struct Payload<T> {
        #[serde(rename = "op")]
        pub(crate) opcode: Opcode,
        // This will sometimes not be anything.
        // Whether this field is actually nullable depends
        // on the opcode we're dealing with.
        #[serde(rename = "d")]
        pub(crate) data: T,
        // This and event_name will always be null when
        // the opcode isn't Opcode::Dispatch.
        #[serde(rename = "s")]
        pub(crate) sequence_number: Option<SequenceNumber>,
        #[serde(rename = "t")]
        pub(crate) event_name: Option<String>,
    }
    /// [Opcode](https://discord.com/developers/docs/topics/opcodes-and-status-codes#gateway-gateway-opcodes)
    #[derive(Serialize_repr, Deserialize_repr, Debug)]
    #[repr(u8)]
    pub(crate) enum Opcode {
        // Receive
        Dispatch = 0,
        // Send/Receive
        Heartbeat = 1,
        // Send
        Identify = 2,
        // Send
        PresenceUpdate = 3,
        // Send
        VoiceStateUpdate = 4,
        // Send
        Resume = 6,
        // Receive
        Reconnect = 7,
        // Send
        RequestGuildMembers = 8,
        // Receive
        InvalidSession = 9,
        // Receive
        Hello = 10,
        // Receive
        HeartbeatACK = 11,
    }
    #[derive(Serialize, Deserialize, Copy, Clone, Debug)]
    #[serde(transparent)]
    pub(crate) struct SequenceNumber(u32);
    #[derive(Deserialize)]
    pub(crate) struct HelloData {
        pub(crate) heartbeat_interval: u32,
    }
    /// [Unavailable Guild](https://discord.com/developers/docs/resources/guild#unavailable-guild-object)
    #[derive(Deserialize)]
    struct UnavailableGuild {
        id: U64,
    }
    /// [Partial Application](https://discord.com/developers/docs/topics/gateway#ready-ready-event-fields)
    #[derive(Deserialize)]
    struct PartialApplication {
        id: Snowflake,
        flags: u64,
    }
    // TODO: make this use a Cow<'a, str> or something
    #[derive(Serialize, Deserialize, Clone)]
    #[serde(transparent)]
    pub(crate) struct SessionId(String);
    /// [Ready](https://discord.com/developers/docs/topics/gateway#ready)
    #[derive(Deserialize)]
    pub(crate) struct Ready {
        // TODO: ensure each of these fields is correct
        // #[serde(rename = "v")]
        // pub(crate) version: u8,
        // pub(crate) user: User,
        // pub(crate) private_channels: [(); 0],
        // guilds: Vec<UnavailableGuild>,
        pub(crate) session_id: SessionId,
        // pub(crate) shard: ShardData,
        // application: PartialApplication,
    }
    #[derive(Deserialize)]
    pub(crate) struct DispatchData {}
    pub(crate) enum Intent {
        Guilds = 0,
        GuildMembers = 1,
        GuildBans = 2,
        GuildEmojis = 3,
        GuildIntegrations = 4,
        GuildWebhooks = 5,
        GuildInvites = 6,
        GuildVoiceStates = 7,
        GuildPresences = 8,
        GuildMessages = 9,
        GuildMessageReactions = 10,
        GuildMessageTyping = 11,
        DirectMessages = 12,
        DirectMessageReactions = 13,
        DirectMessageTyping = 14,
    }
    #[derive(Clone, Copy, Serialize, Deserialize)]
    #[serde(transparent)]
    pub(crate) struct Intents(u16);
    impl Intents {
        pub(crate) fn empty() -> Self {
            Self(0)
        }
        pub(crate) fn add_intent(self, intent: Intent) -> Self {
            Self(self.0 | (1 << (intent as u8)))
        }
    }
    #[derive(Serialize)]
    pub(crate) struct ConnectionProperties {
        #[serde(rename = "$os")]
        pub(crate) os: String,
        #[serde(rename = "$browser")]
        pub(crate) browser: String,
        #[serde(rename = "$device")]
        pub(crate) device: String,
    }
    #[derive(Serialize_repr)]
    #[repr(u8)]
    pub(crate) enum ActivityType {
        Game = 0,
        Streaming = 1,
        Listening = 2,
        Custom = 4,
        Competing = 5,
    }
    #[derive(Serialize)]
    pub(crate) struct ActivityTimestamps {
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) start: Option<u32>,
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) end: Option<u32>,
    }
    #[derive(Serialize, Deserialize, Debug)]
    #[serde(transparent)]
    pub(crate) struct Snowflake(pub(crate) U64);
    #[derive(Serialize)]
    pub(crate) struct ActivityEmoji {
        pub(crate) name: String,
        // `null` is anot allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) id: Option<Snowflake>,
        pub(crate) animated: bool,
    }
    #[derive(Serialize)]
    pub(crate) enum StatusType {
        #[serde(rename = "online")]
        Online,
        #[serde(rename = "dnd")]
        DoNotDisturb,
        #[serde(rename = "idle")]
        Idle,
        #[serde(rename = "invisible")]
        Invisible,
        #[serde(rename = "offline")]
        Offline,
    }
    // Bots are only allowed to send these fields of the `Activity` struct.
    #[derive(Serialize)]
    pub(crate) struct BotActivity {
        pub(crate) name: String,
        #[serde(rename = "type")]
        pub(crate) ty: ActivityType,
        // Stream URL. Is validated when type is Streaming.
        // Both `null` and omission are allowed.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) url: Option<String>,
    }
    #[derive(Serialize)]
    struct Activity {
        name: String,
        #[serde(rename = "type")]
        ty: ActivityType,
        // Stream URL. Is validated when type is Streaming.
        // Both `null` and omission are allowed.
        #[serde(skip_serializing_if = "Option::is_none")]
        url: Option<String>,
        created_at: U64,
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        timestamps: Option<ActivityTimestamps>,
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        application_id: Option<Snowflake>,
        // Both `null` and omission are allowed.
        #[serde(skip_serializing_if = "Option::is_none")]
        details: Option<String>,
        // Both `null` and omission are allowed.
        #[serde(skip_serializing_if = "Option::is_none")]
        state: Option<String>,
        // Both `null` and omission are allowed.
        #[serde(skip_serializing_if = "Option::is_none")]
        emoji: Option<ActivityEmoji>,
        // TODO: Incomplete.
    }
    #[derive(Serialize)]
    pub(crate) struct UpdateStatus {
        pub(crate) since: Option<u32>,
        // `null` is allowed, omission is not.
        // TODO: Since we only care about sending these for the moment,
        // we're using the BotActivity type, but I'd prefer to
        // abstract over the two at some point.
        pub(crate) activities: Option<Vec<BotActivity>>,
        pub(crate) status: StatusType,
        pub(crate) afk: bool,
    }
    /// [Sharding](https://discord.com/developers/docs/topics/gateway#sharding)
    #[derive(Serialize, Deserialize)]
    #[serde(transparent)]
    pub(crate) struct ShardData {
        arr: [u32; 2],
    }
    /// [Resume](https://discord.com/developers/docs/topics/gateway#resume)
    #[derive(Serialize)]
    pub(crate) struct ResumeData {
        // This is an auth token.
        // Perhaps it should be typed that way.
        pub(crate) token: String,
        pub(crate) session_id: SessionId,
        #[serde(rename = "seq")]
        pub(crate) sequence_number: SequenceNumber,
    }
    #[derive(Serialize)]
    pub(crate) struct IdentifyData {
        // This is an auth token.
        // Perhaps it should be typed that way.
        pub(crate) token: String,
        pub(crate) properties: ConnectionProperties,
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) compress: Option<bool>,
        // Between 50 and 250, total number of members
        // where the gateway will stop sending offline members
        // in the guild member list.
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) large_threshold: Option<u8>,
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) shard: Option<ShardData>,
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) presence: Option<UpdateStatus>,
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) guild_subscriptions: Option<bool>,
        pub(crate) intents: Intents,
    }
    #[derive(Deserialize_repr, Debug)]
    #[repr(u8)]
    pub(crate) enum InteractionType {
        Ping = 1,
        ApplicationCommand = 2,
    }
    #[derive(Deserialize, Debug)]
    pub(crate) struct ApplicationCommandInteractionData {
        pub(crate) id: Snowflake,
        pub(crate) name: String,
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) options: Option<Vec<ApplicationCommandInteractionDataOption>>,
    }
    #[derive(Deserialize, Debug)]
    pub(crate) struct ApplicationCommandInteractionDataOption {
        pub(crate) name: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) value: Option<::serde_json::Value>,
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) options: Option<Vec<ApplicationCommandInteractionDataOption>>,
    }
    #[derive(Deserialize, Debug)]
    pub(crate) struct User {
        pub(crate) id: Snowflake,
        pub(crate) username: String,
        // This is four digits long.
        // Perhaps I should just parse this to an integer.
        pub(crate) discriminator: String,
        // `null` is allowed, omission is not.
        pub(crate) avatar: Option<String>,
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) bot: Option<bool>,
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) system: Option<bool>,
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) mfa_enabled: Option<bool>,
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) locale: Option<String>,
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) verified: Option<bool>,
        // Both `null` and omission are allowed.
        pub(crate) email: Option<String>,
        // TODO: type these flags
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) flags: Option<U64>,
        // TODO: type this
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) premium_type: Option<U64>,
        // TODO: type this
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) public_flags: Option<U64>,
    }
    #[derive(Deserialize, Debug)]
    pub(crate) struct GuildMember {
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) user: Option<User>,
        // `null` is allowed, omission is not.
        pub(crate) nick: Option<String>,
        pub(crate) roles: Vec<Snowflake>,
        // TODO: this is an ISO8601 timestamp, and should be typed as such
        pub(crate) joined_at: String,
        // TODO: this is also an ISO8601 timestamp,
        // and should be typed as such as well
        // Both `null` and omission are allowed.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) premium_since: Option<String>,
        pub(crate) deaf: bool,
        pub(crate) mute: bool,
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) pending: Option<bool>,
    }
    #[derive(Deserialize, Debug)]
    pub(crate) struct Interaction {
        pub(crate) id: Snowflake,
        #[serde(rename = "type")]
        pub(crate) ty: InteractionType,
        // This is always present on ApplicationCommand Interaction types.
        // `null` is not allowed, omission is.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub(crate) data: Option<ApplicationCommandInteractionData>,
        pub(crate) guild_id: Snowflake,
        pub(crate) channel_id: Snowflake,
        pub(crate) member: GuildMember,
        pub(crate) token: String,
        // Always `1`.
        pub(crate) version: u8,
    }
    // TODO: examine each use of Snowflake and
    // see if it should be more strongly typed
    // TODO: Look into borrowing more strings, possibly using Cow<'a, str>.
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
enum ChoiceValue {
    String(String),
    Int(u64),
}

#[derive(Debug, Serialize, Deserialize)]
struct Choice {
    name: String,
    value: ChoiceValue,
}

#[derive(Debug, Serialize_repr, Deserialize)]
#[repr(u8)]
enum CommandOptionType {
    SubCommand = 1,
    SubCommandGroup = 2,
    String = 3,
    Integer = 4,
    Boolean = 5,
    User = 6,
    Channel = 7,
    Role = 8,
}

#[derive(Debug, Serialize, Deserialize)]
struct CommandOption {
    #[serde(rename = "type")]
    ty: CommandOptionType,
    name: String,
    description: String,
    default: Option<bool>,
    // Might need to customize how this is serialized.
    required: Option<bool>,
    choices: Option<Vec<Choice>>,
    options: Option<Vec<CommandOption>>,
}

#[derive(Debug, Serialize, Deserialize)]
struct SlashSchema {
    name: String,
    description: String,
    options: Option<Vec<CommandOption>>,
}

/// Discord Guild ID
#[derive(Debug, StructOpt, Serialize)]
#[serde(transparent)]
struct GuildId {
    id: u64,
}

/// Discord Application ID
#[derive(Debug, Serialize, Deserialize, Copy, Clone)]
#[serde(transparent)]
struct ApplicationId {
    id: u64,
}

#[derive(Debug, Deserialize)]
enum Auth {
    BotToken(String),
    BearerToken(String),
}

#[derive(Debug, Deserialize)]
struct AppConfig {
    id: ApplicationId,
    auth: Auth,
}

// This is a strict superset of `AppConfig`.
// Ideally, I should enforce this with a macro or something.
#[derive(Debug, Deserialize)]
#[serde(rename = "AppConfig")]
struct ServerAppConfig {
    id: ApplicationId,
    auth: Auth,
    cache: PathBuf,
    // While this may not be the final form
    // this field or sharding configuration takes,
    // there will always be a "we don't do sharding" value.
    // That is the first state we're going to support.
    sharded: bool,
}

#[derive(Debug, Serialize, Deserialize)]
struct ServerAppCache {
    #[serde(default, with = "url_opt")]
    gateway_url: Option<Url>,
}
#[derive(Debug, Error)]
enum CacheLoadError {
    #[error("{0}")]
    InvalidData(#[from] ::ron::de::Error),
    #[error("{0}")]
    Io(#[from] io::Error),
}
impl ServerAppCache {
    fn load(path: &Path) -> Result<Self, CacheLoadError> {
        let file = File::open(path);
        match file {
            Ok(file) => ::ron::de::from_reader(BufReader::new(file)).map_err(|e| e.into()),
            Err(e) => match e.kind() {
                io::ErrorKind::NotFound => Ok(Self { gateway_url: None }),
                _ => Err(e.into()),
            },
        }
    }
    fn save(&self, path: &Path) -> Result<(), io::Error> {
        Ok(
            ::ron::ser::to_writer(BufWriter::new(File::create(path)?), self)
                .expect("I don't understand how ron can fail at serialization"),
        )
    }
    async fn get_gateway_url(
        &mut self,
        client: &::reqwest::Client,
    ) -> Result<Url, ::reqwest::Error> {
        match self.gateway_url {
            Some(ref x) => Ok(x.clone()),
            None => match api::get_gateway(client).await {
                Ok(x) => {
                    self.gateway_url = Some(x);
                    match self.gateway_url {
                        Some(ref x) => Ok(x.clone()),
                        None => unreachable!("this is literally impossible"),
                    }
                }
                Err(e) => Err(e),
            },
        }
    }
}

#[derive(Debug, StructOpt)]
enum Kind {
    /// Adds a guild-level slash command.
    Guild(GuildId),
    /// Adds a global slash command.
    Global,
}

enum Slash {
    Guild {
        schema: SlashSchema,
        guild_id: GuildId,
    },
    Global {
        schema: SlashSchema,
    },
}

#[derive(Debug, StructOpt)]
enum SchemaKind {
    /// Validates a Slash Command schema.
    Slash { path: PathBuf },
    /// Validates an Application schema.
    App { app_path: PathBuf },
    /// Validates a server Application schema.
    Server { app_path: PathBuf },
}

/// Doing stuff with slash commands.
#[derive(Debug, StructOpt)]
enum Opt {
    /// Registers or updates a slash command.
    Register {
        /// Path to a command schema.
        path: PathBuf,
        /// Path to app configuration.
        app_path: PathBuf,
        #[structopt(flatten)]
        kind: Kind,
    },
    /// Runs Discord Gateway connected server
    /// for receiving Interactions.
    // We'll get to handling later, lol.
    Server {
        /// Path to app configuration.
        app_path: PathBuf,
    },
    /// Validate a schema.
    Validate(SchemaKind),
}

mod heartbeat {
    use super::gateway::{self, SequenceNumber};
    use ::tokio::sync::mpsc::error::TryRecvError;
    use ::tokio::sync::{mpsc, oneshot, watch};
    use ::tokio::time;
    struct Heart {
        receiver: mpsc::Receiver<HeartMessage>,
        seq_rx: watch::Receiver<Option<SequenceNumber>>,
        interval: u64,
        ws_sender: mpsc::Sender<::tungstenite::Message>,
    }
    #[derive(Debug)]
    pub(crate) enum HeartMessage {
        Ack,
    }
    impl Heart {
        fn new(
            receiver: mpsc::Receiver<HeartMessage>,
            seq_rx: watch::Receiver<Option<SequenceNumber>>,
            interval: u64,
            ws_sender: mpsc::Sender<::tungstenite::Message>,
        ) -> Self {
            Self {
                receiver,
                seq_rx,
                interval,
                ws_sender,
            }
        }
        async fn run(mut self) {
            let mut interval = time::interval(time::Duration::from_millis(self.interval));
            loop {
                interval.tick().await;
                let seq = *self.seq_rx.borrow();
                match self.receiver.try_recv() {
                    Ok(HeartMessage::Ack) => {
                        // TODO: consider what would happen if this send took
                        // longer than the heartbeat interval.
                        let send_result = self
                            .ws_sender
                            .send(::tungstenite::Message::text(
                                ::serde_json::to_string(&gateway::Payload {
                                    opcode: gateway::Opcode::Heartbeat,
                                    data: seq,
                                    event_name: None,
                                    sequence_number: None,
                                })
                                .expect("couldn't serialize heartbeat payload"),
                            ))
                            .await;
                        match send_result {
                            Ok(()) => (),
                            // If sending returns an Err, that means the receiver is closed.
                            // If the receiver is closed, that means we're winding down.
                            // So, just exit the heartbeat loop.
                            Err(_) => break,
                        }
                    }
                    // This is the case where we haven't received any HeartbeatACKs since
                    // our last heartbeat, unless it's the first run?
                    // Consider sending a special initial heartbeat before entering the loop.
                    // TODO: terminate the connection with a non-1000 close code, reconnect,
                    // and attempt to resume.
                    Err(TryRecvError::Empty) => todo!("attempt reconnection"),
                    // If the sender is closed, that means we're winding down.
                    // As above, panicking is silly- just stop sending heartbeat messages.
                    Err(TryRecvError::Closed) => break,
                }
            }
        }
    }

    pub(crate) struct HeartHandle {
        sender: mpsc::Sender<HeartMessage>,
        seq_sender: watch::Sender<Option<SequenceNumber>>,
    }
    impl HeartHandle {
        pub(crate) fn new(interval: u64, ws_sender: mpsc::Sender<::tungstenite::Message>) -> Self {
            // This capacity is connected to the TODO above.
            let (sender, receiver) = mpsc::channel(1);
            // TODO: ascertain that this initial ACK is necessary
            sender
                .try_send(HeartMessage::Ack)
                .expect("couldn't send initial ACK");
            let (seq_sender, seq_receiver) = watch::channel(None);
            let beater = Heart::new(receiver, seq_receiver, interval, ws_sender);
            ::tokio::spawn(beater.run());
            Self { sender, seq_sender }
        }
        pub(crate) async fn send(
            &self,
            msg: HeartMessage,
        ) -> Result<(), mpsc::error::SendError<HeartMessage>> {
            self.sender.send(msg).await
        }
        pub(crate) fn set_seq(&self, seq: SequenceNumber) {
            let _ = self.seq_sender.send(Some(seq));
        }
    }
}

mod connection {
    use super::gateway::{self, SequenceNumber, SessionId};
    use super::heartbeat::{HeartHandle, HeartMessage};
    use super::Auth;
    /// TLS secured WebSocketStream we use with the Discord Gateway.
    type WSStream = ::tokio_tungstenite::WebSocketStream<::tokio_rustls::client::TlsStream<::tokio::net::TcpStream>>;
    use ::tokio_tungstenite::WebSocketStream;
    use ::tungstenite::{Message, protocol::{CloseFrame, frame::coding::CloseCode}};
    use ::tokio::sync::{oneshot, mpsc};
    // Handles IO with a single Gateway connection.
    // Does not perform reconnections- that's someone else's job.
    // *Does* return errors indicative of whether reconnects are advisable.
    struct Connection {
        stream: WSStream,
        heartbeat_interval: u32,
        // Since this field is initialized by the `Ready` event,
        // which is required for a session to be considered initialized,
        // this is guaranteed to be present.
        sequence_number: SequenceNumber,
    }
    enum Never {}
    // TODO: flesh this out
    enum ConnectionClosed {
        Resumable(Option<SequenceNumber>),
        Nonresumable,
    }
    impl Connection {
        /// Initializes a Gateway connection from a just created WebSocket stream,
        /// issuing a Resume event instead of an Identify event.
        async fn with_resume(auth: &Auth, seq: SequenceNumber, session_id: SessionId, mut stream: WSStream) -> Result<Self, ()> {
            use ::futures::SinkExt;
            use ::tokio::stream::StreamExt;
            match stream.next().await {
                // TODO: reduce duplication between this and `initialize`
                Some(Ok(Message::Text(first_msg))) => match ::serde_json::from_str(&first_msg) {
                    Ok(gateway::Payload {
                        opcode: gateway::Opcode::Hello,
                        data: gateway::HelloData { heartbeat_interval },
                        ..
                    }) => {
                        stream.send(Message::Text(::serde_json::to_string(&gateway::Payload {
                            opcode: gateway::Opcode::Resume,
                            data: gateway::ResumeData {
                                token: match auth {
                                    Auth::BotToken(x) => x.clone(),
                                    Auth::BearerToken(_) => todo!("figure out if token kind is relevant"),
                                },
                                sequence_number: seq,
                                session_id,
                            },
                            event_name: None,
                            sequence_number: None,
                        }).expect("couldn't serialize Resume payload"))).await.map_err(|_| ())?;
                        Ok(Self { sequence_number: seq, stream, heartbeat_interval })
                    },
                    Ok(_) | Err(_) => Err(()),
                },
                // Initialization failure:
                // Invalid first message
                Some(Ok(_)) | Some(Err(_)) => Err(()),
                // Initialization failure:
                // Connection closed before we could read a single message.
                None => Err(()),
            }
        }
        /// Initializes a Gateway connection from a just created WebSocket stream.
        async fn initialize(auth: &Auth, mut stream: WSStream) -> Result<(Self, SessionId), ()> {
            use ::futures::SinkExt;
            use ::tokio::stream::StreamExt;
            // TODO: more precise handling of invalid first messages
            match stream.next().await {
                Some(Ok(Message::Text(first_msg))) => match ::serde_json::from_str(&first_msg) {
                    Ok(gateway::Payload {
                        opcode: gateway::Opcode::Hello,
                        data: gateway::HelloData { heartbeat_interval },
                        ..
                    }) => {
                        stream.send(Message::Text(::serde_json::to_string(&gateway::Payload {
                            opcode: gateway::Opcode::Identify,
                            data: gateway::IdentifyData {
                                token: match auth {
                                    Auth::BotToken(x) => x.clone(),
                                    Auth::BearerToken(_) => todo!("figure out if token kind is relevant in Identify payloads"),
                                },
                                properties: gateway::ConnectionProperties {
                                    // TODO: perform OS detection
                                    os: "Linux".to_string(),
                                    // TODO: consider renaming our gateway library at some point
                                    browser: "mbot-slash-gateway".to_string(),
                                    device: "mbot-slash-gateway".to_string(),
                                },
                                compress: Some(false),
                                large_threshold: None,
                                shard: None,
                                presence: None,
                                guild_subscriptions: None,
                                intents: gateway::Intents::empty(),
                            },
                            event_name: None,
                            sequence_number: None,
                            // TODO: consider distinguishing between Identify send failure and
                            // failure to read the first message.
                        }).expect("couldn't serialize Identify payload"))).await.map_err(|_| ())?;
                        // TODO: consider handling the case where the time Discord takes to send us
                        // the Ready event is longer than the heartbeat interval
                        let (ready, sequence_number) = Self::take_ready(&mut stream).await?;
                        Ok((Self { sequence_number, stream, heartbeat_interval },
                            ready.session_id))
                    },
                    // Initialization failure:
                    // Invalid first message
                    Ok(_) => Err(()),
                    Err(_) => Err(()),
                }
                // Initialization failure:
                // Invalid first message
                Some(Ok(_)) | Some(Err(_)) => Err(()),
                // Initialization failure:
                // Connection closed before we could read a single message.
                None => Err(()),
            }
        }
        /// Internal function, used by `Connection::initialize`.
        /// Consumes the next message from the given stream under the assumption
        /// that it will be the `Ready` event.
        async fn take_ready(stream: &mut WSStream) -> Result<(gateway::Ready, SequenceNumber), ()> {
            use ::tokio::stream::StreamExt;
            match stream.next().await {
                Some(Ok(Message::Text(text))) => match ::serde_json::from_str(&text) {
                    Ok(gateway::Payload {
                        opcode: gateway::Opcode::Dispatch,
                        event_name: Some(event_name),
                        sequence_number: Some(sequence_number),
                        data: ready @ gateway::Ready { .. },
                    }) if event_name == "READY" => Ok((ready, sequence_number)),
                    Ok(_) | Err(_) => Err(()),
                },
                Some(Ok(_)) | Some(Err(_)) => Err(()),
                None => Err(()),
            }
        }
        /// Runs event loop and concurrently sends appropriate heartbeat messages.
        async fn run_event_loop(mut self, exit: oneshot::Sender<ConnectionClosed>,
                                events: mpsc::UnboundedSender<GatewayEvent>) {
            use ::tokio::stream::StreamExt;
            use ::futures::sink::SinkExt;
            let (h_ws_tx, mut h_ws_rx) = ::tokio::sync::mpsc::channel(1);
            let heart_handle = HeartHandle::new(self.heartbeat_interval as _, h_ws_tx);
            // The latest sequence number received from a dispatch.
            // TODO: examine how this gets initialized when resuming a session
            let mut sequence_number = Some(self.sequence_number);
            // Wrapper closure to ensure we always set both at the same time.
            let mut set_seq = |seq| {
                sequence_number = Some(seq);
                heart_handle.set_seq(seq);
            };
            loop {
                ::tokio::select! {
                    msg = h_ws_rx.recv() => match msg {
                        Some(msg) => self.stream.send(msg).await.expect("couldn't send heartbeat"),
                        None => todo!("handle heart exiting early"),
                    },
                    // If the receiving half is closed, we've
                    // either been intentionally stopped by the ConnectionHandle,
                    // or the ConnectionHandle has been dropped.
                    // In either case, this task should wind down immediately.
                    _ = events.closed() => break,
                    msg = self.stream.next() => match msg {
                        Some(Ok(Message::Text(msg))) => {
                            println!("Another message: {:?}", msg);
                            // TODO: Consider factoring out event decoding somehow.
                            let msg: gateway::Payload<::serde_json::Value> = ::serde_json::from_str(&msg)
                                .expect("discord sent invalid message");
                            // TODO: consider matching on the whole structure of the message,
                            // for improved validity in the face of updates to the Discord API.
                            match msg.opcode {
                                gateway::Opcode::Dispatch => {
                                    let event_name = &*msg.event_name
                                        .expect("event dispatches always have event names");
                                    let event_seq = msg.sequence_number
                                        .expect("events always have sequence numbers");
                                    println!("--------------------");
                                    println!("|     Dispatch     |");
                                    println!("--------------------");
                                    println!("Event Name: {}", event_name);
                                    println!("Sequence Number: {:?}", msg.sequence_number);
                                    println!("--------------------");
                                    set_seq(event_seq);
                                    match event_name {
                                        "INTERACTION_CREATE" => {
                                            let interaction: gateway::Interaction =
                                                ::serde_json::from_value(msg.data)
                                                .expect("discord sent invalid INTERACTION_CREATE payload");
                                            match events.send(
                                                GatewayEvent::DispatchInteractionCreate(interaction)
                                            ) {
                                                Ok(()) => (),
                                                // Sending on an unbounded mpsc sender will only fail
                                                // if the receiving half is closed.
                                                // If the receiving half is closed, that means
                                                // either we've been intentionally stopped by
                                                // the ConnectionHandle, or the ConnectionHandle
                                                // has been dropped.
                                                // In either case, this task should wind down.
                                                Err(_) => {
                                                    eprintln!("dropped gateway message due to internal event channel being closed while parsing message");
                                                    break
                                                },
                                            }
                                        },
                                        name => eprintln!("unknown event name: {}", name),
                                    }
                                }
                                gateway::Opcode::HeartbeatACK => {
                                    match heart_handle.send(HeartMessage::Ack).await {
                                        Ok(()) => (),
                                        Err(_) => todo!("handle heart exiting early"),
                                    }
                                }
                                gateway::Opcode::Reconnect => {
                                    eprintln!("Received Reconnect. Ending current WebSocket connection.");
                                    // Whether we succeed to send this or not,
                                    // this task must wind down now.
                                    match exit.send(ConnectionClosed::Resumable(sequence_number)) {
                                        Ok(()) => break,
                                        Err(_) => break,
                                    }
                                },
                                code => eprintln!("unhandled payload type: {:?}", code),
                            }
                        },
                        Some(Ok(Message::Close(Some(CloseFrame {
                            code: CloseCode::Away,
                            reason,
                        })))) => {
                            eprintln!("Close reason: {}", reason);
                            // Whether we succeed to send this or not,
                            // this task must wind down now.
                            match exit.send(ConnectionClosed::Nonresumable) {
                                Ok(()) => break,
                                Err(_) => break,
                            }
                        }
                        Some(Ok(msg)) => todo!("handle other kinds of WebSocket message: {:?}", msg),
                        Some(Err(e)) => todo!("handle WebSocket error: {:?}", e),
                        None => todo!("handle closed gateway stream"),
                    }
                }
            }
        }
    }
    // I will hopefully merge this with the deserialization
    // code in the gateway module at some point.
    // For now, it's manual, though.
    //
    // I don't feel like exposing sequence numbers unless we have to.
    #[non_exhaustive]
    pub(crate) enum GatewayEvent {
        // We'll be omitting heartbeats from this, for now.
        DispatchInteractionCreate(gateway::Interaction),
    }
    /// A session resume token.
    /// Contains whatever is necessary to resume a Gateway session.
    pub(crate) struct SessionResume {
        seq: SequenceNumber,
    }
    // TODO: consider renaming this to `SessionHandle`
    pub(crate) struct ConnectionHandle<'a> {
        // TODO: figure out what needs to be held to talk back 'n shit
        exit: Option<oneshot::Receiver<ConnectionClosed>>,
        events: mpsc::UnboundedReceiver<GatewayEvent>,
        // Backup clone of the sender half of the event channel,
        // so we can continue using the same channel for each connection.
        event_sender: mpsc::UnboundedSender<GatewayEvent>,
        auth: &'a Auth,
        // I'd abbreviate to `resume`, but I'd rather not have
        // a field and method with the same name.
        // We use this to keep the SessionResume on hand
        // because we can't recv from a oneshot channel more than once.
        resume_token: Option<SessionResume>,
        session_id: SessionId,
    }
    impl<'a> ConnectionHandle<'a> {
        pub(crate) async fn new(auth: &'a Auth, stream: WSStream) -> Result<ConnectionHandle<'a>, ()> {
            let (connection, session_id) = Connection::initialize(auth, stream).await?;
            let (exit_tx, exit_rx) = oneshot::channel();
            let (event_tx, event_rx) = mpsc::unbounded_channel();
            let extra_event_tx = event_tx.clone();
            ::tokio::spawn(connection.run_event_loop(exit_tx, event_tx));
            Ok(Self { exit: Some(exit_rx),
                      events: event_rx,
                      event_sender: extra_event_tx,
                      resume_token: None,
                      auth, session_id
            })
        }
        /// Attempt to resume Gateway session with new WebSocket stream.
        pub(crate) async fn resume(&mut self, resume_token: SessionResume, stream: WSStream) -> Result<(), ()> {
            let connection = Connection::with_resume(self.auth, resume_token.seq, self.session_id.clone(), stream).await?;
            let (exit_tx, exit_rx) = oneshot::channel();
            ::tokio::spawn(connection.run_event_loop(exit_tx, self.event_sender.clone()));
            self.exit = Some(exit_rx);
            Ok(())
        }
        // TODO: consider making this return a Result that's more descriptive
        // of why no elements are being yielded
        // TODO: consider yielding events even when gateway connection is closed
        // The reason we don't currently is just that we want to give the user
        // a chance to terminate their event processing loop and do reconnection.
        pub(crate) async fn next(&mut self) -> Option<GatewayEvent> {
            use ::tokio::stream::StreamExt;
            // If we continue using the same mpsc channel, we can simply use the latest
            // sequence number Discord's given us, instead of trying
            // to resume from the latest processed event.
            // This strategy is lighter on IO and requires less bookkeeping.
            match self.exit {
                Some(ref mut channel) => match channel.try_recv() {
                    Ok(closed) => {
                        match closed {
                            ConnectionClosed::Resumable(Some(seq)) =>
                                self.resume_token = Some(SessionResume {
                                    seq
                                }),
                            _ => ()
                        }
                        self.exit = None;
                        None
                    },
                    // Only yield events when the connection is open,
                    // even if we have some we haven't processed yet.
                    Err(oneshot::error::TryRecvError::Empty) => {
                        ::tokio::select! {
                            event = self.events.next() => event,
                            closed = channel => {
                                match closed {
                                    Ok(ConnectionClosed::Resumable(Some(seq))) =>
                                        self.resume_token = Some(SessionResume { seq }),
                                    _ => ()
                                }
                                self.exit = None;
                                None
                            },
                        }
                    },
                    Err(oneshot::error::TryRecvError::Closed) => None,
                },
                None => None,
            }
        }
        pub(crate) fn take_resume(&mut self) -> Option<SessionResume> {
            match self.exit {
                Some(ref mut exit) => match exit.try_recv() {
                    Ok(closed) => {
                        self.exit = None;
                        match closed {
                            ConnectionClosed::Resumable(Some(seq)) => Some(SessionResume { seq }),
                            // I doubt this one is going to occur, but it's technically possible.
                            ConnectionClosed::Resumable(None) => None,
                            ConnectionClosed::Nonresumable => None,
                        }
                    }
                    Err(_) => None,
                },
                None => match self.resume_token.take() {
                    Some(token) => Some(token),
                    None => None,
                }
            }
        }
    }
    fn get_stream() -> WSStream {
        todo!("lol")
    }
    fn get_auth() -> Auth {
        todo!(":upside_down:")
    }
    async fn example() {
        let ws_stream = get_stream();
        let auth = get_auth();
        let mut connection = ConnectionHandle::new(&auth, ws_stream).await.unwrap();
        loop {
            // use it
            while let Some(event) = connection.next().await {
                match event {
                    GatewayEvent::DispatchInteractionCreate { .. } => (),
                }
            }
            // handle it dying:
            // note that we'll likely have a retry loop or something here
            eprintln!("Disconnected. Attempted to reconnect...");
            match connection.take_resume() {
                Some(token) => connection.resume(token, get_stream()).await.unwrap(),
                None => match ConnectionHandle::new(&auth, get_stream()).await {
                    Ok(x) => connection = x,
                    Err(_) => break
                }
            }
        }
    }
}

// TODO: split apart this gigant main function.
#[::tokio::main]
async fn main() {
    let opt = Opt::from_args();
    match opt {
        Opt::Register {
            path,
            kind,
            app_path,
        } => {
            let command_input =
                BufReader::new(File::open(path).expect("couldn't open schema file"));
            let app_input = BufReader::new(File::open(app_path).expect("couldn't open app file"));
            let schema: SlashSchema =
                ::ron::de::from_reader(command_input).expect("couldn't deserialize command schema");
            let app: AppConfig =
                ::ron::de::from_reader(app_input).expect("couldn't deserialize app configuration");
            println!("Schema: {:#?}", schema);
            println!(
                "JSON Schema: {}",
                serde_json::to_string_pretty(&schema).unwrap()
            );
            let client = ::reqwest::Client::new();
            match kind {
                Kind::Guild(guild_id) => {
                    let result = api::upsert_slash(
                        &client,
                        app.id,
                        &app.auth,
                        &Slash::Guild { guild_id, schema },
                    )
                    .compat()
                    .await;
                    println!("Result: {:?}", result);
                }
                Kind::Global => {
                    let result =
                        api::upsert_slash(&client, app.id, &app.auth, &Slash::Global { schema })
                            .compat()
                            .await;
                    println!("Result: {:?}", result);
                }
            }
        }
        Opt::Server { app_path } => {
            let app_input = BufReader::new(File::open(&app_path).expect("couldn't open app file"));
            // Note: Consider adding a ServerAppConfig that's a superset of the fields of
            // AppConfig, in the case that the server needs more configuration.
            let config: ServerAppConfig =
                ::ron::de::from_reader(app_input).expect("couldn't deserialize app configuration");
            let mut cache =
                ServerAppCache::load(&config.cache).expect("couldn't load server cache");
            let req_client = ::reqwest::Client::new();
            // Load stuff we need from cache before we do anything else.
            let gateway_url = cache
                .get_gateway_url(&req_client)
                .compat()
                .await
                .expect("couldn't get gateway URI");
            cache.save(&config.cache).expect("couldn't save our cache"); // Might as well save our cache.
            let domain = gateway_url
                .domain()
                .expect("gateway URI needs a domain component");
            let mut wss_request = gateway_url.clone();
            wss_request
                .query_pairs_mut()
                .clear()
                .append_pair("v", &format!("{}", gateway::VERSION))
                .append_pair("encoding", "json");
            // Done preparing from cache.

            use ::std::net::ToSocketAddrs;
            use ::std::sync::Arc;
            use ::tokio::net::TcpStream;
            use ::tokio_rustls::{rustls::ClientConfig, TlsConnector};
            use ::webpki::DNSNameRef;
            let mut client_config = ClientConfig::new();
            client_config
                .root_store
                .add_server_trust_anchors(&::webpki_roots::TLS_SERVER_ROOTS);
            let connector = TlsConnector::from(Arc::new(client_config));
            let dnsname = DNSNameRef::try_from_ascii_str(domain).unwrap();
            let addr = (domain, gateway::PORT)
                .to_socket_addrs()
                .unwrap()
                .next()
                .expect("Discord host not found");
            let get_stream = || async {
                let stream = TcpStream::connect(addr)
                    .await
                    .expect("couldn't open TCP connection with Discord");
                let stream = connector
                    .connect(dnsname, stream)
                    .await
                    .expect("couldn't open TLS connection");
                let (ws_stream, _) = ::tokio_tungstenite::client_async(&wss_request, stream)
                    .await
                    .expect("couldn't open WebSocket stream");
                ws_stream
            };
            let ws_stream = get_stream().await;
            use connection::{ConnectionHandle, GatewayEvent};
            let mut connection = ConnectionHandle::new(&config.auth, ws_stream).await.unwrap();
            loop {
                while let Some(event) = connection.next().await {
                    match event {
                        GatewayEvent::DispatchInteractionCreate(interaction) => {
                            let data = interaction.data
                                .expect("Currently, this field is guaranteed to be populated");
                            match data.options.as_deref() {
                                Some(
                                    [gateway::ApplicationCommandInteractionDataOption {
                                        name, value: Some(::serde_json::Value::String(exp)), ..
                                    }],
                                ) => {
                                    let name: &str = &*name;
                                    match name {
                                        "expression" => {
                                            let exp =
                                                ::mice::parse::Expression::parse(exp).unwrap().1.unwrap();
                                            api::reply_gateway_interaction(&req_client,
                                                                           interaction.id,
                                                                           interaction.token,
                                                                           exp.roll().unwrap().format(
                                                                               Default::default())
                                            ).compat().await.unwrap();
                                        }
                                        _ => todo!("wrong option"),
                                    }
                                }
                                Some([..]) => todo!("wrong options"),
                                None => todo!("no options"),
                            }
                        },
                    }
                }
                match connection.take_resume() {
                    Some(token) => connection.resume(token, get_stream().await).await.unwrap(),
                    None => match ConnectionHandle::new(&config.auth, get_stream().await).await {
                        Ok(x) => connection = x,
                        Err(_) => break,
                    }
                }
            }
            // while let Some(msg) = ws_stream.next().await {
            //     println!("Another message: {:?}", msg);
            // }
        }
        Opt::Validate(SchemaKind::Slash { path }) => {
            let slash_input = BufReader::new(File::open(path).expect("couldn't open schema file"));
            match ::ron::de::from_reader(slash_input) {
                Ok(SlashSchema { .. }) => {
                    println!("Valid schema!");
                    ::std::process::exit(0);
                }
                Err(e) => {
                    eprintln!("Invalid schema: {:?}", e);
                    ::std::process::exit(1);
                }
            }
        }
        Opt::Validate(SchemaKind::App { app_path }) => {
            let app_input =
                BufReader::new(File::open(app_path).expect("couldn't open schema file"));
            match ::ron::de::from_reader(app_input) {
                Ok(AppConfig { .. }) => {
                    println!("Valid schema!");
                    ::std::process::exit(0);
                }
                Err(e) => {
                    eprintln!("Invalid schema: {:?}", e);
                    ::std::process::exit(1);
                }
            }
        }
        Opt::Validate(SchemaKind::Server { app_path }) => {
            let server_input =
                BufReader::new(File::open(app_path).expect("couldn't open schema file"));
            match ::ron::de::from_reader(server_input) {
                Ok(ServerAppConfig { sharded: false, .. }) => {
                    println!("Valid schema!");
                    ::std::process::exit(0);
                }
                Ok(ServerAppConfig { sharded: true, .. }) => {
                    eprintln!("Sharding isn't supported yet.");
                    ::std::process::exit(2);
                }
                Err(e) => {
                    eprintln!("Invalid schema: {:?}", e);
                    ::std::process::exit(1);
                }
            }
        }
    }
}
