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
}
mod gateway {
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
    #[derive(Deserialize)]
    pub(crate) struct DispatchData {}
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
            let stream = TcpStream::connect(addr)
                .await
                .expect("couldn't open TCP connection with Discord");
            let stream = connector
                .connect(dnsname, stream)
                .await
                .expect("couldn't open TLS connection");
            let (mut ws_stream, _) = ::tokio_tungstenite::client_async(wss_request, stream)
                .await
                .expect("couldn't open WebSocket stream");
            let (sequence_tx, sequence_rx) =
                ::tokio::sync::watch::channel(None::<gateway::SequenceNumber>);
            // Read Hello first.
            if let Some(first_msg) = ws_stream.next().await {
                use ::tungstenite::error::Error as WsError;
                use ::tungstenite::Message;
                println!("First Message: {:?}", first_msg);
                match first_msg {
                    Ok(Message::Text(text)) => match ::serde_json::from_str(&text) {
                        Ok(gateway::Payload {
                            opcode: gateway::Opcode::Hello,
                            data: gateway::HelloData { heartbeat_interval },
                            ..
                        }) => {
                            // We're might put the sender half in a Mutex or something.
                            // We're going to have a single task reading from the receiver, though.
                            // That would be our event loop.
                            // Gonna be a fun time, handling reconnects.
                            // Probably gonna end up with a bunch of tasks.
                            let (mut ws_sender, mut ws_receiver) =
                                ::futures::stream::StreamExt::split(ws_stream);
                            task::spawn(async move {
                                // Consider doing the heartbeat a little early?
                                let mut interval =
                                    time::interval(Duration::from_millis(heartbeat_interval as _));
                                loop {
                                    interval.tick().await;
                                    // TODO:
                                    // Check to see if we've received a heartbeat ACK.
                                    // If so, send heartbeat.
                                    // Otherwise, terminate the connection with a non-1000 close code,
                                    // reconnect, and attempt to resume.
                                    let seq_number: Option<gateway::SequenceNumber> =
                                        *sequence_rx.borrow();
                                    ws_sender
                                        .send(Message::text(
                                            ::serde_json::to_string(&gateway::Payload {
                                                opcode: gateway::Opcode::Heartbeat,
                                                data: seq_number,
                                                event_name: None,
                                                sequence_number: None,
                                            })
                                            .expect("couldn't serialize heartbeat payload"),
                                        ))
                                        .await.expect("heartbeat send failure");
                                }
                            });
                            while let Some(msg) = ws_receiver.next().await {
                                println!("Another message: {:?}", msg);
                                match msg {
                                    Ok(Message::Text(text)) => {
                                        match ::serde_json::from_str(&text) {
                                            Ok(gateway::Payload {
                                                opcode: gateway::Opcode::Dispatch,
                                                data,
                                                event_name: Some(event_name),
                                                sequence_number: Some(sequence_number),
                                            }) => {
                                                let data: gateway::DispatchData =
                                                    ::serde_json::from_value(data).unwrap();
                                                sequence_tx.send(Some(sequence_number)).unwrap();
                                                println!("--------------------");
                                                println!("|     Dispatch     |");
                                                println!("--------------------");
                                                println!("Event Name: {}", event_name);
                                                println!("Sequence Number: {:?}", sequence_number);
                                                println!("--------------------");
                                            }
                                            Ok(payload) => {
                                                println!("Payload of unknown type: {:?}", payload);
                                            }
                                            Err(_) => todo!("handling malformed Gateway payloads"),
                                        }
                                    }
                                    Ok(_) => {
                                        todo!("handling different forms of WebSocket messages")
                                    }
                                    Err(_) => todo!("handling connection errors"),
                                }
                            }
                        }
                        Ok(_) | Err(_) => todo!("handling invalid first message"),
                    },
                    Err(WsError::ConnectionClosed) => {
                        todo!("handle connection being closed before we can read a single message")
                    }
                    Err(WsError::Io(e)) => match e.kind() {
                        io::ErrorKind::WouldBlock => todo!("what to do with this"),
                        _ => todo!("stream is dead. reconnect or something"),
                    },
                    Err(WsError::AlreadyClosed) => {
                        unreachable!("reading from already closed stream")
                    }
                    _ => todo!("handling other websocket errors"),
                }
            }
            // while let Some(msg) = ws_stream.next().await {
            //     println!("Another message: {:?}", msg);
            // }
            todo!("actually using the WebSocket connection")
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
