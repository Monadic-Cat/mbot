//! Supporting Slash Commands for `mbot`.
use ::serde::{Deserialize, Serialize};
use ::serde_repr::Serialize_repr;
use ::std::fs::File;
use ::std::io::BufReader;
use ::std::path::{Path, PathBuf};
use ::structopt::StructOpt;
// Reqwest still uses Tokio 0.2,
// but tokio-rustls uses Tokio 0.3.
use ::tokio_compat_02::FutureExt;

mod api {
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
}
mod gateway {
    /// The Discord Gateway is versioned separately from the HTTP APIs.
    /// This is the Gateway version against which this is written.
    pub(crate) const VERSION: u8 = 8;
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
    Validate(SchemaKind),
}

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
            let app_input = BufReader::new(File::open(app_path).expect("couldn't open app file"));
            // Note: Consider adding a ServerAppConfig that's a superset of the fields of
            // AppConfig, in the case that the server needs more configuration.
            let config: ServerAppConfig =
                ::ron::de::from_reader(app_input).expect("couldn't deserialize app configuration");

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
            let dnsname = DNSNameRef::try_from_ascii_str("gateway.discord.gg").unwrap();
            let addr = ("gateway.discord.gg", 443)
                .to_socket_addrs()
                .unwrap()
                .next()
                .expect("Discord host not found");
            let stream = TcpStream::connect(addr)
                .await
                .expect("couldn't open TCP connection with Discord");
            let mut stream = connector
                .connect(dnsname, stream)
                .await
                .expect("couldn't open TLS connection");
            let (ws_stream, _) = ::tokio_tungstenite::client_async(
                "wss://gateway.discord.gg/?v=6&encoding=json",
                stream,
            )
            .await
            .expect("couldn't open WebSocket stream");
            todo!("actually using the WebSocket connection")
        },
        Opt::Validate(SchemaKind::Slash { path }) => {
            let slash_input = BufReader::new(File::open(path).expect("couldn't open schema file"));
            match ::ron::de::from_reader(slash_input) {
                Ok(SlashSchema { .. }) => {
                    println!("Valid schema!");
                    ::std::process::exit(0);
                },
                Err(e) => {
                    eprintln!("Invalid schema: {:?}", e);
                    ::std::process::exit(1);
                }
            }
        },
        Opt::Validate(SchemaKind::App { app_path }) => {
            let app_input = BufReader::new(File::open(app_path).expect("couldn't open schema file"));
            match ::ron::de::from_reader(app_input) {
                Ok(AppConfig { .. }) => {
                    println!("Valid schema!");
                    ::std::process::exit(0);
                },
                Err(e) => {
                    eprintln!("Invalid schema: {:?}", e);
                    ::std::process::exit(1);
                },
            }
        },
        Opt::Validate(SchemaKind::Server { app_path }) => {
            let server_input = BufReader::new(File::open(app_path).expect("couldn't open schema file"));
            match ::ron::de::from_reader(server_input) {
                Ok(ServerAppConfig { .. }) => {
                    println!("Valid schema!");
                    ::std::process::exit(0);
                },
                Err(e) => {
                    eprintln!("Invalid schema: {:?}", e);
                    ::std::process::exit(1);
                }
            }
        },
    }
}
