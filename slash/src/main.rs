//! Supporting Slash Commands for `mbot`.
use ::serde::{Deserialize, Serialize};
use ::serde_repr::Serialize_repr;
use ::std::fs::File;
use ::std::io::BufReader;
use ::std::path::{Path, PathBuf};
use ::structopt::StructOpt;

mod api {
    /// Discord API base URI.
    pub(crate) const BASE: &str = "https://discord.com/api";
    /// Discord API version against which this is written.
    // I'm not going to attempt to support older versions of the Discord API,
    // since nobody but Discord provides it.
    pub(crate) const API_VERSION: u8 = 8;
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
struct GuildId { id: u64 }

#[derive(Debug, StructOpt)]
enum Kind {
    /// Adds a guild-level slash command.
    Guild(GuildId),
    /// Adds a global slash command.
    Global,
}

/// Doing stuff with slash commands.
#[derive(Debug, StructOpt)]
enum Opt {
    Register {
        /// Path to a command schema.
        path: PathBuf,
        #[structopt(flatten)]
        kind: Kind,
    },
}

fn main() {
    let opt = Opt::from_args();
    match opt {
        Opt::Register { path, kind } => {
            let input = BufReader::new(File::open(path).expect("couldn't open schema file"));
            let schema: SlashSchema =
                ::ron::de::from_reader(input).expect("couldn't deserialize command schema");
            println!("Schema: {:#?}", schema);
            println!("JSON Schema: {}", serde_json::to_string_pretty(&schema).unwrap());
        }
    }
}
