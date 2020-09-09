use ::std::io::Write;
use serde::Deserialize;

/// A Masks move.
#[derive(Deserialize, Debug)]
struct Move {
    /// I'm not sure what this field is for.
    id: u64,
    /// Command name for a move.
    #[serde(rename = "shortName")]
    short_name: String,
    /// All capitalized letters short description of a move.
    capital: String,
    /// For printing a message of the form,
    /// > "PLAYER directly engages a threat!"
    /// This is the part after PLAYER.
    phrase: String,
    /// Full text description of a move.
    #[serde(rename = "blob")]
    description: String,
    /// The label a move uses.
    label: String,
    /// Image URI.
    #[serde(rename = "img")]
    image: String,
    /// The playbook the move comes from.
    /// "basic" means it's from the basic set of moves.
    playbook: String,
}

/// Top level of Maddie data JSON.
#[derive(Deserialize, Debug)]
struct MaddieData {
    moves: Vec<Move>,
}

fn move_command(m: &Move) -> proc_macro2::TokenStream {
    let short_name = &m.short_name;
    let cmd_ident = quote::format_ident!("{}", short_name);
    quote::quote! {
        #[command]
        async fn #cmd_ident(ctx: &Context, msg: &Message) -> CommandResult {
            reply(ctx, msg, #short_name).await
        }
    }
}

fn command_group(name: &str, commands: Vec<&str>) -> proc_macro2::TokenStream {
    use ::quote::TokenStreamExt;
    let mut idents = proc_macro2::TokenStream::new();
    idents.append_separated(commands.iter().map(|x| quote::format_ident!("{}", x)), quote::quote!(,));
    let group_ident = quote::format_ident!("{}", name);
    quote::quote! {
        #[group]
        #[commands(#idents)]
        struct #group_ident;
    }
}

fn main() -> anyhow::Result<()> {
    println!("cargo:rerun-if-changed=data/maddie.json");
    let opath = ::std::path::PathBuf::from(::std::env::var("OUT_DIR")?).join("maddie.rs");
    let data: MaddieData = ::serde_json::from_reader(::std::fs::File::open("data/maddie.json")?)?;
    let mut maddie = ::std::fs::File::create(opath)?;
    let mut commands: Vec<&str> = Vec::new();
    for mv in data.moves.iter() {
        write!(maddie, "{}\n", move_command(mv))?;
        commands.push(&mv.short_name);
    }
    write!(maddie, "{}", command_group("MaddieTools", commands))?;
    Ok(())
}
