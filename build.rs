use ::std::io::Write;
use ::std::collections::HashMap;
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
    /// Whether or not a Move requires an associated dice roll.
    #[serde(rename = "requiresRolling")]
    needs_roll: bool,
}

/// Top level of Maddie data JSON.
#[derive(Deserialize, Debug)]
struct MaddieData {
    moves: Vec<Move>,
}

fn move_command(m: &Move) -> proc_macro2::TokenStream {
    let short_name = &m.short_name;
    let cmd_ident = quote::format_ident!("{}", short_name);
    let desc = &m.description;
    let phrase = &m.phrase;
    let capital = &m.capital;
    let args = match m.needs_roll {
        true => quote::quote! { , mut args: Args },
        false => quote::quote! {},
    };
    let roll = match m.needs_roll {
        true => quote::quote! {
            let label: i8 = args.single().unwrap_or(0);
            let (r1, r2) = {
                use ::rand::Rng;
                let mut rng = ::rand::thread_rng();
                (rng.gen_range(1, 7), rng.gen_range(1, 7))
            };
            let rstr = format!("Dice **{}** + **{}**, Label **{}**", r1, r2, label);
            let rtot = r1 + r2 + label as i16;
        },
        false => quote::quote! {},
    };
    let dice = match m.needs_roll {
        true => quote::quote! {.field("Calculation", rstr, false).field("Result", rtot, false)},
        false => quote::quote! {},
    };
    quote::quote! {
        #[command]
        async fn #cmd_ident(ctx: &Context, msg: &Message #args) -> CommandResult {
            let id = msg.channel_id;
            let author_line = format!("{} {}", msg.author.name, #phrase);
            #roll
            let description = format!("**Description**\n{}", #desc);
            id.send_message(ctx.http.clone(), |m| m.embed(|e| {
                e.description(description).author(|a| a.name(author_line)).title(#capital) #dice
            })).await?;
            Ok(())
        }
    }
}

fn playbook_command(name: &str, moves: &[&str]) -> proc_macro2::TokenStream {
    let cmd_ident = quote::format_ident!("{}", name);
    let msg = moves.join(", ");
    quote::quote! {
        #[command]
        async fn #cmd_ident(ctx: &Context, msg: &Message) -> CommandResult {
            reply(ctx, msg, #msg).await
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
    let mut playbook_moves: HashMap<&str, Vec<&str>> = HashMap::new();
    for mv in data.moves.iter() {
        write!(maddie, "{}\n", move_command(mv))?;
        commands.push(&mv.short_name);

        let playbook_entry = playbook_moves.entry(&mv.playbook);
        let cvec: &mut Vec<&str> = playbook_entry.or_insert(Vec::with_capacity(1));
        cvec.push(&mv.short_name);
    }
    for (playbook, moves) in playbook_moves.iter() {
        write!(maddie, "{}\n", playbook_command(playbook, moves))?;
        commands.push(playbook);
    }
    write!(maddie, "{}", command_group("MaddieTools", commands))?;
    Ok(())
}
