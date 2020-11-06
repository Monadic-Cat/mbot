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

#[derive(Deserialize, Debug)]
struct Playbook {
    id: u64,
    name: String,
    #[serde(rename = "mot")]
    moment_of_truth: String,
    #[serde(rename = "img")]
    image: String,
    source: String,
}

#[derive(Deserialize, Debug)]
struct Source {
    id: u64,
    source: String,
    name: String,
}

/// Top level of Maddie data JSON.
#[derive(Deserialize, Debug)]
struct MaddieData {
    moves: Vec<Move>,
    playbooks: Vec<Playbook>,
    sources: Vec<Source>,
}

fn move_command(m: &Move) -> proc_macro2::TokenStream {
    let short_name = &m.short_name;
    let cmd_ident = quote::format_ident!("{}", short_name);
    let desc = &m.description;
    let phrase = &m.phrase;
    let capital = &m.capital;
    match m.needs_roll {
        true => quote::quote! {
            #[command]
            async fn #cmd_ident(ctx: &Context, msg: &Message, args: Args) -> CommandResult {
                move_with_roll(ctx, msg, args, #phrase, #desc, #capital).await
            }
        },
        false => quote::quote! {
            #[command]
            async fn #cmd_ident(ctx: &Context, msg: &Message) -> CommandResult {
                move_without_roll(ctx, msg, #phrase, #desc, #capital).await
            }
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

fn playbook_list_command(playbooks: &[Playbook], sources: &[Source]) -> proc_macro2::TokenStream {
    let mut source_playbooks = std::collections::HashMap::new();
    let mut source_keys = std::collections::HashMap::new();
    for (idx, source) in sources.iter().enumerate() {
        source_playbooks.insert(&source.source, Vec::new());
        source_keys.insert(&source.source, idx);
    }
    for book in playbooks {
        let vec = source_playbooks.get_mut(&book.source).expect("every playbook has a real source");
        vec.push(&book.name);
    }
    let mut list: Vec<_> = source_playbooks.into_iter().collect();
    list.sort_by(|(a, _), (b, _)| {
        sources[source_keys[a]].id.cmp(&sources[source_keys[b]].id)
    });
    let mut body = String::new();
    for (source, vec) in list {
        write!(body, "**{}**\n", sources[source_keys[source]].name).unwrap();
        use std::fmt::Write;
        for (idx, book) in vec.iter().enumerate() {
            let book = ::titlecase::titlecase(book);
            if idx < vec.len() - 1 {
                write!(body, "{}, ", book).unwrap();
            } else {
                write!(body, "{}\n", book).unwrap();
            }
        }
    }
    quote::quote! {
        #[command]
        async fn playbooks(ctx: &Context, msg: &Message) -> CommandResult {
            let id = msg.channel_id;
            let author_line = "Available Playbooks are -";
            id.send_message(&ctx.http, |m| m.embed(|e| {
                e.author(|a| a.name(author_line)).title("Playbooks").description(#body)
            })).await?;
            Ok(())
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
    write!(maddie, "{}\n", playbook_list_command(&data.playbooks, &data.sources))?;
    commands.push("playbooks");
    write!(maddie, "{}", command_group("MaddieTools", commands))?;
    Ok(())
}
