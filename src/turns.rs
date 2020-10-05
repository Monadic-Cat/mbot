use crate::reply;
use crate::IN_DEV_SERVER_CHECK;
use chrono::{DateTime, Utc};
/// Rules:
/// The game is composed of rounds.
/// A round is a collection of turns.
/// A turn is an action by a player that consumes time in world.
/// For ease of automation of this flow, a turn consists of a single post.
/// This may be extended by manual DM intervention,
/// but the super majority of actions a player will take should not
/// require more than 2000 unicode code points to express.
/// Each player gets a single turn per round.
/// A round may or may not have a defined ordering that players' turns must obey.
/// If it does:
///   A player drops their turn if they go a full 24 hours [PARTIAL_ABSENCE]
///   (tune constants as needed, perhaps parameterize over situation)
///   without posting. Perhaps enable accomodations when communication of busy-ness is done.
///   A player's turns become immediately player-skippable if 3 consecutive turns of theirs are skipped,
///   until they post or reacquire their turn lock by communication with the GM. [FULL_ABSENCE]
/// If it does not:
///   If a round proceeds for a full 24 hours without a post, [PARTIAL_ABSENCE]
///   the current turn is unlocked to players who have already posted that round.
///   A round consists of a number of turns equal to the number of players,
///   but not all players will necessarily post each round.
///   If a player goes 3 rounds without posting, [FULL_ABSENCE]
///   they are no longer included in the count of turns per round,
///   until they post or reacquire their turn by communication with the GM.
///   Such a dropped player may post after the end of any round without consuming a turn.
///   They will then be reinserted into the round player count.
/// If a round exits without any posts, the channel is STALE.
/// Certain kinds of round might auto create a new one following themselves,
/// and some might not. This should be included in the round creation command.
/// If a round goes STALE, however, such auto creation should be cancelled.
/// In addition, a notification will be placed in the notifications channel for the game.
// Trying these out in this module.
use fehler::{throw as yeet, throws as yeets};
use indexmap::IndexMap;
use once_cell::sync::Lazy;
use serenity::{
    framework::standard::{
        macros::{command, group},
        Args, CommandResult,
    },
    model::{
        channel::Message,
        id::{ChannelId, GuildId, UserId},
    },
    prelude::*,
};
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::sync::RwLock;
use thiserror::Error;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
struct PlayerId {
    user_id: UserId,
}

enum Presence {
    Present,
    PartiallyAbsent,
    FullyAbsent,
}

struct PlayerAlreadyPresent;

#[derive(Copy, Clone, Debug)]
pub(crate) enum GameMode {
    Ordered,
    Unordered,
}

#[derive(Error, Debug)]
#[error("invalid game mode")]
pub(crate) struct ParseGameModeError;
impl core::str::FromStr for GameMode {
    type Err = ParseGameModeError;
    #[yeets(Self::Err)]
    fn from_str(s: &str) -> Self {
        match s {
            "ordered" | "o" => Self::Ordered,
            "unordered" | "u" => Self::Unordered,
            _ => yeet!(ParseGameModeError),
        }
    }
}

#[derive(Debug)]
pub(crate) enum PostingControls {
    On,
    Off,
}

#[derive(Error, Debug)]
#[error("invalid post control state")]
pub(crate) struct ParseControlModeError;
impl core::str::FromStr for PostingControls {
    type Err = ParseControlModeError;
    #[yeets(Self::Err)]
    fn from_str(s: &str) -> Self {
        match s {
            "on" | "o" => Self::On,
            "off" | "f" => Self::Off,
            _ => yeet!(ParseControlModeError),
        }
    }
}

/// Represents a turn a player has taken.
/// We store the post time to enable clean rollbacks.
#[derive(Clone)]
struct UnorderedTurn(PlayerId, DateTime<Utc>);

/// Represents a turn a player has taken.
/// We store the post/skip time to enable clean rollbacks.
#[derive(Clone)]
enum OrderedTurn {
    Posted(PlayerId, DateTime<Utc>),
    Skipped(PlayerId, DateTime<Utc>),
}

#[derive(Clone)]
enum Round {
    Unordered {
        start_time: DateTime<Utc>, // required for clean rollbacks
        acted: IndexMap<PlayerId, UnorderedTurn>,
        unacted: BTreeSet<PlayerId>,
        // can't be more than this because BTreeSet can't contain more elements
        remaining_turns: usize,
    },
    Ordered {
        start_time: DateTime<Utc>, // required for clean rollbacks
        ordering: Vec<PlayerId>,
        // required for clean rollbacks in the presence of deletion of far preceding posts
        used: Vec<OrderedTurn>,
        // note that we can restore a player's turn in two ways:
        //  - roll back to it and undo the skip
        //  - insert a new turn for them to use in the current round's ordering
    },
}

struct ChannelData {
    default_game_mode: GameMode,
    control_state: PostingControls,
    round: Option<Round>,
}

struct Game {
    players: BTreeSet<PlayerId>,
    channels: HashMap<ChannelId, ChannelData>,
    default_mode: GameMode,
}
impl Game {
    fn new(default_mode: GameMode) -> Self {
        Self {
            players: BTreeSet::new(),
            channels: HashMap::new(),
            default_mode,
        }
    }
    #[yeets(PlayerAlreadyPresent)]
    fn add_player(&mut self, player: PlayerId) {
        if !self.players.insert(player) {
            yeet!(PlayerAlreadyPresent)
        }
    }
}

/// This should use a Tokio synchronization thing if
/// any applicable locks are held across `.await` points.
// TODO: support multiple games per server
// Note that the RwLock here isn't fine grained enough
// for the majority of cases.
// To reduce contention of usage between servers,
// we really should use Mutexes or RwLocks inside `Game`
// so that the majority of uses can take a shared reference
// to the global map. Currently, essentially all uses
// require a unique reference, which makes it no better than a Mutex.
// Ideally, the only two user facing operations that should require
// a unique reference to the global map are game creation and game deletion.
// TODO: add inner locks
// TODO: initialize from SQLite db
static SERVER_GAMES_MAP: Lazy<RwLock<HashMap<GuildId, Game>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

#[derive(Error, Debug)]
#[error("game already exists")]
pub(crate) struct GameAlreadyExists;

#[yeets(GameAlreadyExists)]
pub(crate) fn create_game(server: GuildId) {
    let mut map = SERVER_GAMES_MAP.write().unwrap();
    match map.insert(server, Game::new(GameMode::Unordered)) {
        Some(_) => yeet!(GameAlreadyExists),
        None => (),
    }
}

#[derive(Error, Debug)]
#[error("game doesn't exist")]
pub(crate) struct NonexistentGameError;
#[yeets(NonexistentGameError)]
pub(crate) fn manage_channel(
    server: GuildId,
    channel: ChannelId,
    control_state: PostingControls,
    mode: Option<GameMode>,
) {
    let mut map_lock = SERVER_GAMES_MAP.write().unwrap();
    let game = map_lock.get_mut(&server).ok_or(NonexistentGameError)?;
    use std::collections::hash_map::Entry;
    let chan_entry = game.channels.entry(channel);
    match chan_entry {
        Entry::Vacant(e) => {
            e.insert(ChannelData {
                default_game_mode: mode.map_or(game.default_mode, |x| x),
                round: None,
                control_state,
            });
        }
        Entry::Occupied(mut e) => {
            let chan = e.get_mut();
            match mode {
                Some(x) => chan.default_game_mode = x,
                None => (),
            }
            chan.control_state = control_state;
        }
    }
}

#[derive(Error, Debug)]
pub(crate) enum AddPlayerError {
    #[error("game doesn't exist")]
    NoGame,
    #[error("player already added")]
    PlayerAlreadyPresent,
}
#[yeets(AddPlayerError)]
pub(crate) fn add_player(server: GuildId, player: UserId) {
    let mut map_lock = SERVER_GAMES_MAP.write().unwrap();
    let game = map_lock.get_mut(&server).ok_or(AddPlayerError::NoGame)?;
    if !game.players.insert(PlayerId { user_id: player }) {
        yeet!(AddPlayerError::PlayerAlreadyPresent)
    }
}

#[derive(Debug)]
pub(crate) enum YesNo {
    Yes,
    No,
}
#[derive(Error, Debug)]
#[error("invalid yes-no")]
pub(crate) struct ParseYesNoError;
impl core::str::FromStr for YesNo {
    type Err = ParseYesNoError;
    #[yeets(Self::Err)]
    fn from_str(s: &str) -> Self {
        match s {
            "yes" => Self::Yes,
            "no" => Self::No,
            _ => yeet!(ParseYesNoError),
        }
    }
}

#[derive(Error, Debug)]
#[error("not your turn")]
pub(crate) struct OffTurnMessage;
/// If game does not exist, does nothing.
/// If game exists, but does not contain the channel, does nothing.
/// If game exists and contains the given channel, tries using turn.
/// If message is off turn, does not advance round and returns error.
// Note that enforcing this even in weird situations
// requires keeping a stack of the used turns in the current round.
#[yeets(OffTurnMessage)]
pub(crate) fn receive_message(
    server: GuildId,
    channel: ChannelId,
    player: UserId,
    message: String,
) {
    let map_lock = SERVER_GAMES_MAP.write().unwrap();
    let game = match map_lock.get(&server) {
        Some(x) => x,
        None => return,
    };
}

use crate::POOL;

#[derive(Debug, Copy, Clone)]
pub(crate) struct TurnErrInfo {
    notify_channel: Option<ChannelId>,
}

impl TurnErrInfo {
    pub(crate) fn notify_channel(&self) -> Option<ChannelId> {
        self.notify_channel
    }
}

#[derive(Error, Debug)]
pub(crate) enum TurnError {
    #[error("not in this game")]
    NotInGame(TurnErrInfo),
    #[error("not in this round")]
    NotInRound(TurnErrInfo),
    #[error("not your turn")]
    WrongTurn(TurnErrInfo),
    #[error("no game in this channel")]
    NoGameHere,
    #[error("{0}")]
    SqlxError(#[from] sqlx::Error),
}

impl TurnError {
    pub(crate) fn notify_channel(&self) -> Option<ChannelId> {
        use TurnError::*;
        match self {
            NotInGame(info) | NotInRound(info) | WrongTurn(info) => info.notify_channel(),
            _ => None,
        }
    }
}

use sqlx::{query, query_as};

#[yeets(TurnError)]
pub(crate) async fn attempt_turn(player: i64, channel: i64, time: DateTime<Utc>) {
    let mut conn = POOL.begin().await?;
    let game_id = match query!("SELECT GameID FROM Channels WHERE ID = ?", channel)
        .fetch_one(&mut conn)
        .await
    {
        Err(sqlx::Error::RowNotFound) => yeet!(TurnError::NoGameHere),
        x => x?,
    };
    println!("Row: {:?}", game_id);
    unimplemented!("actually taking turns, lol")
}

#[derive(Error, Debug)]
pub(crate) enum InferenceError {
    #[error("no game found")]
    NoGame,
    #[error("sqlx error: {0}")]
    SqlxError(#[from] ::sqlx::Error),
}

/// Attempt to use available information to figure out
/// which game a user is trying to manipulate.
///
/// Take care not to give this information that would
/// produce surprising results for the user.
// This may want to take a transaction parameter.
pub(crate) async fn infer_game(
    guild_id: i64,
    channel_id: Option<i64>,
    game_name: Option<String>,
) -> Result<i64, InferenceError> {
    let mut conn = POOL.acquire().await?;
    match game_name {
        Some(n) => match query!(
            "SELECT ID FROM Games WHERE ServerID = ? AND GameName = ?",
            guild_id,
            n
        )
        .fetch_one(&mut conn)
        .await
        {
            Ok(x) => Ok(x.ID),
            Err(sqlx::Error::RowNotFound) => Err(InferenceError::NoGame),
            Err(e) => Err(e.into()),
        },
        None => match (
            query!("SELECT ID FROM Games WHERE ServerID = ? LIMIT 2", guild_id)
                .fetch_all(&mut conn)
                .await,
            channel_id,
        ) {
            (Ok(games), _) if games.len() == 1 => Ok(games[0].ID),
            // attempt channel inference if available
            (Ok(games), Some(chan)) => {
                struct Channel {
                    GameID: Option<i64>,
                }
                match query_as!(Channel, "SELECT GameID FROM Channels WHERE ID = ?", chan)
                    .fetch_one(&mut conn)
                    .await
                {
                    Ok(Channel { GameID: Some(x) }) => Ok(x),
                    Ok(_) => Err(InferenceError::NoGame),
                    Err(sqlx::Error::RowNotFound) => Err(InferenceError::NoGame),
                    Err(e) => Err(e.into()),
                }
            }
            (Ok(games), None) => Err(InferenceError::NoGame),
            (Err(sqlx::Error::RowNotFound), _) => Err(InferenceError::NoGame),
            (Err(e), _) => Err(e.into()),
        },
    }
}

#[command]
#[only_in(guilds)]
#[min_args(0)]
#[max_args(1)]
#[aliases("start_game", "sg")]
async fn gm_start_game(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    use sqlx::{
        query,
        query_as, // Done
    };
    let guild_id = msg.guild_id.unwrap().0 as i64; // only_in(guilds)
    let player_id = msg.author.id.0 as i64;
    let game_name: Option<String> = args.single().ok();
    let mut transaction = POOL.begin().await?;
    query!("INSERT INTO Servers (ID) VALUES (?)", guild_id)
        .execute(&mut transaction)
        .await;
    struct GameID {
        #[allow(non_snake_case)]
        ID: i64,
    }
    // Unfortunately, SQLite (and many others) treat NULLs as distinct from each other.
    // Our UNIQUE constraint does not prevent inserting multiple rows with the same
    // ServerID and NULL GameName.
    // See https://www.sqlite.org/nulls.html for more information.
    // So, we enforce it here.
    // We will also need to enforce it for any game renaming functionality
    // we provide.
    let preexisting = match game_name {
        Some(ref name) => {
            query_as!(
                GameID,
                "SELECT ID FROM Games WHERE ServerID = ? AND GameName = ?",
                guild_id,
                name
            )
            .fetch_one(&mut transaction)
            .await
        }
        None => {
            query_as!(
                GameID,
                "SELECT ID FROM Games WHERE ServerID = ? AND GameName IS NULL",
                guild_id
            )
            .fetch_one(&mut transaction)
            .await
        }
    };
    match preexisting {
        Ok(_) => reply(ctx, msg, "game already exists.").await?,
        Err(sqlx::Error::RowNotFound) => {
            query!(
                "INSERT INTO Games (ServerID, GameMaster, GameName)
                    VALUES (?, ?, ?)",
                guild_id,
                player_id,
                game_name
            )
            .execute(&mut transaction)
            .await?;
            reply(ctx, msg, "successfully created the game!").await?;
        }
        Err(e) => return Err(e.into()),
    }
    transaction.commit().await?;
    Ok(())
}

#[command]
#[only_in(guilds)]
#[aliases("list_games", "lg")]
async fn gm_list_games(ctx: &Context, msg: &Message, _: Args) -> CommandResult {
    use sqlx::query;
    let guild_id = msg.guild_id.unwrap().0 as i64;
    let mut connection = POOL.acquire().await?;
    let games = match query!("SELECT * FROM Games where ServerID = ?", guild_id)
        .fetch_all(&mut connection)
        .await
    {
        Ok(x) => x,
        Err(e) => {
            log::error!("sqlx: {}", e);
            return Err(e.into());
        }
    };
    use comfy_table::Table;
    let mut table = Table::new();
    table.set_header(vec!["Game ID", "Game Name"]);
    table.force_no_tty();
    for x in games.into_iter() {
        table.add_row(vec![
            format!("{}", x.ID),
            x.GameName.map_or(String::new(), |n| format!("{}", n)),
        ]);
    }
    reply(ctx, msg, &format!("```{}```", table)).await
}

#[command]
#[only_in(guilds)] // some `.unwrap()`s in this function rely on this.
#[min_args(2)]
#[max_args(3)]
#[aliases("manage_channel", "mc")]
async fn gm_manage_channel(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    let channel: ChannelId = args.single()?;
    let control_state: PostingControls = args.single()?;
    let game_mode: Option<GameMode> = match args.current() {
        Some(x) => {
            let a = Some(x.parse()?);
            args.advance();
            a
        }
        None => None,
    };
    // TODO: take game name argument
    let guild_id = msg.guild_id.unwrap().0 as i64; // only_in(guilds)
    if msg
        .guild(&ctx.cache)
        .await
        .unwrap()
        .channels
        .contains_key(&channel)
    {
        use sqlx::query;
        let mut transaction = POOL.begin().await.unwrap();
        let game = infer_game(guild_id, None, None).await;
        println!("game: {:?}", game);
        // query!("INSERT INTO Channels (ID, GameID, ControlState, DefaultGameMode)
        //         VALUES (?, ?, ?, ?) UPSERT");
        match manage_channel(msg.guild_id.unwrap(), channel, control_state, game_mode) {
            Ok(_) => reply(ctx, msg, "channel configured").await,
            Err(e) => reply(ctx, msg, &format!("{}", e)).await,
        }
    } else {
        reply(ctx, msg, "no such channel in this server").await
    }
}

#[command]
#[only_in(guilds)]
#[min_args(1)]
#[max_args(2)]
#[aliases("add_player", "ap")]
async fn gm_add_player(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    use foretry::async_try;
    use sqlx::query;
    use thiserror::Error;
    let guild_id = msg.guild_id.unwrap().0 as i64;
    let user = args.single::<serenity::model::id::UserId>()?.0 as i64;
    let game_name: Option<String> = match args.current() {
        Some(x) => {
            let a = Some(x.parse()?);
            args.advance();
            a
        }
        None => None,
    };
    let mut transaction = POOL.begin().await?;
    let game_id: i64 = match infer_game(guild_id, Some(msg.channel_id.0 as i64), game_name).await {
        Ok(x) => x,
        Err(InferenceError::NoGame) => return reply(ctx, msg, "no game found.").await,
        Err(InferenceError::SqlxError(e)) => return Ok(log::error!("sqlx: {}", e)),
    };
    match query!(
        "INSERT INTO Players (ID, GameID) VALUES (?, ?)",
        user,
        game_id
    )
    .execute(&mut transaction)
    .await
    {
        Ok(_) => reply(ctx, msg, "added player").await?,
        // https://sqlite.org/rescode.html#constraint
        // Search "1555". That's the SQLite extended error code for violating
        // the uniqueness constraint here.
        // Considering the sqlstatus codes aren't portable,
        // I don't mind downcasting to SqliteError here.
        // Much of the SQL I've been doing isn't portable, anyhow.
        // If I move to another DB, it will be a chore.
        // So be it.
        Err(sqlx::Error::Database(e))
            if e.downcast_ref::<sqlx::sqlite::SqliteError>().sqlite_code() == 1555 =>
        {
            reply(ctx, msg, "player already added").await?
        }
        Err(e) => {
            log::error!("sqlx: {}", e);
            return Err(e.into());
        }
    };
    match transaction.commit().await {
        Ok(_) => (),
        Err(e) => log::error!("sqlx: {}", e),
    }
    Ok(())
}
#[command]
#[only_in(guilds)]
#[aliases("extend_turn", "et")]
async fn gm_extend_turn(_ctx: &Context, _msg: &Message, _args: Args) -> CommandResult {
    // This is actually not supported by the DB as is.
    // This can either be handled by tacking on an extra turn,
    // or by letting turns span multiple messages.
    // Neither is completely trivial.
    unimplemented!("turn extensions")
}
#[command]
#[only_in(guilds)]
#[aliases("round_unordered", "ru")]
async fn gm_round_unordered(_ctx: &Context, _msg: &Message, _args: Args) -> CommandResult {
    unimplemented!("starting unordered rounds")
}
#[command]
#[only_in(guilds)]
#[aliases("round_ordered", "ro")]
async fn gm_round_ordered(_ctx: &Context, _msg: &Message, _args: Args) -> CommandResult {
    unimplemented!("starting ordered rounds")
}

// TODO: figure out what kind of controls we'll want for manually skipping turns.
#[command]
#[only_in(guilds)]
#[aliases("skip_turn", "st")]
async fn gm_skip_turn(_ctx: &Context, _msg: &Message, _args: Args) -> CommandResult {
    unimplemented!("turn skipping")
}

#[command]
#[only_in(guilds)]
#[aliases("set_notification_channel", "snc")]
async fn gm_set_notification_channel(_ctx: &Context, _msg: &Message, _args: Args) -> CommandResult {
    unimplemented!("notification channel")
}

#[command]
#[aliases("skip")]
async fn player_skip_turn(_ctx: &Context, _: &Message, _: Args) -> CommandResult {
    unimplemented!("player turn skipping")
}

#[command]
#[only_in(guilds)]
#[aliases("notify")]
async fn player_notify(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    let yes_no: YesNo = args.single()?;
    unimplemented!("player notification");
    reply(ctx, msg, &format!("will notify: {:?}", yes_no)).await
}

#[group]
#[prefix("gm")]
#[commands(
    gm_start_game,
    gm_list_games,
    gm_manage_channel,
    gm_add_player,
    gm_extend_turn,
    gm_round_unordered,
    gm_round_ordered,
    gm_skip_turn,
    gm_set_notification_channel
)]
#[checks(in_dev_server)]
#[allowed_roles("GM")]
struct GMTools;

#[group]
#[commands(player_skip_turn, player_notify)]
#[checks(in_dev_server)]
struct PlayerTools;
