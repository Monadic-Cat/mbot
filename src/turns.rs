use chrono::{DateTime, Duration, Utc};
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
use serenity::model::id::{ChannelId, GuildId, UserId};
// Trying these out in this module.
use fehler::{throw as yeet, throws as yeets};
use once_cell::sync::Lazy;
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

struct PastTurn {
    player: PlayerId,
    order_pos: usize,
    time: DateTime<Utc>,
}

struct UpcomingTurn {
    player: PlayerId,
    order_pos: usize,
}

enum Round {
    Ordered(OrderedRound),
    Unordered(UnorderedRound),
}
trait Turn {
    fn players(&self) -> &[PlayerId];
    // fn start_date(&self) -> NaiveDateTime;
    fn can_skip(&self) -> bool;
    fn complete(&mut self, player: PlayerId) -> Result<(), ()>;
}
struct OrderedTurn {
    player: PlayerId,
    start_time: DateTime<Utc>,
}
impl Turn for OrderedTurn {
    fn players(&self) -> &[PlayerId] {
        core::slice::from_ref(&self.player)
    }
    fn can_skip(&self) -> bool {
        Utc::now() - self.start_time > Duration::hours(24)
    }
    #[yeets(())]
    fn complete(&mut self, player: PlayerId) {
        if self.player != player {
            yeet!(())
        }
    }
}
struct UnorderedTurn {}
impl Round {
    fn next_turn(&self) -> Option<impl Turn> {
        // TODO: do something real here
        Some(OrderedTurn {
            player: PlayerId { user_id: UserId(0) },
            start_time: Utc::now(),
        })
    }
}

struct UnorderedRound {
    start_time: DateTime<Utc>, // required for clean rollbacks
    acted: BTreeSet<PlayerId>,
    unacted: BTreeSet<PlayerId>,
}

/// Error: A turn order is empty.
struct EmptyTurnOrder;

struct OrderedRound {
    start_time: DateTime<Utc>, // required for clean rollbacks
    ordering: Vec<PlayerId>,
    last_turn: Option<PastTurn>,
    skipped: BTreeSet<PlayerId>,
}
impl OrderedRound {
    #[yeets(EmptyTurnOrder)]
    fn next_player(&self) -> UpcomingTurn {
        if self.ordering.len() == 0 {
            yeet!(EmptyTurnOrder)
        }
        if let Some(ref turn) = self.last_turn {
            // We're going to assume that we never have more than usize::MAX - 1 players here.
            let npos = if turn.order_pos + 1 < self.ordering.len() {
                turn.order_pos + 1
            } else {
                0
            };
            UpcomingTurn {
                player: self.ordering[npos],
                order_pos: npos,
            }
        } else {
            UpcomingTurn {
                // This is infallible because we know
                // that there's at least one player by this point.
                // Otherwise, an error return would have happened first.
                player: self.ordering[0],
                order_pos: 0,
            }
        }
    }
    /// Skip currently upcoming turn.
    fn skip_turn(&mut self) -> PlayerId {
        unimplemented!("turn skipping")
    }
    #[yeets(PlayerAlreadyPresent)]
    fn append_player(&mut self, player: PlayerId) {
        // Note that this performs a linear search
        // and may become slow with large numbers of players.
        // It is unlikely that I could manage a game
        // with so many players that this would become
        // noticeable, however.
        if self.ordering.contains(&player) {
            yeet!(PlayerAlreadyPresent)
        } else {
            self.ordering.push(player)
        }
    }
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
    let round = game.channels.get(&channel).and_then(|c| c.round);
    let channel_mode = ChannelData {
        default_game_mode: mode.map_or(game.default_mode, |x| x),
        round,
        control_state,
    };
    game.channels.insert(channel, channel_mode);
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
