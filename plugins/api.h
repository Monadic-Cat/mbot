#ifndef MBOT_SCHEDULER_PLUGIN_API
#define MBOT_SCHEDULER_PLUGIN_API
//! This file documents the correct way for a plugin to operate,
//! but the host is not permitted to require correct plugin operation
//! for the assurance of its own performance or security.
//! The plugin, however, is allowed to assume correct host operation.
//! Asymmetric trust relationship and all that, lol

// The host will never invoke plugin code from more than one thread at once.
// If a plugin is used in more than one guild, each such instance will operate
// on its own memory and its own guild local record store.
// So, the plugin is specifically encouraged to use all the "globals" it wants,
// since all of those are really guild local.
// TODO: further narrowing of scope?
// I won't narrow the scope smaller than guilds until two things are possible:
//  - Routing different channel categories' events to different shards
//  - Shrinking the WASM page size to smaller than 64 KiB
// If and when those two things become possible, I will consider narrowing the
// scope of plugins further.
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

typedef uint64_t PlayerId;
typedef uint64_t ChannelId;
typedef uint64_t MessageId;
/// We use number of milliseconds since Discord Epoch,
/// the first second of 2015, which is 1420070400000 milliseconds
/// after the Unix Epoch.
typedef uint64_t Timestamp;

// If I wanted this to be extra fast, since
// wasm32 uses 32 bit addresses but lets us return
// 64 bit integers, I could staple these two numbers
// together and return them directly.
typedef struct FatPointer FatPointer;
struct FatPointer {
   size_t size;
   void* ptr;
};

/// This function is particularly special.
/// It is called once when the plugin is
/// loaded for a guild, and never again.
/// It returns a pointer to a fat pointer
/// to its immediate state.

// TODO: Specify more guarantees about the location of
// state being copied back in, so absolute pointers can be used.
// This is *almost* required for us to skirt around the
// need for a distinct records store as outlined below.
// The alternative is to iterate over all pointers in
// plugin state and rewrite them to IDs or something
// and rewrite them back to offsets after being copied back in.
// Or, of course, to rely heavily on the records mechanism.

/// This immediate state is of a fixed but unknown (to the host)
/// size, containing no absolute pointers.
__attribute__((export_name("initialize")))
FatPointer* initialize();
FatPointer* suspend(void* state);
// TODO: decide if plugins should know about
// their runtime suspension and resumption.
void resume(size_t size, void* ptr);

/// Time event handler.
__attribute__((export_name("pass_time")))
void pass_time(Timestamp now);

typedef struct PermittedPlayers PermittedPlayers;
struct PermittedPlayers {
   size_t count;
   PlayerId players[];
};

/// Look at the player(s) who should be going right now.
__attribute__((export_name("permitted_players")))
PermittedPlayers* permitted_players(Timestamp now, ChannelId channel, const void* state);

__attribute__((export_name("advance")))
bool advance(PlayerId player, Timestamp now, void* state);

// Plugins are not required to provide this function.
// They are perfectly well allowed to simply hard code
// what players there are in their game.
__attribute__((export_name("add_player")))
bool add_player(PlayerId player, void* state);

/// Sets the time between host invocations of the `pass_time` function.
int set_timer_granularity(uint32_t interval);

// Persistent storage provided by mbot.
// The collection of records is implicitly guild local,
// and hidden from direct access by the plugin.
// The amount of storage allowed to be granted to a plugin
// is currently a fixed quantity, but may become proportional
// to the number of players in the guild at some point.
// Still, depending on how this ends up being used,
// this system may be overkill.
// I'll leave it unimplemented on the host side until I encounter
// a real use for it on this side.
// There are a number of open questions though:
//  - How should I handle suspending plugins?
//   - What state should I save automatically?
//  - How should I handle waking plugins?
//   - What state should I insert to a plugin's memory on wake?
typedef struct Record Record;
struct Record {
   uint64_t buf[8];
};
typedef uint32_t Key;

/// Saves a `Record`.
/// Returns the number of record slots left.
/// `-1` if failure because the store is full.
/// `-2` if failure because the store is locked.
int save_record(Key key, const Record* record);
/// Load a record with key, returns `false` on failure.
/// The only reason for failure is the nonexistence of
/// the requested record.
bool load_record(Key key, Record* out);

// mbot should provide the guarantee that no records will
// be created or deleted while a `RecordIter` lives.
// Pull based iteration, to give the most flexibility to
// the plugin writer.
typedef struct RecordIter RecordIter;
struct RecordIter {
   Record current_value;
   Key current_key;
};
int iter_records(RecordIter* out);
int close_records_iter(RecordIter* self);
int next_record(RecordIter* self);
int prev_record(RecordIter* self);

/// A debug printing routine.
/// This may or may not have its output filtered, or simply discarded.
int print(const char* str);

#endif  /* MBOT_SCHEDULER_PLUGIN_API */
