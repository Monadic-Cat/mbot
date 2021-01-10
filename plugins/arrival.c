//! A turn scheduler plugin for my game, which shall run in its own server.
//! Things will, naturally, be quite specific to my game.
#include "api.h"
#include "helpers/bump.h"
#include "helpers/map.h"
#include "players.h"

/// Quick macro to make defining types less annoying.
#define defstruct(name) typedef struct name name; \
   struct name
/// Quick macro to make defining union types less annoying.
#define defunion(name) typedef union name name; \
   union name
#define deftag(name) typedef enum name name; \
   enum name

defstruct(OrderedRound) {
   size_t size;
   PlayerId ordering[];
};
defstruct(UnorderedRound) {
   size_t size;
   PlayerId players[];
};

defunion(Round) {
   uint8_t tag;
   OrderedRound ordered;
   UnorderedRound unordered;
};

// TODO: partial orderings
// Some cases of partial ordering may be constructed by the catenation
// of both ordered and unordered rounds.
// Something to think about, though.

defstruct(TurnQuery) {
   PlayerId player;
   Timestamp time;
   ChannelId channel;
};
deftag(EventKind) {
   DoTurn, ExtendTurn, SkipTurn, MissTurn, LoseTurnLock,
};
defstruct(Event) {
   EventKind kind;
   TurnQuery target;
   // ID of associated message, if any.
   // Option<MessageId>
   // 0 is used as the None variant.
   MessageId msg;
};

defmap(TurnLockMap, map, bool, alloc,
       turn_lock_map_with_capacity,
       turn_lock_map_get_lock,
       turn_lock_map_insert);

defstruct(Channel) {
   ChannelId id;
   TurnLockMap lock_map;
};

defmap(ChannelMap, map, Channel, alloc,
       channel_map_with_capacity,
       channel_map_get_channel,
       channel_map_insert_channel);

defstruct(State) {
   ChannelMap channel_map;
};


// GM approved accommodation
void extend_turn(TurnQuery query);
// GM approved turn skip
void skip_turn(TurnQuery query);

// Internal thing, triggered by pass_time.
void drop_turn(TurnQuery query);

PermittedPlayers* permitted_players(Timestamp now, ChannelId channel, const void* ptr) {
   const State* state = ptr;
   
   return 0;
}
