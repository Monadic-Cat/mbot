#ifndef MBOT_SCHEDULER_PLUGIN_HELPER_MAP
#define MBOT_SCHEDULER_PLUGIN_HELPER_MAP

//! Map for plugins to use.
//! Lookups are currently linear searches.

// We could implement arbitrary key types via hashing,
// although that's low on the priority list at the moment.
typedef uint64_t MapKey;

typedef struct Map Map;
struct Map {
   size_t length;
   size_t capacity;
   MapKey* keys;
   void* items;
};

Map map_with_capacity(size_t capacity, size_t item_size, void* alloc_func(size_t)) {
   if (alloc_func != 0) {
      MapKey* keys = alloc_func(capacity * sizeof(MapKey));
      void* items = alloc_func(capacity * item_size);
      Map map = { 0, capacity, keys, items };
      return map;
   } else {
      #ifndef MBOT_SCHEDULER_PLUGIN_HELPER_MAP_UNCHECKED_ALLOC_FUNC
      __builtin_trap();
      #else
      __builtin_unreachable();
      #endif
   }
}

void* map___item_offset(size_t index, size_t item_size, void* items) {
   size_t offset = (size_t) items;
   offset += item_size * index;
   return (void*) offset;
}

void* map_lookup(MapKey key, size_t item_size, Map map) {
   for (size_t i = 0; i < map.length; i++) {
      if (map.keys[i] == key) {
         return map___item_offset(i, item_size, map.items);
      }
   }
   return 0;
}

typedef enum MapEntryKind MapEntryKind;
enum MapEntryKind {
   Empty, Filled, Unavailable
};
typedef struct MapEntry MapEntry;
struct MapEntry {
   MapEntryKind kind;
   void* item;
};

/// Gets the entry for a key, by lookup or creating a new one.
MapEntry map_entry(MapKey key, size_t item_size, Map map) {
   for (size_t i = 0; i < map.length; i++) {
      if (map.keys[i] == key) {
         MapEntry entry = { Filled, map___item_offset(i, item_size, map.items) };
         return entry;
      }
   }
   if (map.length < map.capacity) {
      MapEntry entry = { Empty, map___item_offset(map.length, item_size, map.items) };
      map.length += 1;
      return entry;
   } else {
      MapEntry failure = { Unavailable, 0 };
      return failure;
   }
}

/// Basic convenience wrapper around untyped map interface.
#define defmap(name, field_name, item_type, alloc_func, constructor_name, get_name, insert_name) \
   typedef struct name name;                                            \
   struct name {                                                        \
      Map field_name;                                                   \
   };                                                                   \
   name constructor_name (size_t capacity) {                            \
   name map = { map_with_capacity(capacity, sizeof(item_type), alloc_func) }; \
   return map;                                                          \
   }                                                                    \
    item_type* get_name(MapKey key, name map) {                         \
       return map_lookup(key, sizeof(item_type), map.field_name);       \
    }                                                                   \
    bool insert_name(MapKey key, item_type item, name m) {              \
       Map map = m.field_name;                                          \
       MapEntry entry = map_entry(key, sizeof(item_type), map);         \
       item_type* it_ptr;                                               \
       switch (entry.kind) {                                            \
          case Empty:                                                   \
             it_ptr = entry.item;                                       \
             *it_ptr = item;                                            \
             return true;                                               \
          default:                                                      \
             return false;                                              \
       }                                                                \
    }

#endif /* MBOT_SCHEDULER_PLUGIN_HELPER_MAP */
