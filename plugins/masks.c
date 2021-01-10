#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include "api.h"

typedef struct State State;
struct State {
   PlayerId player;
};

// This is passed a pointer to whatever it gives out.
PlayerId next_player(const void* ptr) {
   const State* state = ptr;
   return state->player;
}

bool advance(PlayerId player, void* ptr) {
   State* state = ptr;
   state->player = player;
   return true;
}

// I *believe* WASM memory is automatically zeroed.
// Let's make a bump allocator.
extern unsigned char __heap_base;
// TODO: Ensure this is never at address 0,
// so we can use null pointers as None variants.
// This is already practically the case, since we know
// that the heap is placed after the stack.
// Still, I'd like some check after linking to signal failure
// if something strange happens here.
size_t bump_pointer = (size_t) &__heap_base;

// This doesn't do anything to respect alignment.
/* We don't really need to export this. */
/* Not doing so saves us 44 bytes in the final WASM. */
/* __attribute__((export_name("alloc"))) */
void* alloc(size_t size) {
   size_t ptr = bump_pointer;
   bump_pointer += size;
   return (void*)ptr;
}

// Returns a pointer to a fat pointer to our state.
FatPointer* initialize() {
   FatPointer* fptr = alloc(sizeof(FatPointer));
   fptr->size = sizeof(State);
   fptr->ptr = alloc(sizeof(State));
   print("Successfully allocated for and initialized stuff\n");
   return fptr;
}
