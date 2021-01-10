// This file uses these C language extensions:
// __attribute__((warn_unused_result))
// __builtin_trap()
// __builtin_unreachable()

#ifndef MBOT_SCHEDULER_PLUGIN_HELPER_BUMP
#define MBOT_SCHEDULER_PLUGIN_HELPER_BUMP
#include <stdint.h>
#include <stddef.h>
//! A bump allocator for plugins to use.
//! Doesn't provide a way to free particular segments of memory.

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

// This doesn't do anything to respect alignment,
// since WASM is perfectly fine with unaligned accesses,
// except possibly in the case of atomics.
/* We don't really need to export this. */
/* Not doing so saves us 44 bytes in the final WASM. */
/* __attribute__((export_name("alloc"))) */
__attribute__((warn_unused_result))
void* alloc(size_t size) {
   // Checked arithmetic.
   // It's a little annoying that I don't have
   // checked arithmetic functions available for free.
   if ((SIZE_MAX - bump_pointer) > size) {
      size_t ptr = bump_pointer;
      bump_pointer += size;
      return (void*)ptr;
   } else {
      // Allocation failure, due to overflow of the address space.
      // The most *absurd* way to do it, considering I know damn well
      // that we have less memory than that.
      // Given that I know this particular failure case will never occur,
      // because we'll trap on out of bounds access long before that,
      // we simply trap here as well.
      #ifndef MBOT_SCHEDULER_PLUGIN_HELPER_BUMP_ALLOC_UNCHECKED_SIZE_OVERFLOW
      // __builtin_trap() and __builtin_unreachable() are both
      // compiler builtins provided by Clang and GCC.
      __builtin_trap();
      #else
      // It is an option for us to return a 0 pointer here,
      // but there is no point in checking for this kind of failure
      // in our scenario.
      // It's also an option for us to not check for this at all,
      // but I'd rather dangerous behavior like that be behind a configuration option.
      __builtin_unreachable();
      #endif
   }
}

/// Frees the top `size` bytes of the heap.
/// If this fails, it returns the number of bytes left on the heap,
/// otherwise SIZE_T_MAX.
/// (Since if there were actually SIZE_T_MAX bytes on the heap,
///  it would be guaranteed not to fail.)
size_t free_top(size_t size) {
   size_t base = (size_t) &__heap_base;
   // We know that bump_pointer is always at least base,
   // so there's no worry of underflow in this calculation.
   if ((bump_pointer - base) > size) {
      bump_pointer -= size;
      return SIZE_MAX;
   } else {
      return bump_pointer - base;
   }
}

// We're a bump allocator, so this interface is pretty unimplementable.
void free(void* p) {
   // Simply leaking memory doesn't violate its contract, though.
   // Perhaps we'll move to a more powerful memory allocator,
   // one that's capable of freeing memory like this,
   // later. In that case, it'd be convenient to have already
   // written all the calls to `free(ptr)`.
}

#endif /* MBOT_SCHEDULER_PLUGIN_HELPER_BUMP */
