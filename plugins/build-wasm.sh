#!/bin/sh

# The default maximum stack size is 64 KiB,
# but we can change it with the `-Wl,-z,stack-size=<size>`
# option.
# We don't do that here, because we don't need a big stack.

OUTPUT_FILE=masks.wasm

clang \
    --target=wasm32 \
    -O3 \
    -flto \
    --no-standard-libraries \
    -Wl,--no-entry \
    -Wl,--lto-O3 \
    -Wl,-z,stack-size=1024 \
    -Wl,-allow-undefined-file "wasm.syms" \
    "$1" \
    -o "$OUTPUT_FILE"

# If compilation was a success, strip out unnecessary sections.
if [ $? = 0 ] ; then
    wasm-strip "$OUTPUT_FILE"
fi
