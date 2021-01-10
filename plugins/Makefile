OPT_LEVEL ?= O3

# Clang warnings to turn into hard errors.
# This list will expand as I encounter more
# warnings I think should be errors.
WARNING_ERRORS ?= -Werror=excess-initializers -Werror=return-type

# TODO: improve this sort of configuration
# Optimization options that have potentially disastrous effects
# in the case of programmer mistakes.
# We currently have two, which are defined on line 36 of `helpers/bump.h`,
# and line 26 of `helpers/map.h`.
DANGEROUS_CONFIGS ?= -D MBOT_SCHEDULER_PLUGIN_HELPER_BUMP_ALLOC_UNCHECKED_SIZE_OVERFLOW\
-D MBOT_SCHEDULER_PLUGIN_HELPER_MAP_UNCHECKED_ALLOC_FUNC

CFLAGS ?=--target=wasm32 -$(OPT_LEVEL) -flto --no-standard-libraries -Wl,--no-entry\
-Wl,--lto-$(OPT_LEVEL) $(WARNING_ERRORS) $(DANGEROUS_CONFIGS)

arrival.wasm: arrival.c api.h helpers/bump.h helpers/map.h wasm.syms
	clang $(CFLAGS) -Wl,-z,stack-size=1024 -Wl,-allow-undefined-file "wasm.syms" arrival.c -o arrival.wasm
	# wasm-strip arrival.wasm

masks.wasm: masks.c api.h wasm.syms
	clang $(CFLAGS) -Wl,-z,stack-size=1024 -Wl,-allow-undefined-file "wasm.syms" masks.c -o masks.wasm

.PHONY:
clean:
	rm *.wasm
