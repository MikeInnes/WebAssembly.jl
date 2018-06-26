# Converts a module to its bytecode representation.

# The bytecode layout is somewhat different to the structure of the wasm IR. It
# would make sense to restructure the IR into an intermediary format and then
# make a short hop to bytecode.

const magic   = UInt32(0x6d736100) # \0asm
const version = UInt32(0x1)        # Targeting version 1
