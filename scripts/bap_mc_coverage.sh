#!/bin/bash
# Tests all opcodes for given instruction.
# Requires ASLI_PATH to point to your asl-interpreter checkout.

if [ $# != 1 ] && [ $# != 3 ]
then
	echo "Usage: $0 instruction [in] [out]"
  echo "e.g. $0 aarch64_integer_arithmetic_add_sub_carry result.txt errors.txt"
	exit 1
fi

if [[ -z "${ASLI_PATH}" ]]; then
  echo "Set environment variable ASLI_PATH to the asl-interpreter checkout"
  exit 1
fi

eval $(opam env)
export DYLD_LIBRARY_PATH=$(opam var z3:lib)

# Get all the opcodes for the given instruction
OPCODES=$(echo ":opcodes A64 $1" | \
$ASLI_PATH/asli
$ASLI_PATH/prelude.asl
$ASLI_PATH/mra_tools/arch/regs.asl \
$ASLI_PATH/mra_tools/types.asl
$ASLI_PATH/mra_tools/arch/arch.asl \
$ASLI_PATH/mra_tools/arch/arch_instrs.asl
$ASLI_PATH/mra_tools/arch/arch_decode.asl \
$ASLI_PATH/mra_tools/support/aes.asl
$ASLI_PATH/mra_tools/support/barriers.asl \
$ASLI_PATH/mra_tools/support/debug.asl
$ASLI_PATH/mra_tools/support/feature.asl \
$ASLI_PATH/mra_tools/support/hints.asl
$ASLI_PATH/mra_tools/support/interrupts.asl \
$ASLI_PATH/mra_tools/support/memory.asl
$ASLI_PATH/mra_tools/support/stubs.asl \
$ASLI_PATH/mra_tools/support/fetchdecode.asl
$ASLI_PATH/tests/override.asl)

# Test all the opcodes
bap-mc --show-bir --arch=aarch64 \
--primus-lisp-semantics=disable \
--asli-prelude=$ASLI_PATH/prelude.asl \
--asli-specs=$ASLI_PATH/mra_tools/arch/regs.asl \
--asli-specs=$ASLI_PATH/mra_tools/types.asl \
--asli-specs=$ASLI_PATH/mra_tools/arch/arch.asl \
--asli-specs=$ASLI_PATH/mra_tools/arch/arch_instrs.asl \
--asli-specs=$ASLI_PATH/mra_tools/arch/arch_decode.asl \
--asli-specs=$ASLI_PATH/mra_tools/support/aes.asl \
--asli-specs=$ASLI_PATH/mra_tools/support/barriers.asl \
--asli-specs=$ASLI_PATH/mra_tools/support/debug.asl \
--asli-specs=$ASLI_PATH/mra_tools/support/feature.asl \
--asli-specs=$ASLI_PATH/mra_tools/support/hints.asl \
--asli-specs=$ASLI_PATH/mra_tools/support/interrupts.asl \
--asli-specs=$ASLI_PATH/mra_tools/support/memory.asl \
--asli-specs=$ASLI_PATH/mra_tools/support/stubs.asl \
--asli-specs=$ASLI_PATH/tests/override.asl \
-- $OPCODES > ${2:-/dev/stdout} 2> ${3:-/dev/stderr}
