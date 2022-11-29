#!/bin/bash
# Tests all opcodes for given instruction

if [ $# != 1 ] && [ $# != 3 ]
then 
	echo "Usage: $0 instruction [in] [out]"
    echo "e.g. bash bap_mc_coverage.sh aarch64_integer_arithmetic_add_sub_carry result.txt errors.txt"
	exit 0
fi

eval $(opam env)

# Get all the opcodes for the given instruction
OPCODES=$(echo ":opcodes A64 $1" | \
~/asl-interpreter/asli prelude.asl $HOME/mra_tools/arch/regs.asl \
$HOME/mra_tools/types.asl $HOME/mra_tools/arch/arch.asl \
$HOME/mra_tools/arch/arch_instrs.asl $HOME/mra_tools/arch/arch_decode.asl \
$HOME/mra_tools/support/aes.asl $HOME/mra_tools/support/barriers.asl \
$HOME/mra_tools/support/debug.asl $HOME/mra_tools/support/feature.asl \
$HOME/mra_tools/support/hints.asl $HOME/mra_tools/support/interrupts.asl \
$HOME/mra_tools/support/memory.asl $HOME/mra_tools/support/stubs.asl \
$HOME/mra_tools/support/fetchdecode.asl $HOME/asl-interpreter/tests/override.asl)

# Test all the opcodes
bap-mc --show-bir --arch=aarch64 --primus-lisp-semantics=disable \
--a64-main-prelude=$HOME/asl-interpreter/prelude.asl \
--a64-main-specs=$HOME/mra_tools/arch/regs.asl \
--a64-main-specs=$HOME/mra_tools/types.asl \
--a64-main-specs=$HOME/mra_tools/arch/arch.asl \
--a64-main-specs=$HOME/mra_tools/arch/arch_instrs.asl \
--a64-main-specs=$HOME/mra_tools/arch/arch_decode.asl \
--a64-main-specs=$HOME/asl-interpreter/tests/override.asl \
--a64-main-specs=$HOME/mra_tools/support/aes.asl \
--a64-main-specs=$HOME/mra_tools/support/barriers.asl \
--a64-main-specs=$HOME/mra_tools/support/debug.asl \
--a64-main-specs=$HOME/mra_tools/support/feature.asl \
--a64-main-specs=$HOME/mra_tools/support/hints.asl \
--a64-main-specs=$HOME/mra_tools/support/interrupts.asl \
--a64-main-specs=$HOME/mra_tools/support/memory.asl \
--a64-main-specs=$HOME/mra_tools/support/stubs.asl \
-- $OPCODES > ${2:-/dev/stdout} 2> ${3:-/dev/stderr}
