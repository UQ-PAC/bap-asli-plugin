#!/bin/bash
# Lift a given stream of bytes. 
# If you encounter errors, you may need to provide additional mra_tools paths
# Also, endianness between different bap calls may be incosistent. If you get further errors,
# you may need to switch the endianness in a64_lifter.ml

if [ $# != 1 ]
then 
	echo "Usage: $0 bytes"
    echo "e.g. bash bap_mc_single.sh \"20 00 02 8b\""
	exit 0
fi

eval $(opam env) &&
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
-- $1
