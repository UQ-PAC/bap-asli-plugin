#!/bin/bash
# Lift a given stream of bytes.
# Requires ASLI_PATH to point to your asl-interpreter checkout.

if [ $# != 1 ]
then
	echo "Usage: $0 <bytes>"
  echo "e.g. $0 \"20 00 02 8b\""
	exit 1
fi

if [[ -z "${ASLI_PATH}" ]]; then
  echo "Set environment variable ASLI_PATH to the asl-interpreter checkout"
  exit 1
fi

eval $(opam env)
export DYLD_LIBRARY_PATH=$(opam var z3:lib)

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
-- $1
