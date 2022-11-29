#!/bin/bash
# Attempts to lift the cntlm binary. Run without --a64-main-throw-errors=true and the output
# piping if you just want to get the bir file, rather than debug errors.

bap ${1:-../../cntlm} -d bir:cntlm.bir --primus-lisp-semantics=disable \
--asli-throw-errors=true \
--asli-prelude=$HOME/asl-interpreter/prelude.asl \
--asli-specs=$HOME/mra_tools/arch/regs.asl \
--asli-specs=$HOME/mra_tools/types.asl \
--asli-specs=$HOME/mra_tools/arch/arch.asl \
--asli-specs=$HOME/mra_tools/arch/arch_instrs.asl \
--asli-specs=$HOME/mra_tools/arch/arch_decode.asl \
--asli-specs=$HOME/asl-interpreter/tests/override.asl \
--asli-specs=$HOME/mra_tools/support/aes.asl \
--asli-specs=$HOME/mra_tools/support/barriers.asl \
--asli-specs=$HOME/mra_tools/support/debug.asl \
--asli-specs=$HOME/mra_tools/support/feature.asl \
--asli-specs=$HOME/mra_tools/support/hints.asl \
--asli-specs=$HOME/mra_tools/support/interrupts.asl \
--asli-specs=$HOME/mra_tools/support/memory.asl \
--asli-specs=$HOME/mra_tools/support/stubs.asl \
--print-missing \
> errors.txt 2> total.txt
