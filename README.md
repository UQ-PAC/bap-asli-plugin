# BAP ASLi Lifter Plugin

This project contains the ASLi lifter plugin. The file with most of the heavy lifting is `a64_lifter.ml`. The lifter is able to generate semantics for the armv8 instruction set, using the [ASL-interpreter](https://github.com/UQ-PAC/asl-interpreter) to automatically generate an OCaml representation of the semantics, which are then translated into BIR through this program.
In the lifter, if you need to implement a new operation, [this](http://binaryanalysisplatform.github.io/bap/api/odoc/bap-core-theory/Bap_core_theory/Theory/module-type-Basic/index.html#val-slt) page is useful for finding what you have available to you through BAP.

## Installation

To build and run this plugin, you'll need:
*   BAP, likely a version greater than 2.5.0
*   [ASLi](https://github.com/UQ-PAC/asl-interpreter/blob/partial_eval) with partial evaluation

### BAP

Install BAP using the standard install instructions, such as `opam install bap`.

### ASLi

Checkout and install ASLi following its README or the summarised steps below.
Note that this plugin must be rebuilt to include any changes to the ASLi project.

```
opam install dune z3 zarith alcotest ocamlfind pprint ott menhir linenoise
export LD_LIBRARY_PATH=`opam var z3:lib`
eval $(opam env)
git clone -b partial_eval https://github.com/UQ-PAC/asl-interpreter.git
make -C asl-interpreter install
```

### Plugin

If the requirements have been installed, run `make` in this directory to build and install the plugin.

## Use

Requires the architecture specification to be provided.

```
export ASLI_PATH=<PATH TO ASLI DIR>
bap <binary> --primus-lisp-semantics=disable \
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
             --asli-specs=$ASLI_PATH/tests/override.asl
```

Various scripts are provided in `scripts` to pass the appropriate options to `bap` given the `ASLI_PATH` environment variable is set.
