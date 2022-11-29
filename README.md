# BAP ASLi Lifter Plugin

This project contains the ASLi lifter plugin. The file with most of the heavy lifting is `a64_lifter.ml`. The lifter is able to generate semantics for the armv8 instruction set, using the [ASL-interpreter](https://github.com/UQ-PAC/asl-interpreter) to automatically generate an OCaml representation of the semantics, which are then translated into BIR through this program.

Getting BAP and ASLi running together can be extremely difficult. In the ASL-interpreter repository I have created a [dockerfile](https://github.com/UQ-PAC/asl-interpreter/blob/partial_eval/dockerfile) that should be able to build a fully functioning environment. You can run these instructions individually on your own machine, but I strongly recommend using a docker container as it is not uncommon for BAP to break, and it is much easier to spin up a new container rather than trying to fix the issue.

A number of helpful bash scripts are contained in `plugins/a64/scripts` to help with the development workflow. These include the ability to build and install the plugin, run it with single instructions, and run it with large binary files. Your development workflow will consist of running the [`package_asli.sh`](https://github.com/UQ-PAC/asl-interpreter/blob/partial_eval/scripts/package_asli.sh) from the ASL-interpreter (only if you have made any changes to that), then running `build_a64_plugin.sh`, then running either of the `bap_mc_*` or the `test_cntlm.sh` scripts to test your changes.

In the lifter, if you need to implement a new operation, [this](http://binaryanalysisplatform.github.io/bap/api/odoc/bap-core-theory/Bap_core_theory/Theory/module-type-Basic/index.html#val-slt) page is useful for finding what you have available to you through BAP.

Future work currently consists of two things:
*   Optimising the BIR output. The most pressing issue is the `goto`s appearing between each instruction.
*   Handling `assert` and `throw` statements, possibly with some kind of intrinsic call.
