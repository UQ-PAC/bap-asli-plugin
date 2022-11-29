#!/bin/bash
# Builds the a64 lifter plugin for bap.
# Requires that the asli package has been installed 
#    (see package_asli.sh in the asl-interpreter repo)
# If a build fails, you may need to run the steps individually,
# since the rm will fail. We rm to avoid any cache issues.

cd $HOME/bap/plugins/a64 && eval $(opam env) &&
rm -rf _build &&
rm *.plugin &&
bapbuild -package asli.libASL a64_main.plugin &&
mv a64_main.plugin a64.plugin &&
bapbundle install a64.plugin