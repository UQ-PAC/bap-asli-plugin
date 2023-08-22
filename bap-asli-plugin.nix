{ stdenv,
  lib,
  aslp,
  ocamlPackages, 
}:


stdenv.mkDerivation {
  name = "bap-asli-plugin";
  version = "0.1.0";

  src = (lib.sourceByRegex ./. [".*\\.ml" "Makefile"]);

  buildInputs = [ aslp ocamlPackages.bap ocamlPackages.findlib ];

  buildPhase = ''
    runHook preBuild

    bapbuild -package asli.libASL asli.plugin
    mkdir -p $out
    cp asli.plugin $out/asli.plugin

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    runHook postInstall
  '';

  meta = {
    homepage = "https://github.com/inhabitedtype/angstrom";
    description = "OCaml parser combinators built for speed and memory efficiency";
    license = lib.licenses.bsd3;
    maintainers = with lib.maintainers; [ sternenseemann ];
  };
}
