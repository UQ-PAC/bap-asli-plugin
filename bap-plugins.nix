{ symlinkJoin, ocamlPackages, makeWrapper, plugins ? []}:
  symlinkJoin {
    name = "bap";
    paths = [ ocamlPackages.bap ];
    buildInputs = [ makeWrapper ];
    postBuild = ''
      for b in $out/bin/*; do 
        wrapProgram $b \
          --append-flags "${toString (map (x: "-L ${x}") plugins)}"
      done
    '';
  }
