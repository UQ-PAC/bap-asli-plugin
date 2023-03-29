open Bap.Std
open Bap_main
open Extension.Syntax

include Self()

include struct
  open Extension

  let prelude =
    Configuration.parameter Type.non_dir_file "prelude"
      ~doc:"Path to ASL prelude."

  let specs =
    Configuration.parameters Type.(list non_dir_file) "specs"
      ~doc:"List of ASL specification file paths."

  let disable =
    Configuration.parameter Type.bool "disable"
      ~doc:"Disable the ASL lifter."

end

let () = Bap_main.Extension.declare ~doc @@ fun ctxt -> 
  let prelude = ctxt-->prelude in
  let specs = List.concat (ctxt-->specs) in
  if ctxt-->disable then Ok () else
  Ok (Asli_lifter.load prelude specs)
