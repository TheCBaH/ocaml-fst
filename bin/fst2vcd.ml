[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic, OpenAI"]

(* The options of GTKWave's fst2vcd. *)

let usage =
  "Usage: fst2vcd [OPTION]... [FSTFILE]\n\n\
   VCD is emitted to stdout if output filename is unspecified.\n"

let () =
  let fstname = ref None and outname = ref None and extensions = ref false in
  let set r v = r := Some v in
  let spec =
    [
      ("-e", Arg.Set extensions, " emit FST extensions to VCD");
      ("--extensions", Arg.Set extensions, " emit FST extensions to VCD");
      ("-f", Arg.String (set fstname), "FILE specify FST input filename");
      ("--fstname", Arg.String (set fstname), "FILE specify FST input filename");
      ("-o", Arg.String (set outname), "FILE specify output filename");
      ("--output", Arg.String (set outname), "FILE specify output filename");
    ]
  in
  Arg.parse (Arg.align spec) (fun a -> if !fstname = None then fstname := Some a) usage;
  let fail fmt =
    Printf.ksprintf
      (fun m ->
        prerr_endline m;
        exit 255)
      fmt
  in
  let path =
    match !fstname with
    | Some p -> p
    | None ->
        Arg.usage (Arg.align spec) usage;
        exit 0
  in
  match Fst.open_file path with
  | Error e -> fail "Could not open '%s', exiting: %s" path (Fst.Error.to_string e)
  | Ok t -> (
      let oc =
        match !outname with
        | None ->
            set_binary_mode_out stdout true;
            stdout
        | Some o -> (
            try open_out_bin o with Sys_error m -> fail "Could not open '%s', exiting: %s" o m)
      in
      let r = Fst.Vcd.write ~extensions:!extensions t oc in
      Fst.close t;
      close_out oc;
      match r with Ok () -> () | Error e -> fail "%s: %s" path (Fst.Error.to_string e))
