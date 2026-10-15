[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic, OpenAI"]

(* Every corpus file must convert to exactly the VCD libfst made of it. *)

let first_difference a b =
  let n = min (String.length a) (String.length b) in
  let rec go i line =
    if i < n && a.[i] = b.[i] then go (i + 1) (if a.[i] = '\n' then line + 1 else line)
    else (i, line)
  in
  go 0 1

let line_at s i =
  let start =
    match String.rindex_from_opt s (max 0 (min i (String.length s - 1))) '\n' with
    | Some j when j < i -> j + 1
    | _ -> 0
  in
  let stop =
    match String.index_from_opt s start '\n' with Some j -> j | None -> String.length s
  in
  String.sub s start (stop - start)

let () =
  let dir = Sys.argv.(1) in
  let checked = ref 0 and failed = ref 0 in
  List.iter
    (fun f ->
      let base = Filename.concat dir (Filename.chop_suffix f ".fst") in
      List.iter
        (fun (extensions, suffix) ->
          let golden = base ^ suffix in
          if Sys.file_exists golden then begin
            incr checked;
            let expected = Common.read_file golden in
            let actual =
              Result.bind
                (Fst.open_file (Filename.concat dir f))
                (fun t ->
                  Fun.protect
                    ~finally:(fun () -> Fst.close t)
                    (fun () -> Fst.Vcd.to_string ~extensions t))
            in
            match actual with
            | Error e ->
                incr failed;
                Printf.printf "FAIL %s: %s\n" golden (Fst.Error.to_string e)
            | Ok actual when actual <> expected ->
                incr failed;
                let i, line = first_difference expected actual in
                Printf.printf
                  "FAIL %s: line %d differs (%d vs %d bytes)\n  expected: %S\n  actual:   %S\n"
                  golden line (String.length expected) (String.length actual) (line_at expected i)
                  (line_at actual i)
            | Ok _ -> ()
          end)
        [ (false, ".vcd"); (true, ".ext.vcd") ])
    (Common.files dir ".fst");
  (* Every block type and compression must stay exercised: a corpus change that drops one
     (the double-LZ4 hierarchy needs over 4 MiB of it) fails here. *)
  let seen = Hashtbl.create 16 in
  List.iter
    (fun f ->
      let path = Filename.concat dir f in
      if (Common.read_file path).[0] = '\xfe' then Hashtbl.replace seen "ZWRAPPER" ();
      match Fst.open_file path with
      | Error _ -> ()
      | Ok t ->
          List.iter (fun b -> Hashtbl.replace seen (string_of_int b) ()) (Fst.Private.block_types t);
          (match Fst.Private.packtypes t with
          | Ok l -> List.iter (fun c -> Hashtbl.replace seen (String.make 1 c) ()) l
          | Error _ -> ());
          Fst.close t)
    (Common.files dir ".fst");
  let required = [ "1"; "5"; "8"; "4"; "6"; "7"; "ZWRAPPER"; "Z"; "F"; "4" ] in
  let missing = List.filter (fun k -> not (Hashtbl.mem seen k)) required in
  List.iter (Printf.printf "FAIL corpus lacks %s\n") missing;
  Printf.printf "corpus: %d conversions, %d failed\n" !checked !failed;
  if !checked = 0 || !failed > 0 || missing <> [] then exit 1
