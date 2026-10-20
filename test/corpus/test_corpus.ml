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

(* Every call the sink receives, and whether it came from inside [Fst.step]. *)
let recording_sink b ~stepping =
  let add fmt =
    Printf.ksprintf
      (fun s ->
        if not !stepping then Buffer.add_string b "OUTSIDE ";
        Buffer.add_string b s)
      fmt
  in
  {
    Fst.time = (fun t -> add "#%Lu\n" t);
    frame = (fun t -> add "frame %Lu\n" t);
    bit = (fun h c -> add "%c %d\n" c h);
    vector = (fun h s off len -> add "b%s %d\n" (Bytes.sub_string s off len) h);
    real = (fun h d -> add "r%Lx %d\n" (Int64.bits_of_float d) h);
    varlen = (fun h s off len -> add "s%S %d\n" (Bytes.sub_string s off len) h);
  }

(* Stepping must make the calls and have the outcome of [iter_changes]; returns the number of
   steps. *)
let check_steps t =
  let all = Buffer.create 4096 and stepped = Buffer.create 4096 and stepping = ref true in
  let expected = Fst.iter_changes t (recording_sink all ~stepping) in
  let rec go n r =
    stepping := true;
    let s = Fst.step r in
    stepping := false;
    match s with
    | Ok true -> go (n + 1) r
    | Ok false -> (Ok (), n, Fst.step r = Ok false)
    | Error e -> (Error e, n, Fst.step r = Ok false)
  in
  stepping := false;
  let actual, n, ended =
    match Fst.reader t (recording_sink stepped ~stepping) with
    | Ok r -> go 0 r
    | Error e -> (Error e, 0, true)
  in
  if actual <> expected then Error "outcome differs from iter_changes"
  else if not ended then Error "a step after the last one"
  else if Buffer.contents all <> Buffer.contents stepped then Error "calls differ from iter_changes"
  else Ok n

(* The calls, each with its handle; 0 for [time] and [frame]. *)
let tagged_sink calls =
  let add h fmt = Printf.ksprintf (fun s -> calls := (h, s) :: !calls) fmt in
  {
    Fst.time = (fun t -> add 0 "#%Lu" t);
    frame = (fun t -> add 0 "frame %Lu" t);
    bit = (fun h c -> add h "%c %d" c h);
    vector = (fun h s off len -> add h "b%s %d" (Bytes.sub_string s off len) h);
    real = (fun h d -> add h "r%Lx %d" (Int64.bits_of_float d) h);
    varlen = (fun h s off len -> add h "s%S %d" (Bytes.sub_string s off len) h);
  }

let calls ?select t =
  let c = ref [] in
  let r = Fst.iter_changes ?select t (tagged_sink c) in
  (r, List.rev !c)

let masks n =
  let random seed =
    let st = Random.State.make [| seed |] in
    let a = Array.init n (fun _ -> Random.State.bool st) in
    fun h -> a.(h - 1)
  in
  [
    ("all", fun _ -> true);
    ("none", fun _ -> false);
    ("first", fun h -> h = 1);
    ("last", fun h -> h = n);
    ("odd", fun h -> h land 1 = 1);
  ]
  @ List.map (fun s -> (Printf.sprintf "random %d" s, random s)) [ 1; 2; 3 ]
  @
  (* One handle at a time also selects every dynamic alias without its target. *)
  if n <= 64 then List.init n (fun i -> (Printf.sprintf "only %d" (i + 1), fun h -> h = i + 1))
  else []

(* With [select], the calls are those without it minus the unselected handles' ones; stepping
   makes the same calls, and the first step, not [reader], calls [select] once per handle. *)
let check_select t ~full (name, select) =
  let n = Int64.to_int (Fst.header t).max_handle in
  let expected = (fst full, List.filter (fun (h, _) -> h = 0 || select h) (snd full)) in
  let asked = Hashtbl.create 64 in
  let counted h =
    Hashtbl.replace asked h (1 + Option.value (Hashtbl.find_opt asked h) ~default:0);
    select h
  in
  let stepped = ref [] in
  let outcome, before =
    match Fst.reader ~select:counted t (tagged_sink stepped) with
    | Error e -> (Error e, 0)
    | Ok r ->
        let before = Hashtbl.length asked in
        let rec go () =
          match Fst.step r with Ok true -> go () | Ok false -> Ok () | Error e -> Error e
        in
        (go (), before)
  in
  if calls ~select t <> expected then Error (name ^ ": not the unselected calls, filtered")
  else if (outcome, List.rev !stepped) <> expected then Error (name ^ ": stepping differs")
  else if before <> 0 then Error (name ^ ": select called before the first step")
  else if Hashtbl.length asked <> n || not (Hashtbl.fold (fun _ k ok -> ok && k = 1) asked true)
  then
    Error
      (Printf.sprintf "%s: select called for %d of %d handles, or more than once" name
         (Hashtbl.length asked) n)
  else Ok ()

(* A packed vector, expanded to digits, is the call [vector] would have made; the others are
   unchanged. Returns the number of packed calls. *)
let check_packed t ~full (name, select) =
  let expected = (fst full, List.filter (fun (h, _) -> h = 0 || select h) (snd full)) in
  let packed_calls = ref 0 in
  let run make =
    let c = ref [] in
    let packed h b off width =
      incr packed_calls;
      let digits =
        String.init width (fun j ->
            if (Char.code (Bytes.get b (off + (j lsr 3))) lsr (7 - (j land 7))) land 1 = 1 then '1'
            else '0')
      in
      c := (h, Printf.sprintf "b%s %d" digits h) :: !c
    in
    let r = make ~packed (tagged_sink c) in
    (r, List.rev !c)
  in
  let iterated = run (fun ~packed sink -> Fst.iter_changes ~select ~packed t sink) in
  let stepped =
    run (fun ~packed sink ->
        match Fst.reader ~select ~packed t sink with
        | Error e -> Error e
        | Ok r ->
            let rec go () =
              match Fst.step r with Ok true -> go () | Ok false -> Ok () | Error e -> Error e
            in
            go ())
  in
  if iterated <> expected then Error (name ^ ": packed iter_changes differs")
  else if stepped <> expected then Error (name ^ ": packed stepping differs")
  else Ok !packed_calls

let is_time (h, s) = h = 0 && String.length s > 0 && s.[0] = '#'

(* The calls of each step of a stepping reader. *)
let per_step r calls =
  let rec go acc =
    calls := [];
    match Fst.step r with
    | Ok true -> go (List.rev !calls :: acc)
    | Ok false -> Ok (Array.of_list (List.rev acc))
    | Error e -> Error e
  in
  go []

(* What a handle holds after [steps]: the last call that names it. *)
let state_of steps =
  let t = Hashtbl.create 64 in
  Array.iter (List.iter (fun (h, s) -> if h <> 0 then Hashtbl.replace t h s)) steps;
  t

(* Seeking to each block of [t]: the calls from [reader_at k] are a frame holding what the replay
   of the earlier blocks holds, then the later blocks' calls. Returns (seeks that recovered a
   string, seeks inside a timestamp). *)
let check_seek t (name, select) =
  let fail fmt = Printf.ksprintf (fun s -> Error (name ^ ": " ^ s)) fmt in
  let calls = ref [] in
  match Fst.reader ~select t (tagged_sink calls) with
  | Error e -> fail "reader: %s" (Fst.Error.to_string e)
  | Ok r -> (
      match per_step r calls with
      | Error e -> fail "replay: %s" (Fst.Error.to_string e)
      | Ok steps -> (
          let nb = Array.length steps in
          match Fst.blocks t with
          | Error e -> fail "blocks: %s" (Fst.Error.to_string e)
          | Ok bs when Array.length bs <> nb -> fail "%d blocks, %d steps" (Array.length bs) nb
          | Ok bs ->
              let strings = ref 0 and spans = ref 0 and result = ref (Ok ()) in
              let bad fmt =
                Printf.ksprintf (fun s -> if !result = Ok () then result := fail "%s" s) fmt
              in
              for k = 0 to nb - 1 do
                let seek = ref [] in
                (match Fst.reader_at ~select t (tagged_sink seek) ~block:k with
                | Error e -> bad "reader_at %d: %s" k (Fst.Error.to_string e)
                | Ok r -> (
                    match per_step r seek with
                    | Error e -> bad "seeking to %d: %s" k (Fst.Error.to_string e)
                    | Ok got ->
                        let got = List.concat (Array.to_list got) in
                        let later = List.concat (Array.to_list (Array.sub steps k (nb - k))) in
                        if k = 0 then (if got <> later then bad "reader_at 0 differs from reader")
                        else begin
                          let is_frame (h, s) =
                            h = 0 && String.length s > 5 && String.sub s 0 5 = "frame"
                          in
                          (match got with
                          | c :: _ when is_frame c ->
                              if snd c <> Printf.sprintf "frame %Lu" bs.(k).Fst.start_time then
                                bad "block %d: %s, starts at %Lu" k (snd c) bs.(k).start_time
                          | _ -> bad "block %d: no frame first" k);
                          let rec split acc = function
                            | c :: rest when not (is_time c) -> split (c :: acc) rest
                            | rest -> (List.rev acc, rest)
                          in
                          let head, tail = split [] (List.tl got) in
                          let tail_expected =
                            (* The replay merges a timestamp that spans the boundary and does not
                               announce it again. *)
                            match tail with
                            | t0 :: tl
                              when is_time t0 && match later with [] -> true | l0 :: _ -> l0 <> t0
                              ->
                                incr spans;
                                tl
                            | _ -> tail
                          in
                          if tail_expected <> later then begin
                            let show l =
                              String.concat "|" (List.filteri (fun i _ -> i < 4) (List.map snd l))
                            in
                            bad "block %d: later calls differ: got %s, want %s (%d vs %d)" k
                              (show tail_expected) (show later) (List.length tail_expected)
                              (List.length later)
                          end;
                          let held = state_of (Array.sub steps 0 k) in
                          let seen = Hashtbl.create 64 in
                          List.iter (fun (h, s) -> Hashtbl.replace seen h s) head;
                          Hashtbl.iter
                            (fun h s ->
                              if Hashtbl.find_opt seen h <> Some s then
                                bad "block %d: handle %d is %S, the replay has %S" k h
                                  (Option.value (Hashtbl.find_opt seen h) ~default:"-")
                                  s)
                            held;
                          if List.exists (fun (_, s) -> s.[0] = 's') head then incr strings
                        end));
                match Fst.aligned_block t k with
                | Error e -> bad "aligned_block %d: %s" k (Fst.Error.to_string e)
                | Ok j ->
                    if j < 0 || j > k then bad "aligned_block %d = %d" k j
                    else begin
                      (match steps.(k) with
                      | c :: _ when k > 0 && not (is_time c) ->
                          if j >= k then bad "aligned_block %d = %d inside a timestamp" k j
                      | _ -> ());
                      if j > 0 then
                        match steps.(j) with
                        | c :: _ when not (is_time c) ->
                            bad "aligned_block %d = %d, which starts inside a timestamp" k j
                        | _ -> ()
                    end
              done;
              (match Fst.reader_at ~select t (tagged_sink (ref [])) ~block:nb with
              | Ok _ -> bad "reader_at %d: no error" nb
              | Error _ -> ());
              (match Fst.reader_at ~select t (tagged_sink (ref [])) ~block:(-1) with
              | Ok _ -> bad "reader_at -1: no error"
              | Error _ -> ());
              (match Fst.aligned_block t nb with
              | Ok _ -> bad "aligned_block %d: no error" nb
              | Error _ -> ());
              Result.map (fun () -> (!strings, !spans)) !result))

(* The index reads a bounded number of bytes per block and none of the block's payload. *)
let check_index path =
  let data = Common.read_file path in
  if data.[0] = '\xfe' then Ok () (* inflated whole when opened *)
  else
    let count = ref 0 in
    let read pos len =
      let p = Int64.to_int pos in
      if p >= String.length data then ""
      else begin
        let s = String.sub data p (min len (String.length data - p)) in
        count := !count + String.length s;
        s
      end
    in
    match Fst.Private.of_reader ~length:(Int64.of_int (String.length data)) ~read with
    | Error _ -> Ok ()
    | Ok t -> (
        count := 0;
        match Fst.blocks t with
        | Error e -> Error (Fst.Error.to_string e)
        | Ok bs ->
            if !count <> 16 * Array.length bs then
              Error (Printf.sprintf "%d bytes read for %d blocks" !count (Array.length bs))
            else Ok ())

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
  let multi_step = ref false in
  List.iter
    (fun f ->
      match Fst.open_file (Filename.concat dir f) with
      | Error _ -> ()
      | Ok t ->
          (match check_steps t with
          | Ok n -> if n > 1 then multi_step := true
          | Error e ->
              incr failed;
              Printf.printf "FAIL %s: %s\n" f e);
          Fst.close t)
    (Common.files dir ".fst");
  if !multi_step then Hashtbl.replace seen "multi-step" ();
  List.iter
    (fun f ->
      match Fst.open_file (Filename.concat dir f) with
      | Error _ -> ()
      | Ok t ->
          let full = calls t in
          List.iter
            (fun m ->
              match check_select t ~full m with
              | Ok () -> ()
              | Error e ->
                  incr failed;
                  Printf.printf "FAIL %s: %s\n" f e)
            (masks (Int64.to_int (Fst.header t).max_handle));
          Fst.close t)
    (Common.files dir ".fst");
  List.iter
    (fun f ->
      match Fst.open_file (Filename.concat dir f) with
      | Error _ -> ()
      | Ok t ->
          let full = calls t in
          List.iter
            (fun m ->
              match check_packed t ~full m with
              | Ok n -> if n > 0 then Hashtbl.replace seen "packed" ()
              | Error e ->
                  incr failed;
                  Printf.printf "FAIL %s: %s\n" f e)
            (masks (Int64.to_int (Fst.header t).max_handle));
          Fst.close t)
    (Common.files dir ".fst");
  List.iter
    (fun f ->
      let path = Filename.concat dir f in
      (match check_index path with
      | Ok () -> ()
      | Error e ->
          incr failed;
          Printf.printf "FAIL %s: index: %s\n" f e);
      match Fst.open_file path with
      | Error _ -> ()
      | Ok t ->
          List.iter
            (fun m ->
              match check_seek t m with
              | Ok (strings, spans) ->
                  if strings > 0 then Hashtbl.replace seen "seek-strings" ();
                  if spans > 0 then Hashtbl.replace seen "seek-spanning" ()
              | Error e ->
                  incr failed;
                  Printf.printf "FAIL %s: %s\n" f e)
            (masks (Int64.to_int (Fst.header t).max_handle));
          Fst.close t)
    (Common.files dir ".fst");
  let required =
    [
      "1";
      "5";
      "8";
      "4";
      "6";
      "7";
      "ZWRAPPER";
      "Z";
      "F";
      "4";
      "multi-step";
      "packed";
      "seek-strings";
      "seek-spanning";
    ]
  in
  let missing = List.filter (fun k -> not (Hashtbl.mem seen k)) required in
  List.iter (Printf.printf "FAIL corpus lacks %s\n") missing;
  Printf.printf "corpus: %d conversions, %d failed\n" !checked !failed;
  if !checked = 0 || !failed > 0 || missing <> [] then exit 1
