[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic"]

(* The events of Fst.Vcd carry exactly the VCD that Fst.Vcd.to_string prints: a reference printer
   (test/oracle) renders them and the text must be equal, on every corpus file, with and without
   extensions, pushed or pulled, packed or not. Also the structure of the definitions, and what a
   read that resumes at a block passes. *)

module F = Fst_fixture
module V = Fst.Vcd
module E = Vcd_events

let failed = ref 0
let renders = ref 0

let fail fmt =
  Printf.ksprintf
    (fun s ->
      incr failed;
      Printf.printf "FAIL %s\n%!" s)
    fmt

let show_error e = Fst.Error.to_string e

let first_difference a b =
  let n = min (String.length a) (String.length b) in
  let rec go i = if i < n && a.[i] = b.[i] then go (i + 1) else i in
  go 0

(* The text of every way of rendering the events is [Vcd.to_string]'s. *)
let check_render name t =
  List.iter
    (fun extensions ->
      let tag = Printf.sprintf "%s%s" name (if extensions then " -e" else "") in
      match V.to_string ~extensions t with
      | Error e -> (
          (* A damaged file: the events end in the same error. *)
          match E.render ~extensions ~mode:Push t with
          | Error e' ->
              if Fst.Error.kind e <> Fst.Error.kind e' then
                fail "%s: events error %s, to_string %s" tag (show_error e') (show_error e)
          | Ok _ -> fail "%s: events succeed, to_string fails: %s" tag (show_error e))
      | Ok expected ->
          List.iter
            (fun (what, mode, packed, select) ->
              incr renders;
              match E.render ~extensions ~packed ~mode ?select t with
              | Error e -> fail "%s %s: %s" tag what (show_error e)
              | Ok actual ->
                  if actual <> expected then
                    fail "%s %s: differs at byte %d (%d vs %d bytes)" tag what
                      (first_difference expected actual)
                      (String.length expected) (String.length actual)
              | exception Failure m -> fail "%s %s: %s" tag what m)
            [
              ("push", E.Push, false, None);
              ("pull", E.Pull, false, None);
              ("push packed", E.Push, true, None);
              ("pull packed", E.Pull, true, None);
              ("push select all", E.Push, false, Some (fun _ -> true));
            ])
    [ false; true ]

let check_definitions name t =
  let hier = Fst.fold_hierarchy t ~init:[] ~f:(fun acc e -> e :: acc) |> Result.map List.rev in
  List.iter
    (fun extensions ->
      match (E.definitions ~extensions t, hier, Fst.signals t) with
      | Ok defs, Ok hier, Ok signals ->
          let tag = Printf.sprintf "%s%s: definitions" name (if extensions then " -e" else "") in
          let rec last = function [ x ] -> Some x | _ :: l -> last l | [] -> None in
          if last defs <> Some V.End_definitions then fail "%s: not ended by End_definitions" tag;
          if List.length (List.filter (( = ) V.End_definitions) defs) <> 1 then
            fail "%s: End_definitions is not unique" tag;
          let vars =
            List.filter_map
              (function
                | V.Var { handle; alias; name; id; size; kind; var_type } ->
                    Some (handle, alias, name, id, size, kind, var_type)
                | _ -> None)
              defs
          in
          let hvars =
            List.filter_map
              (function
                | Fst.Hier.Var { handle; alias; name; length; var_type; _ } ->
                    Some (handle, alias, name, length, var_type)
                | _ -> None)
              hier
          in
          if List.length vars <> List.length hvars then fail "%s: variable count" tag
          else
            List.iter2
              (fun (handle, alias, name, id, size, kind, var_type) (h', a', n', length, vt) ->
                if handle <> h' || alias <> a' || name <> n' then fail "%s: %s" tag n';
                if id <> V.id handle then fail "%s: id of %s" tag n';
                if size <> V.var_length ~var_type:vt ~length then fail "%s: size of %s" tag n';
                if Fst.Hier.var_type_name vt <> Some var_type then fail "%s: type of %s" tag n';
                let expected =
                  if Fst.Hier.is_real vt then Fst.Real
                  else if length = 0 then Fst.String
                  else if vt = 18 then Fst.Port
                  else if length = 1 then Fst.Bit
                  else Fst.Vector
                in
                if kind <> expected then fail "%s: kind of %s" tag n'
                else if (not alias) && signals.(handle - 1) <> (kind, length) then
                  fail "%s: %s differs from signals" tag n')
              vars hvars;
          let count p l = List.length (List.filter p l) in
          let scopes = count (function V.Scope _ -> true | _ -> false) defs in
          if scopes <> count (function Fst.Hier.Scope _ -> true | _ -> false) hier then
            fail "%s: scope count" tag;
          if
            count (function V.Upscope -> true | _ -> false) defs
            <> count (function Fst.Hier.Upscope -> true | _ -> false) hier
          then fail "%s: upscope count" tag;
          let attrs = count (function V.Comment _ | Attr_begin _ -> true | _ -> false) defs in
          let hattrs = count (function Fst.Hier.Attr_begin _ -> true | _ -> false) hier in
          if attrs <> if extensions then hattrs else 0 then fail "%s: attribute count" tag
      | Error _, _, _ | _, Error _, _ | _, _, Error _ -> ())
    [ false; true ]

let reader_at_calls t k =
  let l = ref [] in
  let add fmt = Printf.ksprintf (fun s -> l := s :: !l) fmt in
  let sink =
    {
      Fst.time = (fun t -> add "#%Lu" t);
      frame = (fun t -> add "frame %Lu" t);
      bit = (fun h c -> add "%c %d" c h);
      vector = (fun h s off len -> add "b%s %d" (Bytes.sub_string s off len) h);
      real = (fun h d -> add "r%Lx %d" (Int64.bits_of_float d) h);
      varlen = (fun h s off len -> add "s%S %d" (Bytes.sub_string s off len) h);
    }
  in
  Result.bind (Fst.reader_at t sink ~block:k) (fun r ->
      let rec go = function
        | Ok true -> go (Fst.step r)
        | Ok false -> Ok (List.rev !l)
        | Error e -> Error e
      in
      go (Fst.step r))

(* The calls of a resumed read are those of [reader_at], and each blackout whose time is the
   first or a later one follows the timestamp it is at. *)
let check_resumed name t =
  match Fst.blocks t with
  | Error _ -> ()
  | Ok bs ->
      let blackouts = Array.of_list (Fst.blackouts t) in
      for k = 1 to Array.length bs - 1 do
        match (E.resumed ~block:k t, reader_at_calls t k) with
        | Ok got, Ok calls ->
            let cur = ref 0 and first = ref true in
            let expected =
              List.concat_map
                (fun c ->
                  if c.[0] <> '#' then [ c ]
                  else begin
                    let time = Int64.of_string ("0u" ^ String.sub c 1 (String.length c - 1)) in
                    if !first then begin
                      first := false;
                      while
                        !cur < Array.length blackouts
                        && Int64.unsigned_compare (fst blackouts.(!cur)) time < 0
                      do
                        incr cur
                      done
                    end;
                    if !cur < Array.length blackouts && Int64.equal (fst blackouts.(!cur)) time then begin
                      let on = snd blackouts.(!cur) in
                      incr cur;
                      [ c; (if on then "dumpon" else "dumpoff") ]
                    end
                    else [ c ]
                  end)
                calls
            in
            incr renders;
            if got <> expected then fail "%s: resumed at block %d differs from reader_at" name k
        | Error e, _ | _, Error e -> fail "%s: resumed at block %d: %s" name k (show_error e)
        | exception Failure m -> fail "%s: resumed at block %d: %s" name k m
      done

let check_file name t =
  check_render name t;
  check_definitions name t;
  check_resumed name t

(* A file with what the corpus may not have: non-finite reals, a port, a string with bytes to
   escape, an empty scope name, a late alias. *)
let fixtures =
  [
    ( "fixture specials",
      {
        (F.make
           [
             F.signal "r" F.Real;
             F.signal "p" (F.Vector 8);
             F.signal "s" F.String;
             F.signal "b" F.Bit;
             F.signal ~alias:4 "b2" F.Bit;
           ]
           [
             F.block ~start:0L
               ~frame:[ (1, F.R nan); (2, F.V "10100101") ]
               ~times:[ 3L; 7L ]
               [
                 (0, 1, F.R infinity);
                 (0, 3, F.S "a\n\000\"\\?\xff");
                 (1, 1, F.R neg_infinity);
                 (1, 2, F.V "1x0z1010");
                 (1, 4, F.B 'z');
               ];
           ])
        with
        scope = "";
      } );
  ]

let fixture_files () =
  List.map
    (fun (name, spec) ->
      let spec = F.set_var_type spec ~signal:2 18 in
      (name, spec))
    fixtures

let raising () =
  let t = Result.get_ok (Fst.of_string (F.build (snd (List.hd (fixture_files ()))))) in
  let calls = ref 0 in
  let stop =
    {
      V.ignore_sink with
      definition =
        (fun _ ->
          incr calls;
          raise Exit);
    }
  in
  (match V.events t stop with
  | _ -> fail "a definition sink that raises: no exception"
  | exception Exit -> ()
  | exception e -> fail "a definition sink that raises: %s" (Printexc.to_string e));
  if !calls <> 1 then fail "the sink was called %d times after raising" !calls;
  (match V.reader t stop with
  | Error e -> fail "reader: %s" (show_error e)
  | Ok r ->
      (match V.step r with
      | _ -> fail "step: no exception"
      | exception Exit -> ()
      | exception e -> fail "step: %s" (Printexc.to_string e));
      if V.step r <> Ok false then fail "a step after the sink raised");
  let stop = { V.ignore_sink with time = (fun _ -> raise Exit) } in
  (match V.events t stop with
  | _ -> fail "a time sink that raises: no exception"
  | exception Exit -> ()
  | exception e -> fail "a time sink that raises: %s" (Printexc.to_string e));
  (match E.render ~mode:Push t with
  | Ok _ -> ()
  | _ -> fail "the handle is not usable after a sink raised");
  (match V.reader t V.ignore_sink with
  | Ok r -> (
      Fst.close t;
      match V.step r with
      | Error e when Fst.Error.kind e = Fst.Error.Io -> ()
      | _ -> fail "step after close is not Io")
  | Error e -> fail "reader: %s" (show_error e));
  let called = ref 0 in
  let counting = { V.ignore_sink with definition = (fun _ -> incr called) } in
  (match V.events t counting with
  | Error e when Fst.Error.kind e = Fst.Error.Io -> ()
  | _ -> fail "events on a closed file is not Io");
  (match V.reader t counting with
  | Error e when Fst.Error.kind e = Fst.Error.Io -> ()
  | _ -> fail "reader on a closed file is not Io");
  if !called <> 0 then fail "a closed file passed %d definitions" !called;
  let t = Result.get_ok (Fst.of_string (F.build (snd (List.hd (fixture_files ()))))) in
  match V.reader ~block:1 t V.ignore_sink with
  | _ -> fail "reader ~block past the end: no Invalid_argument"
  | exception Invalid_argument _ -> ()

let () =
  let dir = Sys.argv.(1) in
  let files = Common.files dir ".fst" in
  List.iter
    (fun f ->
      match Fst.open_file (Filename.concat dir f) with
      | Error _ -> ()
      | Ok t ->
          check_file f t;
          Fst.close t)
    files;
  List.iter
    (fun (name, spec) ->
      match Fst.of_string (F.build spec) with
      | Error e -> fail "%s: %s" name (show_error e)
      | Ok t ->
          check_file name t;
          (* The fixture's VCD must really hold what it was built to test. *)
          (match V.to_string t with
          | Ok vcd ->
              List.iter
                (fun sub ->
                  let n = String.length sub in
                  let rec go i =
                    i + n <= String.length vcd && (String.sub vcd i n = sub || go (i + 1))
                  in
                  if not (go 0) then fail "%s lacks %S" name sub)
                [ "$unnamed_scope_"; "rnan "; "rinf "; "r-inf "; "p1x0z1010"; "p10100101"; "s" ]
          | Error e -> fail "%s: %s" name (show_error e));
          Fst.close t)
    (fixture_files ());
  raising ();
  Printf.printf "events: %d files, %d renderings equal Vcd.to_string, %d failed\n"
    (List.length files + List.length fixtures)
    !renders !failed;
  if !failed > 0 || !renders = 0 then exit 1
