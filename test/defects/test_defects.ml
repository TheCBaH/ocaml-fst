[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic"]

module F = Fst_fixture

let failures = ref 0

let check name ok =
  if not ok then begin
    incr failures;
    Printf.printf "FAIL %s\n%!" name
  end

let sink =
  {
    Fst.time = ignore;
    frame = ignore;
    bit = (fun _ _ -> ());
    vector = (fun _ _ _ _ -> ());
    real = (fun _ _ -> ());
    varlen = (fun _ _ _ _ -> ());
  }

(* [f] returns an [Error] of [kind]; [Ok] or an exception fails. *)
let expect name kind f =
  match f () with
  | Error e -> check (Printf.sprintf "%s: %s" name (Fst.Error.to_string e)) (Fst.Error.kind e = kind)
  | Ok () -> check (name ^ ": Ok") false
  | exception e -> check (name ^ ": raised " ^ Printexc.to_string e) false

let open_ s =
  match Fst.of_string (F.build s) with Ok t -> t | Error e -> failwith (Fst.Error.to_string e)

let drain r =
  let rec go () =
    match Fst.step r with Ok true -> go () | Ok false -> Ok () | Error e -> Error e
  in
  go ()

(* Every entry point that decodes value changes. *)
let entry_points name kind spec =
  let t = open_ spec in
  expect (name ^ " iter_changes") kind (fun () -> Fst.iter_changes t sink);
  expect (name ^ " Vcd.to_string") kind (fun () -> Result.map ignore (Fst.Vcd.to_string t));
  expect (name ^ " reader/step") kind (fun () -> Result.bind (Fst.reader t sink) drain);
  Fst.close t

(* D2: a real whose declared length is not 8. *)
let () =
  let spec =
    F.make
      [ F.signal "a" F.Bit; F.signal "r" F.Real ]
      [
        F.block ~start:5L
          ~frame:[ (2, F.R 1.5) ]
          ~times:[ 10L; 20L ]
          [ (0, 1, F.B '1'); (1, 2, F.R 2.5) ];
      ]
  in
  let t = open_ spec in
  check "real of 8 bytes is fine" (Fst.iter_changes t sink = Ok ());
  Fst.close t;
  List.iter
    (fun n ->
      let bad = F.set_length spec ~signal:2 n in
      entry_points (Printf.sprintf "real length %d" n) Fst.Error.Malformed bad;
      let t = open_ bad in
      (match (Fst.signals t, Fst.signals t) with
      | (Error e as a), b ->
          check "signals: the same error each time" (a == b && Fst.Error.kind e = Malformed)
      | Ok _, _ -> check "signals: Ok" false);
      Fst.close t)
    [ 0; 4; 7; 9; 16 ]

(* D3: a chain declares a huge size, which must be refused before any buffer is allocated. *)
let () =
  let spec =
    F.make
      [ F.signal "a" F.Bit; F.signal "v" (F.Vector 8) ]
      [ F.block ~times:[ 10L; 20L ] [ (0, 1, F.B '1'); (0, 2, F.V "10100101") ] ]
  in
  let dest = min (1 lsl 29) (Sys.max_string_length - 1024) in
  let t = open_ (F.set_chain_dest spec ~block:0 ~handle:2 dest) in
  let before = Gc.allocated_bytes () in
  let r = Fst.iter_changes t sink in
  let allocated = Gc.allocated_bytes () -. before in
  (match r with
  | Error e -> check "huge dest: Limit_exceeded" (Fst.Error.kind e = Fst.Error.Limit_exceeded)
  | Ok () -> check "huge dest: Ok" false);
  check (Printf.sprintf "huge dest allocated %.0f bytes" allocated) (allocated < 1e6);
  Fst.close t

(* N3 / D1: an invalid time-table trailer is an error from every entry point. *)
let () =
  let spec =
    F.make
      [ F.signal "a" F.Bit; F.signal "s" F.String ]
      [
        F.block ~times:[ 10L; 20L ] [ (0, 1, F.B '1'); (1, 2, F.S "x") ];
        F.block ~times:[ 20L; 30L ] [ (0, 1, F.B '0'); (1, 2, F.S "y") ];
        F.block ~times:[ 40L; 50L ] [ (0, 1, F.B '1') ];
      ]
  in
  let t = open_ spec in
  check "valid file" (Fst.iter_changes t sink = Ok ());
  Fst.close t;
  let at_block name f =
    List.iter
      (fun k ->
        let bad = f (F.set_trailer spec ~block:k) in
        let name = Printf.sprintf "%s in block %d" name k in
        entry_points name Fst.Error.Malformed bad;
        let t = open_ bad in
        (match Fst.blocks t with
        | Ok bs -> check (name ^ ": blocks lists all") (Array.length bs = 3)
        | Error _ -> check (name ^ ": blocks failed") false);
        expect (name ^ " aligned_block") Fst.Error.Malformed (fun () ->
            Result.map ignore (Fst.aligned_block t (max k 1)));
        (* The reader at block 2 replays the string variable of the earlier blocks. *)
        expect (name ^ " reader_at 2") Fst.Error.Malformed (fun () ->
            Result.bind (Fst.reader_at t sink ~block:2) drain);
        Fst.close t)
      [ 0; 1 ]
  in
  at_block "clen beyond the block" (fun set -> set ~uclen:2 ~clen:100000 ~nitems:2);
  at_block "negative-sized clen" (fun set -> set ~uclen:2 ~clen:(-1) ~nitems:2);
  at_block "more items than bytes" (fun set -> set ~uclen:2 ~clen:2 ~nitems:3)

(* N2 / D3: a block whose chains are inconsistent is an error naming the block, from every entry
   point, and not silently skipped. *)
let () =
  let spec =
    F.make
      [ F.signal "a" F.Bit; F.signal "s" F.String ]
      [
        F.block ~times:[ 10L; 20L ] [ (0, 1, F.B '1'); (1, 2, F.S "x") ];
        F.block ~times:[ 20L; 30L ] [ (0, 1, F.B '0'); (1, 2, F.S (String.make 100 'y')) ];
        F.block ~times:[ 40L; 50L ] [ (0, 1, F.B '1') ];
      ]
  in
  let t = open_ spec in
  check "valid file" (Fst.iter_changes t sink = Ok ());
  Fst.close t;
  let named name f =
    match f () with
    | Error e ->
        check (name ^ ": Malformed") (Fst.Error.kind e = Fst.Error.Malformed);
        let m = Fst.Error.message e in
        let rec has i = i + 7 <= String.length m && (String.sub m i 7 = "block 1" || has (i + 1)) in
        check (name ^ ": names block 1: " ^ m) (has 0)
    | Ok () -> check (name ^ ": Ok") false
    | exception e -> check (name ^ ": raised " ^ Printexc.to_string e) false
  in
  let skipped name bad =
    let t = open_ bad in
    named (name ^ " iter_changes") (fun () -> Fst.iter_changes t sink);
    named (name ^ " Vcd.to_string") (fun () -> Result.map ignore (Fst.Vcd.to_string t));
    named (name ^ " reader/step") (fun () -> Result.bind (Fst.reader t sink) drain);
    List.iter
      (fun k ->
        named (Printf.sprintf "%s reader_at %d" name k) (fun () ->
            Result.bind (Fst.reader_at t sink ~block:k) drain))
      [ 1; 2 ];
    named (name ^ " aligned_block") (fun () -> Result.map ignore (Fst.aligned_block t 1));
    Fst.close t
  in
  skipped "chains over the declared memory" (F.set_mem_required spec ~block:1 0L);
  skipped "inconsistent chain table" (F.set_chain_table spec ~block:1 "\000")

(* D7: an alias names a handle that exists. *)
let () =
  let spec =
    F.make
      [ F.signal "a" F.Bit; F.signal "v" (F.Vector 4); F.signal ~alias:1 "a2" F.Bit ]
      [ F.block ~times:[ 10L ] [ (0, 1, F.B '1') ] ]
  in
  let fold t = Fst.fold_hierarchy t ~init:0 ~f:(fun n _ -> n + 1) in
  let t = open_ spec in
  check "valid alias" (Result.is_ok (fold t) && Result.is_ok (Fst.Vcd.to_string t));
  Fst.close t;
  List.iter
    (fun target ->
      let t = open_ (F.set_alias_target spec ~signal:3 target) in
      let name = Printf.sprintf "alias to handle %d" target in
      expect (name ^ " fold_hierarchy") Fst.Error.Malformed (fun () -> Result.map ignore (fold t));
      expect (name ^ " Vcd.to_string") Fst.Error.Malformed (fun () ->
          Result.map ignore (Fst.Vcd.to_string t));
      check (name ^ ": values still read") (Fst.iter_changes t sink = Ok ());
      Fst.close t)
    [ 3; 4; 1000; 1 lsl 29 ]

(* D7: a gzip and an LZ4 hierarchy block in one file. libfst keeps the last; here neither is
   preferred. *)
let () =
  let spec = F.make [ F.signal "a" F.Bit ] [ F.block ~times:[ 10L ] [ (0, 1, F.B '1') ] ] in
  let section typ =
    let b = Bytes.make 17 '\000' in
    Bytes.set b 0 (Char.chr typ);
    Bytes.set_int64_be b 1 16L;
    Bytes.to_string b
  in
  let t = open_ spec in
  check "one hierarchy block" (Result.is_ok (Fst.Vcd.to_string t));
  Fst.close t;
  List.iter
    (fun typ ->
      match Fst.of_string (F.build spec ^ section typ) with
      | Error e -> check ("open: " ^ Fst.Error.to_string e) false
      | Ok t ->
          let name = Printf.sprintf "gzip and type %d hierarchy" typ in
          expect (name ^ " fold_hierarchy") Fst.Error.Unsupported (fun () ->
              Result.map ignore (Fst.fold_hierarchy t ~init:() ~f:(fun () _ -> ())));
          expect (name ^ " iter_changes") Fst.Error.Unsupported (fun () -> Fst.iter_changes t sink);
          expect (name ^ " Vcd.to_string") Fst.Error.Unsupported (fun () ->
              Result.map ignore (Fst.Vcd.to_string t));
          Fst.close t)
    [ 6; 7 ]

(* D7: an error's offset says which coordinate system it is in. *)
let () =
  let spec =
    F.make
      [ F.signal "a" F.Bit; F.signal "v" (F.Vector 4); F.signal ~alias:1 "a2" F.Bit ]
      [ F.block ~times:[ 10L; 20L ] [ (0, 1, F.B '1') ] ]
  in
  let offset name f ok =
    match f () with
    | Error e -> check (name ^ ": offset") (ok (Fst.Error.offset e))
    | Ok _ -> check (name ^ ": Ok") false
  in
  let t = open_ (F.set_alias_target spec ~signal:3 9) in
  offset "alias"
    (fun () -> Fst.fold_hierarchy t ~init:() ~f:(fun () _ -> ()))
    (fun o -> o = Some (`Hierarchy 19L));
  Fst.close t;
  let t = open_ (F.set_var_type (F.set_length spec ~signal:2 3) ~signal:2 3) in
  offset "real length" (fun () -> Fst.signals t) (fun o -> o = Some (`Hierarchy 13L));
  Fst.close t;
  let t = open_ (F.set_trailer spec ~block:0 ~uclen:2 ~clen:100000 ~nitems:2) in
  offset "trailer"
    (fun () -> Fst.iter_changes t sink)
    (function Some (`File _) -> true | _ -> false);
  Fst.close t;
  let t = open_ spec in
  Fst.close t;
  offset "closed" (fun () -> Fst.iter_changes t sink) (fun o -> o = None)

(* D7: the length of a compressed wrapper is checked against the platform's int, not wrapped. *)
let () =
  let head =
    let b = Bytes.make 17 '\000' in
    Bytes.set b 0 '\xfe';
    Bytes.set_int64_be b 1 100L;
    Bytes.set_int64_be b 9 100L;
    Bytes.to_string b
  in
  let wrapped length =
    let read pos len =
      let p = Int64.to_int pos in
      if p >= String.length head then "" else String.sub head p (min len (String.length head - p))
    in
    Fst.Private.of_reader ~length ~read ()
  in
  let big =
    (if Sys.int_size < 32 then [ 0x8000_0064L ] else []) @ [ Int64.add (Int64.of_int max_int) 100L ]
  in
  List.iter
    (fun length ->
      let name = Printf.sprintf "wrapper of %Ld bytes" length in
      match wrapped length with
      | Error e ->
          let m = Fst.Error.message e in
          let fits = ref false in
          String.iteri
            (fun i _ ->
              if i + 7 <= String.length m && String.sub m i 7 = "fit in " then fits := true)
            m;
          check
            (name ^ ": Limit_exceeded, does not fit: " ^ m)
            (Fst.Error.kind e = Limit_exceeded && !fits)
      | Ok _ -> check (name ^ ": Ok") false)
    big

(* D1: a sink's [Exit] reaches the caller. *)
let () =
  let spec =
    F.make
      [ F.signal "a" F.Bit ]
      [
        F.block ~times:[ 10L; 20L ] [ (0, 1, F.B '1') ]; F.block ~times:[ 30L ] [ (0, 1, F.B '0') ];
      ]
  in
  let t = open_ spec in
  let raising = { sink with Fst.time = (fun _ -> raise Exit) } in
  check "iter_changes: Exit reaches the caller"
    (match Fst.iter_changes t raising with exception Exit -> true | _ -> false);
  let r = match Fst.reader t raising with Ok r -> r | Error _ -> failwith "reader" in
  check "step: Exit reaches the caller"
    (match Fst.step r with exception Exit -> true | _ -> false);
  check "step after the exception: no block remains" (Fst.step r = Ok false);
  Fst.close t;
  Fst.close t;
  let t = open_ spec in
  check "iter_changes: other exceptions too"
    (match Fst.iter_changes t { sink with Fst.bit = (fun _ _ -> failwith "sink") } with
    | exception Failure _ -> true
    | _ -> false);
  check "the file is still usable" (Fst.iter_changes t sink = Ok ());
  Fst.close t

let () = if !failures > 0 then exit 1
