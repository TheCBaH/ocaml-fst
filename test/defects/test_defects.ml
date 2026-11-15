[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic"]

module F = Fst_fixture

let failures = ref 0

let check name ok =
  if not ok then begin
    incr failures;
    Printf.printf "FAIL %s\n%!" name
  end

let sink = Fst.ignore_sink

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

(* D7: [ignore_sink] ignores everything, and a record update receives only what it overrides. *)
let () =
  let spec =
    F.make
      [ F.signal "a" F.Bit; F.signal "v" (F.Vector 4) ]
      [ F.block ~times:[ 10L; 20L ] [ (0, 1, F.B '1'); (1, 2, F.V "1010"); (1, 1, F.B '0') ] ]
  in
  let t = open_ spec in
  check "ignore_sink" (Fst.iter_changes t Fst.ignore_sink = Ok ());
  let bits = ref 0 and vectors = ref 0 in
  let counting =
    { Fst.ignore_sink with bit = (fun _ _ -> incr bits); vector = (fun _ _ _ _ -> incr vectors) }
  in
  check "counting" (Fst.iter_changes t counting = Ok () && !bits = 2 && !vectors = 1);
  Fst.close t

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

(* S4: seeks share per-block checkpoints, and a seek gives what a fresh handle's gives, whatever
   the order of the seeks, the error of a damaged earlier block included. *)
let seek_result ?select t k =
  let calls = ref [] in
  let add h fmt = Printf.ksprintf (fun s -> calls := Printf.sprintf "%d:%s" h s :: !calls) fmt in
  let sink =
    {
      Fst.time = (fun t -> add 0 "#%Lu" t);
      frame = (fun t -> add 0 "frame %Lu" t);
      bit = (fun h c -> add h "%c" c);
      vector = (fun h b off len -> add h "b%s" (Bytes.sub_string b off len));
      real = (fun h d -> add h "r%g" d);
      varlen = (fun h b off len -> add h "s%S" (Bytes.sub_string b off len));
    }
  in
  match Result.bind (Fst.reader_at ?select t sink ~block:k) drain with
  | Ok () -> Ok (List.rev !calls)
  | Error e -> Error (Fst.Error.to_string e)

let same_as_fresh ?select name spec seeks =
  let t = open_ spec in
  List.iter
    (fun k ->
      let fresh = open_ spec in
      check
        (Printf.sprintf "%s: seek %d after %s equals a fresh handle's" name k
           (String.concat "," (List.map string_of_int seeks)))
        (seek_result ?select t k = seek_result ?select fresh k);
      Fst.close fresh)
    seeks;
  Fst.close t

let () =
  let strings n = List.init n (fun i -> F.signal (Printf.sprintf "s%d" (i + 1)) F.String) in
  let str t h v = (t, h, F.S v) in
  (* The stale-alias fixture of test_fixture, with string variables: block 1 resolves handle 5 as
     the alias of handle 2, which still has its negative length from block 0. *)
  let stale =
    F.make (strings 5)
      [
        F.block
          ~aliases:[ (2, 4); (5, 2) ]
          ~times:[ 10L; 20L ]
          [ str 0 1 "a"; str 0 4 "b"; str 1 4 "c" ];
        F.block ~max_handle:5 ~times:[ 30L; 40L ] [ str 0 1 "d"; str 0 4 "e"; str 1 4 "f" ];
        F.block ~times:[ 50L; 60L ] [ str 0 1 "g" ];
        F.block ~times:[ 70L ] [ str 0 1 "h" ];
      ]
  in
  List.iter
    (same_as_fresh "stale strings" stale)
    [ [ 1; 2; 3 ]; [ 3; 2; 1 ]; [ 2; 1; 3 ]; [ 3; 3; 1; 2; 2 ]; [ 1; 3; 2; 3 ] ];
  let t = open_ stale in
  ignore (seek_result t 1);
  (match seek_result t 2 with
  | Ok calls ->
      check "stale strings: seek 2 recovers handle 5 through the carried negative length"
        (List.exists (fun c -> String.length c > 2 && String.sub c 0 2 = "5:") calls)
  | Error e -> check ("stale strings: " ^ e) false);
  Fst.close t;
  (* Another selection on the same handle has its own checkpoints. *)
  let t = open_ stale in
  let odd h = h land 1 = 1 in
  List.iter
    (fun (select, k) ->
      let fresh = open_ stale in
      check
        (Printf.sprintf "stale strings: mixed selections, seek %d" k)
        (seek_result ?select t k = seek_result ?select fresh k);
      Fst.close fresh)
    [
      (Some odd, 3); (None, 2); (Some odd, 2); (Some (fun h -> h = 4), 3); (None, 3); (Some odd, 1);
    ];
  Fst.close t

(* A seek reads on to the end, but decodes the blocks before its own once: a later seek does not
   read the ones a checkpoint covers. *)
let () =
  let spec =
    F.make
      [ F.signal "a" F.Bit; F.signal "s" F.String ]
      (List.init 6 (fun i ->
           let t0 = Int64.of_int ((20 * i) + 10) in
           F.block
             ~times:[ t0; Int64.add t0 10L ]
             [ (0, 1, F.B '1'); (1, 2, F.S (String.make (i + 1) 'x')) ]))
  in
  let data = F.build spec in
  let log = ref [] in
  let read pos len =
    log := pos :: !log;
    let p = Int64.to_int pos in
    if p >= String.length data then "" else String.sub data p (min len (String.length data - p))
  in
  match Fst.Private.of_reader ~length:(Int64.of_int (String.length data)) ~read () with
  | Error e -> check (Fst.Error.to_string e) false
  | Ok t -> (
      match Fst.blocks t with
      | Error e -> check (Fst.Error.to_string e) false
      | Ok bs ->
          ignore (Fst.signals t);
          let decoded = Array.make (Array.length bs) 0 in
          let seek k =
            log := [];
            check "counted seek" (seek_result t k <> Error "");
            List.iter
              (fun pos ->
                Array.iteri
                  (fun j (b : Fst.block) ->
                    if Int64.equal pos (Int64.succ b.pos) then decoded.(j) <- decoded.(j) + 1)
                  bs)
              !log
          in
          let counts name expected =
            check
              (Printf.sprintf "%s: blocks decoded %s, expected %s" name
                 (String.concat "," (Array.to_list (Array.map string_of_int decoded)))
                 (String.concat "," (List.map string_of_int expected)))
              (Array.to_list decoded = expected)
          in
          seek 4;
          counts "seek 4" [ 1; 1; 1; 1; 1; 1 ];
          seek 4;
          counts "seek 4 again" [ 1; 1; 1; 1; 2; 2 ];
          seek 5;
          counts "seek 5" [ 1; 1; 1; 1; 3; 3 ];
          seek 2;
          counts "seek 2" [ 1; 1; 2; 2; 4; 4 ];
          Fst.close t)

(* A damaged earlier block fails every seek past it with the same error, and is not cached as
   read. *)
let () =
  let spec =
    F.make
      [ F.signal "a" F.Bit; F.signal "s" F.String ]
      (List.init 5 (fun i ->
           let t0 = Int64.of_int ((20 * i) + 10) in
           F.block
             ~times:[ t0; Int64.add t0 10L ]
             [ (0, 1, F.B '1'); (1, 2, F.S (String.make (i + 1) 'x')) ]))
  in
  let bad = F.set_trailer spec ~block:2 ~uclen:2 ~clen:100000 ~nitems:2 in
  List.iter
    (same_as_fresh "damaged block 2" bad)
    [ [ 3; 4; 3; 1; 4 ]; [ 4; 3; 2; 1; 0 ]; [ 1; 2; 3; 4 ] ];
  let t = open_ bad in
  (match seek_result t 3 with
  | Error m -> check "damaged block 2: Malformed" (m <> "")
  | Ok _ -> check "damaged block 2: seek 3 Ok" false);
  check "damaged block 2: seek 4 fails" (Result.is_error (seek_result t 4));
  Fst.close t

let () = if !failures > 0 then exit 1
