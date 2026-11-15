[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic, OpenAI"]

(* Truncated and corrupted inputs: every entry point must end in [Ok] or [Error], never an
   exception, and the entry points must agree. Covers prefixes, seeded byte mutations of the small
   corpus files, libfst's own malformed files (whose outcome is pinned), and hostile field values
   written by the fixture patchers. *)

module E = Fst.Error
module F = Fst_fixture

let failures = ref 0
let cases = ref 0
let calls = ref 0
let verbose = Sys.getenv_opt "ROBUST_VERBOSE" <> None
let current = ref ""

let fail name fmt =
  Printf.ksprintf
    (fun m ->
      incr failures;
      (match Sys.getenv_opt "ROBUST_DUMP" with
      | Some d ->
          Out_channel.with_open_bin
            (Filename.concat d (Printf.sprintf "f%d.fst" !failures))
            (fun oc -> output_string oc !current)
      | None -> ());
      if !failures <= 20 then Printf.printf "FAIL %s: %s\n%!" name m)
    fmt

let kind_name = function
  | E.Truncated -> "Truncated"
  | Malformed -> "Malformed"
  | Unsupported -> "Unsupported"
  | Decompression -> "Decompression"
  | Limit_exceeded -> "Limit_exceeded"
  | Io -> "Io"

(* The outcome of [f] as a name; an exception is a failure, and so is [Io] unless [closed]. *)
let call ?(closed = false) name entry f =
  incr calls;
  match f () with
  | Ok () -> "Ok"
  | Error e ->
      let k = E.kind e in
      if closed <> (k = E.Io) then
        fail name "%s: unexpected %s: %s" entry (kind_name k) (E.message e);
      ignore (E.to_string e);
      ignore (E.offset e);
      kind_name k
  | exception e ->
      fail name "%s raised %s\n%s" entry (Printexc.to_string e) (Printexc.get_backtrace ());
      "raised"

let expect_invalid name entry f =
  incr calls;
  match f () with
  | _ -> fail name "%s: no Invalid_argument" entry
  | exception Invalid_argument _ -> ()
  | exception e -> fail name "%s raised %s" entry (Printexc.to_string e)

let drain r =
  let rec go () =
    match Fst.step r with Ok true -> go () | Ok false -> Ok () | Error e -> Error e
  in
  go ()

let bit_chars = "01xzhuwl-?"

(* A sink that checks what the documentation promises of its calls, against [signals]. The events
   do not mark the frame, so [frame_bits] accepts any character as a bit. *)
let checking ?(frame_bits = false) name signals ~selected =
  let n = match signals with Some a -> Array.length a | None -> 0 in
  let sg what h =
    if h < 1 || h > n then (
      fail name "%s: handle %d outside 1..%d" what h n;
      None)
    else (
      if not (selected h) then fail name "%s: handle %d is not selected" what h;
      Some (Option.get signals).(h - 1))
  in
  let packed h b off width =
    ignore (sg "packed" h);
    ignore (Bytes.sub b off ((width + 7) / 8))
  in
  let in_frame = ref false in
  let sink =
    {
      Fst.time = (fun _ -> in_frame := false);
      frame = (fun _ -> in_frame := true);
      bit =
        (fun h c ->
          (match sg "bit" h with
          | Some (Fst.Bit, _ | Port, 1) -> ()
          | _ -> fail name "bit: handle %d is not a bit" h);
          if (not (!in_frame || frame_bits)) && not (String.contains bit_chars c) then
            fail name "bit: %C" c);
      vector =
        (fun h b off len ->
          (match sg "vector" h with
          | Some ((Fst.Vector | Port), l) when l = len -> ()
          | _ -> fail name "vector: handle %d, length %d" h len);
          ignore (Bytes.sub b off len));
      real =
        (fun h _ ->
          match sg "real" h with
          | Some (Fst.Real, _) -> ()
          | _ -> fail name "real: handle %d is not a real" h);
      varlen =
        (fun h b off len ->
          (match sg "varlen" h with
          | Some (Fst.String, _) -> ()
          | _ -> fail name "varlen: handle %d is not a string" h);
          ignore (Bytes.sub b off len));
    }
  in
  (sink, packed)

let odd h = h land 1 = 1
let all _ = true

(* Runs every entry point on [s] and returns the outcome of reading its value changes ([Ok], an
   error kind, or ["open:"] and the kind when it does not open). The entry points that read the
   same data must agree. *)
let exercise name s =
  incr cases;
  current := s;
  match Fst.of_string s with
  | Error e ->
      ignore (call name "of_string" (fun () -> Error e));
      "open:" ^ kind_name (E.kind e)
  | Ok t ->
      ignore (Fst.header t);
      ignore (Fst.blackouts t);
      let sg = ref None in
      ignore
        (call name "signals" (fun () ->
             Result.map
               (fun a ->
                 sg := Some a;
                 (match Fst.signals t with
                 | Ok b when a == b -> ()
                 | _ -> fail name "signals cache");
                 ())
               (Fst.signals t)));
      ignore (call name "geometry" (fun () -> Result.map ignore (Fst.geometry t)));
      let fold =
        call name "fold_hierarchy" (fun () ->
            Fst.fold_hierarchy t ~init:0 ~f:(fun n _ -> n + 1) |> Result.map ignore)
      in
      let bs = ref None in
      ignore (call name "blocks" (fun () -> Result.map (fun b -> bs := Some b) (Fst.blocks t)));
      let sink_of ?select ?frame_bits () =
        let selected = match select with Some f -> f | None -> all in
        checking ?frame_bits name !sg ~selected
      in
      let iter ?select ?(packed = false) entry =
        let sink, p = sink_of ?select () in
        let packed = if packed then Some p else None in
        call name entry (fun () -> Fst.iter_changes ?select ?packed t sink)
      in
      let plain = iter "iter_changes" in
      let agree entry got =
        if got <> plain then fail name "%s is %s, iter_changes is %s" entry got plain
      in
      let implies entry got = if plain = "Ok" && got <> "Ok" then fail name "%s is %s" entry got in
      ignore (iter ~select:odd "iter_changes ~select");
      agree "iter_changes ~packed" (iter ~packed:true "iter_changes ~packed");
      ignore (iter ~select:odd ~packed:true "iter_changes ~select ~packed");
      (let raising = { Fst.ignore_sink with time = (fun _ -> raise Exit) } in
       incr calls;
       match Fst.iter_changes t raising with
       | _ -> ()
       | exception Exit -> ()
       | exception e -> fail name "raising sink: %s" (Printexc.to_string e));
      agree "iter_changes after a raising sink" (iter "iter_changes again");
      let read ?select ?(packed = false) entry =
        let sink, p = sink_of ?select () in
        let packed = if packed then Some p else None in
        call name entry (fun () -> Result.bind (Fst.reader ?select ?packed t sink) drain)
      in
      agree "reader" (read "reader");
      ignore (read ~select:odd ~packed:true "reader ~select ~packed");
      let vcd ?extensions entry =
        call name entry (fun () -> Result.map ignore (Fst.Vcd.to_string ?extensions t))
      in
      let agree_vcd entry got =
        if got <> plain && got <> fold then
          fail name "%s is %s, iter_changes is %s, fold_hierarchy is %s" entry got plain fold
        else if plain = "Ok" && fold = "Ok" && got <> "Ok" then fail name "%s is %s" entry got
      in
      agree_vcd "Vcd.to_string" (vcd "Vcd.to_string");
      agree_vcd "Vcd.to_string ~extensions" (vcd ~extensions:true "Vcd.to_string ~extensions");
      let events_sink ?select ~packed () =
        let sink, p = sink_of ?select ~frame_bits:true () in
        {
          Fst.Vcd.ignore_sink with
          time = sink.time;
          frame = sink.frame;
          bit = sink.bit;
          vector = sink.vector;
          port = sink.vector;
          real = sink.real;
          varlen = sink.varlen;
          packed = (if packed then Some p else None);
        }
      in
      let events ?extensions ?select ?(packed = false) entry =
        call name entry (fun () ->
            Fst.Vcd.events ?extensions ?select t (events_sink ?select ~packed ()))
      in
      agree_vcd "Vcd.events" (events "Vcd.events");
      agree_vcd "Vcd.events ~extensions" (events ~extensions:true "Vcd.events ~extensions");
      agree_vcd "Vcd.events ~packed" (events ~packed:true "Vcd.events ~packed");
      ignore (events ~select:odd ~packed:true "Vcd.events ~select ~packed");
      let pulled ?extensions ?block entry =
        call name entry (fun () ->
            Result.bind
              (Fst.Vcd.reader ?extensions ?block t (events_sink ~packed:true ()))
              (fun r ->
                let rec go () =
                  match Fst.Vcd.step r with
                  | Ok true -> go ()
                  | Ok false -> Ok ()
                  | Error e -> Error e
                in
                go ()))
      in
      agree_vcd "Vcd.reader" (pulled "Vcd.reader");
      agree_vcd "Vcd.reader ~extensions" (pulled ~extensions:true "Vcd.reader ~extensions");
      (let raising = { Fst.Vcd.ignore_sink with time = (fun _ -> raise Exit) } in
       incr calls;
       match Fst.Vcd.events t raising with
       | _ -> ()
       | exception Exit -> ()
       | exception e -> fail name "raising events sink: %s" (Printexc.to_string e));
      agree_vcd "Vcd.events after a raising sink" (events "Vcd.events again");
      let n = match !bs with Some b -> Array.length b | None -> 0 in
      let ks =
        List.sort_uniq compare (List.filter (fun k -> k >= 0 && k < n) [ 0; 1; n / 2; n - 1 ])
      in
      List.iter
        (fun k ->
          let sink, _ = sink_of () in
          let at =
            call name (Printf.sprintf "reader_at %d" k) (fun () ->
                Result.bind (Fst.reader_at t sink ~block:k) drain)
          in
          if k = 0 then agree "reader_at 0" at;
          if k > 0 then ignore (pulled ~block:k (Printf.sprintf "Vcd.reader ~block:%d" k));
          let sink, p = sink_of ~select:odd () in
          ignore
            (call name (Printf.sprintf "reader_at %d ~select ~packed" k) (fun () ->
                 Result.bind (Fst.reader_at ~select:odd ~packed:p t sink ~block:k) drain));
          let al =
            call name (Printf.sprintf "aligned_block %d" k) (fun () ->
                Result.bind (Fst.aligned_block t k) (fun j ->
                    if j < 0 || j > k then fail name "aligned_block %d is %d" k j;
                    Ok ()))
          in
          implies (Printf.sprintf "aligned_block %d" k) al)
        ks;
      if !bs <> None then begin
        let sink, _ = sink_of () in
        expect_invalid name "reader_at past the end" (fun () -> Fst.reader_at t sink ~block:n);
        expect_invalid name "reader_at negative" (fun () -> Fst.reader_at t sink ~block:(-1));
        expect_invalid name "aligned_block past the end" (fun () -> Fst.aligned_block t n);
        expect_invalid name "aligned_block negative" (fun () -> Fst.aligned_block t (-1));
        expect_invalid name "Vcd.reader past the end" (fun () ->
            Fst.Vcd.reader ~block:n t Fst.Vcd.ignore_sink);
        expect_invalid name "Vcd.reader negative" (fun () ->
            Fst.Vcd.reader ~block:(-1) t Fst.Vcd.ignore_sink)
      end;
      let stepper =
        let sink, _ = sink_of () in
        Fst.reader t sink
      in
      let vstepper = Fst.Vcd.reader t Fst.Vcd.ignore_sink in
      Fst.close t;
      Fst.close t;
      let closed entry f = ignore (call ~closed:true name entry f) in
      closed "closed iter_changes" (fun () -> Fst.iter_changes t Fst.ignore_sink);
      closed "closed signals" (fun () -> Result.map ignore (Fst.signals t));
      closed "closed geometry" (fun () -> Result.map ignore (Fst.geometry t));
      closed "closed fold_hierarchy" (fun () ->
          Result.map ignore (Fst.fold_hierarchy t ~init:() ~f:(fun () _ -> ())));
      closed "closed blocks" (fun () -> Result.map ignore (Fst.blocks t));
      closed "closed Vcd.to_string" (fun () -> Result.map ignore (Fst.Vcd.to_string t));
      closed "closed reader" (fun () -> Result.map ignore (Fst.reader t Fst.ignore_sink));
      closed "closed Vcd.events" (fun () -> Fst.Vcd.events t Fst.Vcd.ignore_sink);
      closed "closed Vcd.reader" (fun () ->
          Result.map ignore (Fst.Vcd.reader t Fst.Vcd.ignore_sink));
      (match vstepper with
      | Ok r -> closed "closed Vcd.step" (fun () -> Result.map ignore (Fst.Vcd.step r))
      | Error _ -> ());
      (match stepper with
      | Ok r -> closed "closed step" (fun () -> Result.map ignore (Fst.step r))
      | Error _ -> ());
      if !bs <> None && n > 0 then begin
        closed "closed reader_at" (fun () ->
            Result.map ignore (Fst.reader_at t Fst.ignore_sink ~block:0));
        closed "closed aligned_block" (fun () -> Result.map ignore (Fst.aligned_block t 0))
      end;
      ignore (Fst.header t);
      ignore (Fst.blackouts t);
      plain

(* libfst's malformed files: the outcome of the file as it is, and of the file with its [.hier]
   sidecar (the hierarchy libfst reads from beside the file) appended as a hierarchy block. *)
let malformed_pins =
  [
    ("chain_shorter_than_its_length.fst", "open:Decompression", None);
    ("chain_table_overrun.fst", "Malformed", None);
    ("chain_table_pair_unterminated.fst", "Malformed", Some "Malformed");
    ("chain_table_unterminated.fst", "Ok", None);
    ("chain_uncompresses_past_traversal.fst", "Malformed", Some "Malformed");
    ("rvat_chain_table_overrun.fst", "Malformed", None);
    ("traversal_buffer_smaller_than_chain.fst", "Malformed", Some "Malformed");
    ("traversal_buffer_smaller_than_chain_fastlz.fst", "Malformed", Some "Malformed");
    ("uncompressed_chain_past_traversal.fst", "Malformed", None);
    ("uncompressed_chain_shorter_than_its_varint.fst", "Malformed", None);
  ]

let hier_block h =
  let be n =
    let b = Bytes.create 8 in
    Bytes.set_int64_be b 0 (Int64.of_int n);
    Bytes.to_string b
  in
  "\004" ^ be (String.length h + 16) ^ be (String.length h) ^ h

let mutate_corpus corpus =
  let rng = Random.State.make [| 42 |] in
  List.iter
    (fun f ->
      let s = Common.read_file (Filename.concat corpus f) in
      let n = String.length s in
      let t0 = Sys.time () in
      if n <= 200_000 then begin
        let big = n > 10_000 in
        let step = max 1 (n / if big then 100 else 400) in
        let i = ref 0 in
        while !i < n do
          ignore (exercise (Printf.sprintf "%s[0..%d]" f !i) (String.sub s 0 !i));
          i := !i + step
        done;
        for k = 1 to if big then 60 else 300 do
          let b = Bytes.of_string s in
          let edits = 1 + Random.State.int rng 3 in
          for _ = 1 to edits do
            Bytes.set b (Random.State.int rng n) (Char.chr (Random.State.int rng 256))
          done;
          ignore (exercise (Printf.sprintf "%s mutation %d" f k) (Bytes.to_string b))
        done
      end;
      if verbose then Printf.printf "  %s %d bytes %.2fs\n" f n (Sys.time () -. t0))
    (Common.files corpus ".fst")

let detail s =
  match Fst.of_string s with
  | Error e -> Fst.Error.to_string e
  | Ok t -> (
      match Fst.iter_changes t Fst.ignore_sink with
      | Ok () -> "Ok"
      | Error e -> Fst.Error.to_string e)

let pin_malformed dir =
  let files = Common.files dir ".fst" in
  List.iter
    (fun f ->
      let path = Filename.concat dir f in
      let s = Common.read_file path in
      let got = exercise f s in
      let sidecar = path ^ ".hier" in
      let with_hier =
        if Sys.file_exists sidecar then
          Some (exercise (f ^ ".hier") (s ^ hier_block (Common.read_file sidecar)))
        else None
      in
      if verbose then begin
        Printf.printf "%-50s %-20s %s\n" f got (Option.value with_hier ~default:"-");
        Printf.printf "    %s\n" (detail s);
        if Sys.file_exists sidecar then
          Printf.printf "    + hier: %s\n" (detail (s ^ hier_block (Common.read_file sidecar)))
      end;
      match List.find_opt (fun (n, _, _) -> n = f) malformed_pins with
      | None -> fail f "no pinned outcome (got %s)" got
      | Some (_, want, want_hier) ->
          if got <> want then fail f "expected %s, got %s" want got;
          if with_hier <> want_hier then
            fail f "with its hierarchy: expected %s, got %s"
              (Option.value want_hier ~default:"-")
              (Option.value with_hier ~default:"-"))
    files;
  List.iter
    (fun (f, _, _) -> if not (List.mem f files) then fail f "pinned but missing from %s" dir)
    malformed_pins

(* Boundary values, written only where the platform's [int] holds them. *)
let boundaries =
  let pow2 k = if k < Sys.int_size - 1 then [ (1 lsl k) - 1; 1 lsl k; (1 lsl k) + 1 ] else [] in
  List.sort_uniq compare
    ([ 0; 1; 2; 7; 8; 9; 255; 256; 65535; 65536 ]
    @ List.concat_map pow2 [ 29; 30; 31; 32; 40; 61 ]
    @ [ max_int - 1; max_int ])

let negatives = [ -1; min_int ]

let int64s =
  [
    0L;
    1L;
    7L;
    8L;
    9L;
    0x7fff_ffffL;
    0x8000_0000L;
    0xffff_ffffL;
    0x1_0000_0000L;
    Int64.shift_left 1L 62;
    Int64.max_int;
    Int64.min_int;
    -1L;
  ]

let skipped = ref 0

let fixture name spec =
  match F.build spec with
  | s -> ignore (exercise name s)
  | exception Invalid_argument m ->
      if verbose then Printf.printf "  skipped %s: %s\n" name m;
      incr skipped

let base_spec =
  F.make
    [
      F.signal "a" F.Bit;
      F.signal "v" (F.Vector 8);
      F.signal "w" (F.Vector 3);
      F.signal "r" F.Real;
      F.signal "s" F.String;
      F.signal ~alias:1 "a2" F.Bit;
    ]
    [
      F.block ~start:5L
        ~frame:[ (1, F.B '0'); (2, F.V "00000000"); (4, F.R 1.5) ]
        ~times:[ 10L; 20L; 30L ]
        [
          (0, 1, F.B '1');
          (0, 2, F.V "10100101");
          (1, 1, F.B 'z');
          (1, 3, F.V "1x0");
          (1, 4, F.R 2.25);
          (2, 5, F.S "hi");
          (2, 1, F.B '0');
        ];
      F.block ~times:[ 40L; 50L ] [ (0, 2, F.V "00001111"); (1, 5, F.S "a b"); (1, 1, F.B 'x') ];
      F.block ~times:[ 50L; 60L ] [ (0, 5, F.S "c"); (1, 1, F.B '1'); (1, 4, F.R 0.5) ];
    ]

let alias_spec =
  let str t h v = (t, h, F.S v) in
  F.make
    (List.init 5 (fun i -> F.signal (Printf.sprintf "s%d" (i + 1)) F.String))
    [
      F.block
        ~aliases:[ (2, 4); (5, 2) ]
        ~times:[ 10L; 20L ]
        [ str 0 1 "a"; str 0 4 "b"; str 1 4 "c" ];
      F.block ~max_handle:5 ~times:[ 30L; 40L ] [ str 0 1 "d"; str 0 4 "e"; str 1 4 "f" ];
      F.block ~times:[ 50L; 60L ] [ str 0 1 "g" ];
    ]

let be64 s p = Int64.to_int (String.get_int64_be s p)

let chain_tables =
  [
    "";
    "\000";
    "\001";
    "\002";
    "\003";
    "\000\000";
    "\000\001";
    "\000\255";
    "\002\002\002";
    "\128";
    "\255\255\255\255\255\255\255\255\255\255\001";
    "\001\001\001\001\001\001\001\001";
    String.make 300 '\001';
    "\254\255\003";
    "\003\000\000\000\000";
  ]

let mutate_fixtures () =
  List.iter
    (fun (sname, spec) ->
      let nsig = List.length spec.F.signals in
      let nblocks = List.length spec.F.blocks in
      let nhandles = List.length (List.filter (fun s -> s.F.alias = None) spec.signals) in
      let label fmt = Printf.ksprintf (fun s -> sname ^ " " ^ s) fmt in
      fixture (label "unmutated") spec;
      for signal = 1 to nsig do
        List.iter
          (fun n -> fixture (label "set_length %d=%d" signal n) (F.set_length spec ~signal n))
          [ 0; 1; 7; 8; 9; 1000 ];
        List.iter
          (fun n ->
            fixture
              (label "set_declared_length %d=%d" signal n)
              (F.set_declared_length spec ~signal n))
          (boundaries @ negatives);
        for ty = 0 to 40 do
          fixture (label "set_var_type %d=%d" signal ty) (F.set_var_type spec ~signal ty)
        done;
        fixture (label "set_var_type %d=255" signal) (F.set_var_type spec ~signal 255);
        List.iter
          (fun n ->
            fixture (label "set_alias_target %d=%d" signal n) (F.set_alias_target spec ~signal n))
          (boundaries @ negatives)
      done;
      for block = 0 to nblocks - 1 do
        for handle = 1 to nhandles do
          List.iter
            (fun d ->
              fixture
                (label "set_chain_dest b%d h%d=%d" block handle d)
                (F.set_chain_dest spec ~block ~handle d))
            (boundaries @ negatives)
        done;
        List.iter
          (fun m ->
            fixture (label "set_mem_required b%d=%Ld" block m) (F.set_mem_required spec ~block m))
          int64s;
        List.iter
          (fun c ->
            fixture (label "set_chain_table b%d %S" block c) (F.set_chain_table spec ~block c))
          chain_tables;
        let s = F.build spec in
        match Fst.of_string s with
        | Error _ -> fail sname "unmutated fixture does not open"
        | Ok t ->
            let pos = Int64.to_int (Result.get_ok (Fst.blocks t)).(block).pos in
            let tr = pos + 1 + be64 s (pos + 1) - 24 in
            let valid = (be64 s tr, be64 s (tr + 8), be64 s (tr + 16)) in
            let uclen0, clen0, nitems0 = valid in
            List.iter
              (fun v ->
                let set ~uclen ~clen ~nitems what =
                  fixture
                    (label "set_trailer b%d %s=%d" block what v)
                    (F.set_trailer spec ~block ~uclen ~clen ~nitems)
                in
                set ~uclen:v ~clen:clen0 ~nitems:nitems0 "uclen";
                set ~uclen:uclen0 ~clen:v ~nitems:nitems0 "clen";
                set ~uclen:uclen0 ~clen:clen0 ~nitems:v "nitems";
                set ~uclen:v ~clen:v ~nitems:v "all")
              (boundaries @ negatives);
            Fst.close t
      done)
    [ ("base", base_spec); ("alias", alias_spec) ]

(* Values whose sums with a position wrap around: a variable as wide as an int allows (a width
   is read as 32 bits, so this only reaches 31-bit ints), and chain lengths whose difference is
   [max_int] in a dynamic alias 2 chain table, whatever the int size. *)
let wrapping () =
  let outcome name spec =
    let got = exercise name (F.build spec) in
    if got = "Ok" then fail name "expected an error, got Ok"
  in
  List.iter
    (fun signal ->
      outcome
        (Printf.sprintf "max_int wide signal %d" signal)
        (F.set_declared_length base_spec ~signal max_int))
    [ 2; 3 ];
  let alias2 =
    {
      base_spec with
      blocks = List.mapi (fun i b -> if i = 0 then { b with F.typ = 8 } else b) base_spec.blocks;
    }
  in
  let table = "\003" ^ String.make 9 '\255' ^ "\000" in
  outcome "chain length max_int"
    (F.set_chain_table (F.set_chain_dest alias2 ~block:0 ~handle:1 4) ~block:0 table)

let () =
  Printexc.record_backtrace true;
  let corpus = Sys.argv.(1) and malformed = Sys.argv.(2) in
  let t0 = Sys.time () in
  mutate_corpus corpus;
  let t1 = Sys.time () in
  pin_malformed malformed;
  let before = !cases in
  mutate_fixtures ();
  wrapping ();
  let t2 = Sys.time () in
  Printf.printf
    "robust: %d cases (%d fixture mutations, %d not representable), %d calls, %d failed\n" !cases
    (!cases - before) !skipped !calls !failures;
  if verbose then Printf.printf "robust: corpus %.1fs, fixtures %.1fs\n" (t1 -. t0) (t2 -. t1);
  if !failures > 0 then exit 1
