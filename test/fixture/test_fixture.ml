[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic"]

module F = Fst_fixture

let failures = ref 0

let check name ok =
  if not ok then begin
    incr failures;
    Printf.printf "FAIL %s\n%!" name
  end

let ok name = function
  | Ok v -> v
  | Error e ->
      check (name ^ ": " ^ Fst.Error.to_string e) false;
      raise Exit

let spec =
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
    ]

let read s = ok "of_string" (Fst.of_string s)

let reference exe s =
  let fst = Filename.temp_file "fixture" ".fst" and vcd = Filename.temp_file "fixture" ".vcd" in
  Out_channel.with_open_bin fst (fun oc -> output_string oc s);
  let rc =
    Sys.command (Printf.sprintf "%s vcd %s %s" exe (Filename.quote fst) (Filename.quote vcd))
  in
  let out = if rc = 0 then Some (In_channel.with_open_bin vcd In_channel.input_all) else None in
  Sys.remove fst;
  Sys.remove vcd;
  out

let contains s sub =
  let n = String.length sub in
  let rec go i = i + n <= String.length s && (String.sub s i n = sub || go (i + 1)) in
  go 0

let () =
  let exe = Sys.argv.(1) in
  let s = F.build spec in
  let t = read s in
  let h = Fst.header t in
  check "header times" (h.start_time = 5L && h.end_time = 50L);
  check "header counts" (h.var_count = 6L && h.max_handle = 5L && h.vc_section_count = 2L);
  (let expected = [| (Fst.Bit, 1); (Vector, 8); (Vector, 3); (Real, 8); (String, 0) |] in
   let a = ok "signals" (Fst.signals t) in
   check "signals" (a = expected);
   check "signals are cached" (Fst.signals t == Fst.signals t);
   let port = read (F.build (F.set_var_type spec ~signal:2 18)) in
   check "port kind"
     (ok "signals" (Fst.signals port)
     = [| (Fst.Bit, 1); (Port, 8); (Vector, 3); (Real, 8); (String, 0) |]);
   let zero = read (F.build (F.set_length spec ~signal:2 0)) in
   check "zero length is a string" ((ok "signals" (Fst.signals zero)).(1) = (Fst.String, 0)));
  check "block count" (Array.length (ok "blocks" (Fst.blocks t)) = 2);
  let vcd = ok "vcd" (Fst.Vcd.to_string t) in
  List.iter
    (fun (what, mode, packed) ->
      check ("events render " ^ what) (Vcd_events.render ~packed ~mode t = Ok vcd))
    [ ("pushed", Vcd_events.Push, false); ("pulled", Pull, false); ("packed", Push, true) ];
  List.iter
    (fun sub -> check ("vcd has " ^ String.escaped sub) (contains vcd sub))
    [
      "$var wire 1 ! a $end";
      "$var wire 1 ! a2 $end";
      "$var real 64 $ r $end";
      "#5\n$dumpvars\n0!\nb00000000 \"\nbxxx #\nr1.5 $\n$end\n";
      "#10\nb10100101 \"\n1!\n";
      "#20\nz!\nr2.25 $\nb1x0 #\n";
      "#30\n0!\nshi %\n";
      "#50\nsa\\040b %\nx!\n";
    ];
  (match reference exe s with
  | Some expected -> check "libfst accepts the fixture and agrees" (expected = vcd)
  | None -> check "libfst rejected the fixture" false);
  let changes = ref [] in
  let sink =
    {
      Fst.time = (fun t -> changes := Printf.sprintf "t%Ld" t :: !changes);
      frame = (fun t -> changes := Printf.sprintf "f%Ld" t :: !changes);
      bit = (fun h c -> changes := Printf.sprintf "%d=%c" h c :: !changes);
      vector =
        (fun h b off len ->
          changes := Printf.sprintf "%d=%s" h (Bytes.sub_string b off len) :: !changes);
      real = (fun h d -> changes := Printf.sprintf "%d=%g" h d :: !changes);
      varlen =
        (fun h b off len ->
          changes := Printf.sprintf "%d=%s" h (Bytes.sub_string b off len) :: !changes);
    }
  in
  ignore (ok "iter" (Fst.iter_changes t sink));
  let seen = List.rev !changes in
  check "frame first" (List.filteri (fun i _ -> i < 4) seen = [ "f5"; "1=0"; "2=00000000"; "3=xxx" ]);
  check "changes" (List.mem "t40" seen && List.mem "5=a b" seen);
  (* The patchers change one field each and leave the rest readable. *)
  let hier t =
    ok "hier"
      (Fst.fold_hierarchy
         (read (F.build t))
         ~init:[]
         ~f:(fun acc -> function
           | Fst.Hier.Var { length; handle; alias; _ } -> (length, handle, alias) :: acc | _ -> acc))
  in
  check "set_length" (List.mem (4, 4, false) (hier (F.set_length spec ~signal:4 4)));
  check "set_declared_length"
    (List.mem (100, 2, false) (hier (F.set_declared_length spec ~signal:2 100))
    && String.length (F.build (F.set_declared_length spec ~signal:2 100)) = String.length s);
  check "set_alias_target" (List.mem (1, 3, true) (hier (F.set_alias_target spec ~signal:6 3)));
  let pos = Int64.to_int (ok "blocks" (Fst.blocks t)).(1).pos in
  let be64 s p = Int64.to_int (String.get_int64_be s p) in
  let patched = F.build (F.set_trailer spec ~block:1 ~uclen:7 ~clen:9 ~nitems:11) in
  check "set_trailer size" (String.length patched = String.length s);
  let tr = pos + 1 + be64 patched (pos + 1) - 24 in
  check "set_trailer bytes"
    (be64 patched tr = 7 && be64 patched (tr + 8) = 9 && be64 patched (tr + 16) = 11);
  ignore (read (F.build (F.set_chain_dest spec ~block:0 ~handle:2 7)));
  ignore (read (F.build (F.set_chain_table spec ~block:0 "\001")));
  (* A source is closed exactly once, whatever the outcome of opening it. *)
  let wrapped data =
    let b = Buffer.create (String.length data + 17) in
    Buffer.add_char b '\xfe';
    Buffer.add_int64_be b (Int64.of_int (String.length data + 8));
    Buffer.add_int64_be b (Int64.of_int (String.length data));
    Buffer.add_string b data;
    Buffer.contents b
  in
  let counted name data ~opens ~after_close =
    let closes = ref 0 in
    let length = Int64.of_int (String.length data) in
    let read pos len =
      let p = Int64.to_int pos in
      if p >= String.length data then "" else String.sub data p (min len (String.length data - p))
    in
    let r = Fst.Private.of_reader ~close:(fun () -> incr closes) ~length ~read () in
    (match r with
    | Ok t ->
        check (name ^ ": opened") opens;
        if opens then begin
          check (Printf.sprintf "%s: closes after open = %d" name !closes) (!closes = after_close);
          Fst.close t;
          Fst.close t
        end
    | Error _ -> check (name ^ ": rejected") (not opens));
    check (Printf.sprintf "%s: closes = %d" name !closes) (!closes = 1)
  in
  counted "plain" s ~opens:true ~after_close:0;
  counted "wrapped" (wrapped s) ~opens:true ~after_close:1;
  counted "wrapped truncated" (String.sub (wrapped s) 0 20) ~opens:false ~after_close:0;
  counted "wrapped, bad inner" (wrapped (String.sub s 0 40)) ~opens:false ~after_close:0;
  counted "no header" "" ~opens:false ~after_close:0

(* D6: a variable that is an unresolved dynamic alias in one block keeps its negative chain length
   when the next block lists it as unchanged, and that block then resolves it as the alias. Handle 2
   aliases the later handle 4, which libfst's parser leaves unresolved; handle 5 aliases handle 2
   and takes over its length. (A forward alias must not precede the first chain: the parser would
   overwrite its length.) libfst's writer only aliases earlier handles, so only a fixture
   produces this. *)
let stale =
  F.make
    (List.map (fun n -> F.signal n F.Bit) [ "a"; "b"; "c"; "d"; "e" ])
    [
      F.block
        ~aliases:[ (2, 4); (5, 2) ]
        ~times:[ 10L; 20L ]
        [ (0, 1, F.B '1'); (0, 4, F.B '1'); (1, 4, F.B '0') ];
      F.block ~max_handle:5 ~times:[ 30L; 40L ]
        [ (0, 1, F.B '0'); (0, 4, F.B '1'); (1, 4, F.B 'z') ];
    ]

let tagged calls =
  let add h fmt = Printf.ksprintf (fun s -> calls := (h, s) :: !calls) fmt in
  {
    Fst.ignore_sink with
    time = (fun t -> add 0 "#%Ld" t);
    frame = (fun t -> add 0 "frame %Ld" t);
    bit = (fun h c -> add h "%d=%c" h c);
  }

let steps r calls =
  let rec go acc =
    calls := [];
    if ok "step" (Fst.step r) then go (List.rev !calls :: acc) else List.rev acc
  in
  go []

let rec after_frame = function
  | (0, s) :: rest when String.length s > 1 && s.[0] = '#' -> (0, s) :: rest
  | _ :: rest -> after_frame rest
  | [] -> []

(* The sequential read's calls from block [k] on, and those of [reader_at k] after its frame. *)
let suffix_and_seek t k =
  let c = ref [] in
  let all = steps (ok "reader" (Fst.reader t (tagged c))) c in
  let suffix = List.concat (List.filteri (fun i _ -> i >= k) all) in
  let c = ref [] in
  let seek = steps (ok "reader_at" (Fst.reader_at t (tagged c) ~block:k)) c in
  (suffix, after_frame (List.concat seek))

(* The documented exception to [reader_at k] being the sequential suffix: a forward alias's
   negative chain length is not carried across a seek, so the stale alias of handle 5 is not
   replayed. [reader_at k] is libfst's time-limited read, which shares this. *)
let () =
  let exe = Sys.argv.(1) in
  let s = F.build stale in
  let t = read s in
  let suffix, seek = suffix_and_seek t 1 in
  let show l = String.concat " " (List.map snd l) in
  check "stale: block 1 sequentially gives handle 5 handle 4's changes"
    (suffix = [ (0, "#30"); (5, "5=1"); (4, "4=1"); (1, "1=0"); (0, "#40"); (4, "4=z"); (5, "5=z") ]);
  check
    (Printf.sprintf
       "exception: reader_at 1 lacks the sequential suffix's stale-alias changes (suffix: %s; \
        reader_at 1: %s)"
       (show suffix) (show seek))
    (seek = List.filter (fun (h, _) -> h <> 5) suffix);
  let path = Filename.temp_file "stale" ".fst" in
  Out_channel.with_open_bin path (fun oc -> output_string oc s);
  let records what = function
    | Ok l -> l
    | Error e ->
        check (what ^ ": " ^ e) false;
        []
  in
  let sequential = records "libfst" (Time_range.libfst exe path) in
  let bs = ok "blocks" (Fst.blocks t) in
  (match Time_range.start_for bs 1 with
  | None -> check "stale: no time range start reaches block 1" false
  | Some start ->
      let limited = records "libfst, limited" (Time_range.libfst exe ~start path) in
      let at = records "reader_at" (Time_range.reader_at t ~block:1) in
      check "stale: libfst's sequential read has handle 5 in block 1"
        (List.mem "30 5 v:1" sequential);
      check "stale: libfst's time-limited read has only handle 5's frame value"
        (List.filter (fun r -> List.nth (String.split_on_char ' ' r) 1 = "5") limited
        = [ "30 5 v:x" ]);
      check "exception: reader_at 1 = libfst's time-limited read" (limited = at));
  Sys.remove path;
  (match reference exe s with
  | Some expected ->
      check "libfst agrees with the stale fixture" (expected = ok "vcd" (Fst.Vcd.to_string t));
      check "events render the stale fixture"
        (Vcd_events.render ~mode:Push t = Ok expected
        && Vcd_events.render ~packed:true ~mode:Pull t = Ok expected)
  | None -> check "libfst rejected the stale fixture" false);
  if !failures > 0 then exit 1
