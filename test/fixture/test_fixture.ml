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
  check "block count" (Array.length (ok "blocks" (Fst.blocks t)) = 2);
  let vcd = ok "vcd" (Fst.Vcd.to_string t) in
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
  check "set_alias_target" (List.mem (1, 3, true) (hier (F.set_alias_target spec ~signal:6 3)));
  let pos = (ok "blocks" (Fst.blocks t)).(1).pos in
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
  counted "no header" "" ~opens:false ~after_close:0;
  if !failures > 0 then exit 1
