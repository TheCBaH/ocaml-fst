[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic"]

module F = Fst_fixture

let failures = ref 0

let check name ok =
  if not ok then begin
    incr failures;
    Printf.printf "FAIL %s\n%!" name
  end

let read_file path = In_channel.with_open_bin path In_channel.input_all

let contains s sub =
  let n = String.length sub in
  let rec go i = i + n <= String.length s && (String.sub s i n = sub || go (i + 1)) in
  go 0

let starts_with p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p
let get = function Ok v -> v | Error e -> failwith (Fst.Error.to_string e)
let lines_with prefix vcd = List.filter (starts_with prefix) (String.split_on_char '\n' vcd)

let times vcd =
  List.filter_map
    (fun l ->
      if starts_with "#" l then Int64.of_string_opt ("0u" ^ String.sub l 1 (String.length l - 1))
      else None)
    (String.split_on_char '\n' vcd)

(* The times of [$dumpoff] and [$dumpon], which follow the time they happen at. *)
let blackouts_of vcd =
  let time = ref 0L in
  List.filter_map
    (fun l ->
      if starts_with "#" l then begin
        time := Int64.of_string ("0u" ^ String.sub l 1 (String.length l - 1));
        None
      end
      else if l = "$dumpoff $end" then Some (!time, false)
      else if l = "$dumpon $end" then Some (!time, true)
      else None)
    (String.split_on_char '\n' vcd)

(* Widths as the geometry block records them, from the hierarchy. *)
let widths t =
  get
    (Fst.fold_hierarchy t ~init:[] ~f:(fun acc -> function
      | Fst.Hier.Var { var_type; length; alias = false; _ } ->
          (if Fst.Hier.is_real var_type then 0L
           else if length = 0 then 0xFFFF_FFFFL
           else Int64.of_int length)
          :: acc
      | _ -> acc))
  |> List.rev |> Array.of_list

let ends_with suffix s =
  let n = String.length suffix and m = String.length s in
  m >= n && String.sub s (m - n) n = suffix

let variant name = String.sub name 0 2

let timescale_of name =
  let stem = Filename.remove_extension name in
  let after p = String.sub stem (String.length p) (String.length stem - String.length p) in
  if contains stem "timescale_" then int_of_string (after (variant name ^ "_timescale_"))
  else if contains stem "repack" || contains stem "clean" then -12
  else -9

let corpus dir =
  let files =
    Sys.readdir dir |> Array.to_list |> List.filter (ends_with ".fst") |> List.sort compare
  in
  let with_blackouts = ref 0 and with_geometry = ref 0 in
  List.iter
    (fun f ->
      let path = Filename.concat dir f in
      let vcd = read_file (Filename.remove_extension path ^ ".vcd") in
      let t = get (Fst.open_file path) in
      let h = Fst.header t in
      let check s ok = check (f ^ ": " ^ s) ok in
      check "version" (h.version = "fst_ref " ^ variant f);
      check "date" (h.date = "Wed Oct 14 12:00:00 2026\n");
      check "timescale" (h.timescale = timescale_of f);
      check "timezero" (h.timezero = if contains f "_timezero" then -42L else 0L);
      check "file type, byte order" (h.file_type = 0 && h.little_endian);
      check "VCD $date" (contains vcd ("$date\n\t" ^ h.date ^ "\n$end\n"));
      check "VCD $version" (contains vcd ("$version\n\t" ^ h.version ^ "\n$end\n"));
      check "VCD $timescale"
        (contains vcd ("$timescale\n\t" ^ Fst.Vcd.timescale h.timescale ^ "\n$end\n"));
      (match times vcd with
      | [] -> check "VCD has times" false
      | first :: _ as ts ->
          check "start time" (h.start_time = if contains f "late_start" then 0L else first);
          check "end time" (h.end_time = List.fold_left (fun _ x -> x) first ts));
      let vars = lines_with "$var " vcd in
      let ids =
        List.sort_uniq compare (List.map (fun l -> List.nth (String.split_on_char ' ' l) 3) vars)
      in
      check "var_count" (h.var_count = Int64.of_int (List.length vars));
      check "max_handle" (h.max_handle = Int64.of_int (List.length ids));
      check "scope_count" (h.scope_count = Int64.of_int (List.length (lines_with "$scope " vcd)));
      check "vc_section_count"
        (h.vc_section_count = Int64.of_int (Array.length (get (Fst.blocks t))));
      let b = Fst.blackouts t in
      check "blackouts" (b = blackouts_of vcd);
      if b <> [] then incr with_blackouts;
      let g = get (Fst.geometry t) in
      check "geometry has a width per handle" (Array.length g = Int64.to_int h.max_handle);
      check "geometry is the hierarchy's widths" (g = widths t);
      if Array.length g > 0 then incr with_geometry;
      Fst.close t)
    files;
  check "some files have blackouts" (!with_blackouts >= 6);
  check "some files have geometry" (!with_geometry >= 30);
  let kitchen = get (Fst.open_file (Filename.concat dir "a2_kitchen_z.fst")) in
  check "kitchen blackouts" (Fst.blackouts kitchen = [ (1500L, false); (1800L, true) ]);
  let g = get (Fst.geometry kitchen) in
  check "kitchen widths"
    (g.(0) = 1L && g.(2) = 8L && g.(4) = 130L && g.(5) = 0L && g.(11) = 0xFFFF_FFFFL && g.(12) = 14L);
  Fst.close kitchen

(* A section appended to a fixture file: the type, the length that counts itself, the body. *)
let section typ body =
  let b = Buffer.create 32 in
  Buffer.add_char b (Char.chr typ);
  Buffer.add_int64_be b (Int64.of_int (String.length body + 8));
  Buffer.add_string b body;
  Buffer.contents b

let u64 n =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 n;
  Bytes.to_string b

let varint64 n =
  let b = Buffer.create 10 in
  let rec go n =
    if Int64.compare n 0L < 0 || Int64.compare n 0x80L >= 0 then begin
      Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand n 0x7fL) lor 0x80));
      go (Int64.shift_right_logical n 7)
    end
    else Buffer.add_char b (Char.chr (Int64.to_int n))
  in
  go n;
  Buffer.contents b

let spec =
  F.make
    [ F.signal "a" F.Bit; F.signal "v" (F.Vector 8); F.signal "r" F.Real ]
    [ F.block ~times:[ 10L; 20L ] [ (0, 1, F.B '1'); (1, 2, F.V "10100101") ] ]

let fixture_header () =
  let s = F.build spec in
  let h = Fst.header (get (Fst.of_string s)) in
  check "fixture header"
    (h
    = {
        Fst.Header.start_time = 10L;
        end_time = 20L;
        little_endian = true;
        writer_memory = 0L;
        scope_count = 1L;
        var_count = 3L;
        max_handle = 3L;
        vc_section_count = 1L;
        timescale = -9;
        version = "ocaml-fst fixture";
        date = "Thu Jan  1 00:00:00 1970\n";
        file_type = 0;
        timezero = 0L;
      });
  List.iter
    (fun ts ->
      let s = F.build { spec with F.timescale = ts } in
      check (Printf.sprintf "timescale %d" ts) ((Fst.header (get (Fst.of_string s))).timescale = ts))
    [ -128; -21; -9; 0; 2; 5; 127 ];
  (* The endianness test value is at offset 25, after the section type, length and two times. *)
  let swapped =
    let b = Bytes.of_string s in
    Bytes.set_int64_be b 25 (Int64.bits_of_float 2.7182818284590452354);
    Bytes.to_string b
  in
  check "big-endian writer" (not (Fst.header (get (Fst.of_string swapped))).little_endian);
  let bad = Bytes.of_string s in
  Bytes.set bad 25 '\000';
  Bytes.set bad 32 '\000';
  (match Fst.of_string (Bytes.to_string bad) with
  | Error e -> check "bad endianness: Malformed" (Fst.Error.kind e = Malformed)
  | Ok _ -> check "bad endianness: Ok" false);
  match Fst.of_string (String.sub s 0 100) with
  | Error e -> check "short header: Malformed" (Fst.Error.kind e = Malformed)
  | Ok _ -> check "short header: Ok" false

let blackout_body records =
  String.concat ""
    (varint64 (Int64.of_int (List.length records))
    :: List.map (fun (on, d) -> String.make 1 (Char.chr on) ^ varint64 d) records)

let fixture_blackouts () =
  let base = F.build spec in
  check "no blackout block" (Fst.blackouts (get (Fst.of_string base)) = []);
  let with_ body = Fst.of_string (base ^ section 2 body) in
  let expect name records expected =
    match with_ (blackout_body records) with
    | Ok t -> check name (Fst.blackouts t = expected)
    | Error e -> check (name ^ ": " ^ Fst.Error.to_string e) false
  in
  expect "empty" [] [];
  expect "deltas add up"
    [ (1, 0L); (0, 5L); (1, Int64.shift_left 1L 40) ]
    [ (0L, true); (5L, false); (Int64.add 5L (Int64.shift_left 1L 40), true) ];
  expect "nonzero is on" [ (255, 3L); (2, 1L); (0, 1L) ] [ (3L, true); (4L, true); (5L, false) ];
  expect "times are unsigned 64-bit"
    [ (0, Int64.max_int); (1, 1L); (0, -1L) ]
    [ (Int64.max_int, false); (Int64.min_int, true); (Int64.max_int, false) ];
  let kind name body k =
    match with_ body with
    | Error e -> check (name ^ ": " ^ Fst.Error.to_string e) (Fst.Error.kind e = k)
    | Ok _ -> check (name ^ ": Ok") false
  in
  kind "fewer records than counted" (varint64 2L ^ "\001\005") Fst.Error.Truncated;
  kind "huge record count" (varint64 0xFFFF_FFFFL) Malformed;
  kind "truncated delta" (varint64 1L ^ "\001\128") Truncated

let geometry_block ~uclen ~count data = section 3 (u64 uclen ^ u64 count ^ data)

let fixture_geometry () =
  let base = F.build spec in
  check "no geometry block" (Fst.geometry (get (Fst.of_string base)) = Ok [||]);
  let stored widths =
    let data = String.concat "" (List.map varint64 widths) in
    geometry_block
      ~uclen:(Int64.of_int (String.length data))
      ~count:(Int64.of_int (List.length widths))
      data
  in
  let geometry s = Fst.geometry (get (Fst.of_string s)) in
  let widths = [ 1L; 8L; 0L; 0xFFFF_FFFFL; 300L ] in
  check "stored widths" (geometry (base ^ stored widths) = Ok (Array.of_list widths));
  check "no widths" (geometry (base ^ stored []) = Ok [||]);
  check "32 bits only"
    (geometry (base ^ stored [ 0x1_0000_0005L; 0xFFFF_FFFF_FFFF_FFFFL ]) = Ok [| 5L; 0xFFFF_FFFFL |]);
  let kind name s k =
    match geometry s with
    | Error e -> check (name ^ ": " ^ Fst.Error.to_string e) (Fst.Error.kind e = k)
    | Ok _ -> check (name ^ ": Ok") false
  in
  kind "more entries than bytes" (base ^ geometry_block ~uclen:2L ~count:3L "\001\001") Malformed;
  kind "entries past the data"
    (base ^ geometry_block ~uclen:3L ~count:3L "\001\001\001\001")
    Decompression;
  kind "truncated width" (base ^ geometry_block ~uclen:2L ~count:2L "\001\128") Truncated;
  kind "huge entry count"
    (base ^ geometry_block ~uclen:2L ~count:Int64.max_int "\001\001")
    Limit_exceeded;
  check "the last geometry block wins"
    (let s = base ^ stored [ 7L ] ^ stored [ 9L ] in
     geometry s = Ok [| 9L |])

let () =
  corpus Sys.argv.(1);
  fixture_header ();
  fixture_blackouts ();
  fixture_geometry ();
  if !failures > 0 then exit 1
