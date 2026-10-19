[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic, OpenAI"]

(* VCD exactly as libfst writes it for fst2vcd: fstReaderProcessHier for the header and
   definitions, fstReaderIterBlocks2 for the values. *)

let attr_type_names = [| "misc"; "array"; "enum"; "class" |]
let array_type_names = [| "none"; "unpacked"; "packed"; "sparse" |]

(* libfst's table stops at unsigned_byte (13) although FST_EV_REG (14) and FST_EV_TIME (15) are
   valid subtypes; it reads past its end for those, so they are named after the enum here. *)
let enum_value_type_names =
  [| "integer"; "bit"; "logic"; "int"; "shortint"; "longint"; "byte"; "unsigned_integer";
     "unsigned_bit"; "unsigned_logic"; "unsigned_int"; "unsigned_shortint"; "unsigned_longint";
     "unsigned_byte"; "reg"; "time" |]
[@@ocamlformat "disable"]

let pack_type_names = [| "none"; "unpacked"; "packed"; "tagged_packed" |]

(* fstVcdID: base 94 from '!', least significant first, for 1-based handles. *)
let id n =
  let b = Buffer.create 4 in
  let v = ref n in
  while !v > 0 do
    decr v;
    Buffer.add_char b (Char.chr (33 + (!v mod 94)));
    v := !v / 94
  done;
  Buffer.contents b

let timescale ts =
  if ts > 2 || ts < -21 then "1ns"
  else
    let base = if ts >= 0 then 0 else -3 * ((2 - ts) / 3) in
    let unit =
      match base with
      | 0 -> ""
      | -3 -> "m"
      | -6 -> "u"
      | -9 -> "n"
      | -12 -> "p"
      | -15 -> "f"
      | -18 -> "a"
      | _ -> "z"
    in
    let scale = match ts - base with 0 -> "1" | 1 -> "10" | _ -> "100" in
    scale ^ unit ^ "s"

(* fstGetVarint64 over the bytes of a source-stem attribute's name. *)
let varint_of_name s =
  let n = String.length s in
  let rec last i = if i < n && Char.code s.[i] land 0x80 <> 0 then last (i + 1) else i in
  let e = last 0 in
  let r = ref 0L in
  for i = e downto 0 do
    let c = if i < n then Char.code s.[i] else 0 in
    r := Int64.logor (Int64.shift_left !r 7) (Int64.of_int (c land 0x7f))
  done;
  !r

(* fstUtilityBinToEsc. *)
let add_escaped b s off len =
  for i = off to off + len - 1 do
    let c = Bytes.unsafe_get s i in
    match c with
    | '\007' -> Buffer.add_string b "\\a"
    | '\b' -> Buffer.add_string b "\\b"
    | '\012' -> Buffer.add_string b "\\f"
    | '\n' -> Buffer.add_string b "\\n"
    | '\r' -> Buffer.add_string b "\\r"
    | '\t' -> Buffer.add_string b "\\t"
    | '\011' -> Buffer.add_string b "\\v"
    | '\'' -> Buffer.add_string b "\\'"
    | '"' -> Buffer.add_string b "\\\""
    | '\\' -> Buffer.add_string b "\\\\"
    | '?' -> Buffer.add_string b "\\?"
    | c when c > ' ' && c <= '~' -> Buffer.add_char b c
    | c ->
        let v = Char.code c in
        Buffer.add_char b '\\';
        Buffer.add_char b (Char.chr (48 + (v / 64)));
        Buffer.add_char b (Char.chr (48 + (v land 63 / 8)));
        Buffer.add_char b (Char.chr (48 + (v land 7)))
  done

(* printf's %.16g, including glibc's spelling of the non-finite values. *)
let g16 d =
  if Float.is_nan d then if Int64.compare (Int64.bits_of_float d) 0L < 0 then "-nan" else "nan"
  else if d = Float.infinity then "inf"
  else if d = Float.neg_infinity then "-inf"
  else Printf.sprintf "%.16g" d

module Growable = struct
  type 'a t = { mutable a : 'a array; mutable n : int; default : 'a }

  let create default = { a = Array.make 1024 default; n = 0; default }

  let push t v =
    if t.n = Array.length t.a then begin
      let a = Array.make (2 * t.n) t.default in
      Array.blit t.a 0 a 0 t.n;
      t.a <- a
    end;
    t.a.(t.n) <- v;
    t.n <- t.n + 1

  let to_array t = Array.sub t.a 0 t.n
end

(* The [$var] size: reals by precision, ports by their 3n+2 encoding. *)
let var_length ~var_type ~length =
  let len =
    if Hier.is_real var_type then if var_type = Hier.vt_shortreal then 32 else 64 else length
  in
  if var_type = Hier.vt_port then (len - 2) land Vc.mask32 / 3 else len

let definitions ~extensions (file : File.t) b =
  let h = file.header in
  Printf.bprintf b "$date\n\t%s\n$end\n" h.date;
  Printf.bprintf b "$version\n\t%s\n$end\n" h.version;
  if not (Int64.equal h.timezero 0L) then Printf.bprintf b "$timezero\n\t%Ld\n$end\n" h.timezero;
  Printf.bprintf b "$timescale\n\t%s\n$end\n" (timescale h.timescale);
  let lens = Growable.create 0
  and reals = Growable.create false
  and ports = Growable.create false in
  let data = File.hierarchy_data file in
  let base = match file.hier with Some blk -> blk.pos | None -> -1L in
  Hier.fold ~base data ~init:() ~f:(fun () e ->
      match e with
      | Hier.Scope { scope_type; name; _ } ->
          let st = if scope_type >= Array.length Hier.scope_type_names then 0 else scope_type in
          Printf.bprintf b "$scope %s %s $end\n" Hier.scope_type_names.(st) name
      | Upscope -> Buffer.add_string b "$upscope $end\n"
      | Attr_begin { attr_type; subtype; name; arg } ->
          let name = if name = "" then "\"\"" else name in
          if extensions then
            let line kind names =
              let sub = if subtype >= Array.length names then 0 else subtype in
              Printf.bprintf b "$attrbegin %s %s %s %Ld $end\n" kind names.(sub) name arg
            in
            if attr_type = 1 then line attr_type_names.(1) array_type_names
            else if attr_type = 2 then line attr_type_names.(2) enum_value_type_names
            else if attr_type = 3 then line attr_type_names.(3) pack_type_names
            else if subtype = 0 then Printf.bprintf b "$comment\n\t%s\n$end\n" name
            else if subtype = 4 || subtype = 5 then
              Printf.bprintf b "$attrbegin misc %02x %Ld %Ld $end\n" subtype (varint_of_name name)
                arg
            else Printf.bprintf b "$attrbegin misc %02x %s %Ld $end\n" subtype name arg
      | Attr_end -> if extensions then Buffer.add_string b "$attrend $end\n"
      | Var { var_type; name; length; handle; alias; _ } ->
          let real = Hier.is_real var_type in
          if not alias then begin
            Growable.push lens length;
            Growable.push reals real;
            Growable.push ports (var_type = Hier.vt_port)
          end;
          Printf.bprintf b "$var %s %d %s %s $end\n" Hier.var_type_names.(var_type)
            (var_length ~var_type ~length) (id handle) name);
  Buffer.add_string b "$enddefinitions $end\n";
  ({ Vc.lens = Growable.to_array lens; reals = Growable.to_array reals }, Growable.to_array ports)

let write ~extensions file b ~flush =
  let signals, ports = definitions ~extensions file b in
  let ids = Array.init (Array.length signals.lens) (fun i -> id (i + 1)) in
  let dumpvars = ref 0 in
  let blackouts = file.blackouts and cur = ref 0 in
  let new_time t =
    if !dumpvars = 1 then begin
      Buffer.add_string b "$end\n";
      dumpvars := 2
    end;
    Printf.bprintf b "#%Lu\n" t;
    if !dumpvars = 0 then begin
      Buffer.add_string b "$dumpvars\n";
      dumpvars := 1
    end
  in
  let blackout t =
    if !cur < Array.length blackouts then begin
      let bt, on = blackouts.(!cur) in
      if Int64.equal bt t then begin
        Buffer.add_string b (if on then "$dumpon $end\n" else "$dumpoff $end\n");
        incr cur
      end
    end
  in
  let tail h =
    Buffer.add_char b ' ';
    Buffer.add_string b ids.(h - 1);
    Buffer.add_char b '\n'
  in
  let sink =
    {
      Vc.time =
        (fun t ->
          if Buffer.length b >= 65536 then flush b;
          new_time t;
          blackout t);
      frame =
        (fun t ->
          if not (Int64.equal t 0L) then new_time t;
          blackout t);
      bit =
        (fun h c ->
          Buffer.add_char b c;
          Buffer.add_string b ids.(h - 1);
          Buffer.add_char b '\n');
      vector =
        (fun h s off len ->
          Buffer.add_char b (if ports.(h - 1) then 'p' else 'b');
          Buffer.add_subbytes b s off len;
          tail h);
      real =
        (fun h d ->
          Buffer.add_char b 'r';
          Buffer.add_string b (g16 d);
          tail h);
      varlen =
        (fun h s off len ->
          Buffer.add_char b 's';
          add_escaped b s off len;
          tail h);
    }
  in
  Vc.iter ~file ~signals ~sink ();
  flush b
