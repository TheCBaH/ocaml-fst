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

(* fstUtilityBinToEsc into [dst] at [pos]: at most four bytes per input byte. Returns the end. *)
let escape_into dst pos s off len =
  let pos = ref pos in
  let put c =
    Bytes.unsafe_set dst !pos c;
    incr pos
  in
  let put2 a b =
    put a;
    put b
  in
  for i = off to off + len - 1 do
    let c = Bytes.unsafe_get s i in
    match c with
    | '\007' -> put2 '\\' 'a'
    | '\b' -> put2 '\\' 'b'
    | '\012' -> put2 '\\' 'f'
    | '\n' -> put2 '\\' 'n'
    | '\r' -> put2 '\\' 'r'
    | '\t' -> put2 '\\' 't'
    | '\011' -> put2 '\\' 'v'
    | '\'' -> put2 '\\' '\''
    | '"' -> put2 '\\' '"'
    | '\\' -> put2 '\\' '\\'
    | '?' -> put2 '\\' '?'
    | c when c > ' ' && c <= '~' -> put c
    | c ->
        let v = Char.code c in
        put '\\';
        put (Char.chr (48 + (v / 64)));
        put (Char.chr (48 + (v land 63 / 8)));
        put (Char.chr (48 + (v land 7)))
  done;
  !pos

(* The input is escaped in pieces so that the output of one never exceeds [4 * chunk]. *)
let chunk = 4096

let add_escaped b s off len =
  let tmp = Bytes.create (4 * min len chunk) in
  let i = ref 0 in
  while !i < len do
    let n = min chunk (len - !i) in
    Buffer.add_subbytes b tmp 0 (escape_into tmp 0 s (off + !i) n);
    i := !i + n
  done

(* printf's %.16g, including glibc's spelling of the non-finite values. *)
let g16 d =
  if Float.is_nan d then if Int64.compare (Int64.bits_of_float d) 0L < 0 then "-nan" else "nan"
  else if d = Float.infinity then "inf"
  else if d = Float.neg_infinity then "-inf"
  else Printf.sprintf "%.16g" d

(* The [$var] size: reals by precision, ports by their 3n+2 encoding. *)
let var_length ~var_type ~length =
  let len =
    if Hier.is_real var_type then if var_type = Hier.vt_shortreal then 32 else 64 else length
  in
  if var_type = Hier.vt_port then (len - 2) land Vc.mask32 / 3 else len

type definition =
  | Date of string
  | Version of string
  | Timezero of int64
  | Timescale of string
  | Scope of { scope_type : string; name : string }
  | Upscope
  | Comment of string
  | Attr_begin of { attr_type : string; subtype : string; name : string; arg : int64 }
  | Attr_end
  | Var of {
      var_type : string;
      kind : Hier.kind;
      size : int;
      id : string;
      handle : int;
      alias : bool;
      name : string;
    }
  | End_definitions

let adds b l = List.iter (Buffer.add_string b) l

let print_definition b = function
  | Date s -> adds b [ "$date\n\t"; s; "\n$end\n" ]
  | Version s -> adds b [ "$version\n\t"; s; "\n$end\n" ]
  | Timezero z -> adds b [ "$timezero\n\t"; Int64.to_string z; "\n$end\n" ]
  | Timescale s -> adds b [ "$timescale\n\t"; s; "\n$end\n" ]
  | Scope { scope_type; name } -> adds b [ "$scope "; scope_type; " "; name; " $end\n" ]
  | Upscope -> Buffer.add_string b "$upscope $end\n"
  | Comment s -> adds b [ "$comment\n\t"; s; "\n$end\n" ]
  | Attr_begin { attr_type; subtype; name; arg } ->
      adds b
        [ "$attrbegin "; attr_type; " "; subtype; " "; name; " "; Int64.to_string arg; " $end\n" ]
  | Attr_end -> Buffer.add_string b "$attrend $end\n"
  | Var { var_type; size; id; name; _ } ->
      Buffer.add_string b "$var ";
      Buffer.add_string b var_type;
      Buffer.add_char b ' ';
      Buffer.add_string b (string_of_int size);
      Buffer.add_char b ' ';
      Buffer.add_string b id;
      Buffer.add_char b ' ';
      Buffer.add_string b name;
      Buffer.add_string b " $end\n"
  | End_definitions -> Buffer.add_string b "$enddefinitions $end\n"

(* The definitions in the order [print_definition] writes them. [signals] is the file's, read
   first so that a closed or damaged file is reported before any definition is passed on. *)
let definitions ~extensions (file : File.t) ~signals f =
  let h = file.header in
  f (Date h.date);
  f (Version h.version);
  if not (Int64.equal h.timezero 0L) then f (Timezero h.timezero);
  f (Timescale (timescale h.timescale));
  let data = File.hierarchy_data file in
  Hier.fold ~handles:(Array.length signals) data ~init:() ~f:(fun () e ->
      match e with
      | Hier.Scope { scope_type; name; _ } ->
          let st = if scope_type >= Array.length Hier.scope_type_names then 0 else scope_type in
          f (Scope { scope_type = Hier.scope_type_names.(st); name })
      | Upscope -> f Upscope
      | Attr_begin { attr_type; subtype; name; arg } ->
          if extensions then begin
            let name = if name = "" then "\"\"" else name in
            let typed kind names =
              let sub = if subtype >= Array.length names then 0 else subtype in
              f (Attr_begin { attr_type = kind; subtype = names.(sub); name; arg })
            in
            if attr_type = 1 then typed attr_type_names.(1) array_type_names
            else if attr_type = 2 then typed attr_type_names.(2) enum_value_type_names
            else if attr_type = 3 then typed attr_type_names.(3) pack_type_names
            else if subtype = 0 then f (Comment name)
            else
              let name =
                if subtype = 4 || subtype = 5 then Int64.to_string (varint_of_name name) else name
              in
              f
                (Attr_begin
                   { attr_type = "misc"; subtype = Printf.sprintf "%02x" subtype; name; arg })
          end
      | Attr_end -> if extensions then f Attr_end
      | Var { var_type; name; length; handle; alias; _ } ->
          f
            (Var
               {
                 var_type = Hier.var_type_names.(var_type);
                 kind = Hier.kind_of ~var_type ~length;
                 size = var_length ~var_type ~length;
                 id = id handle;
                 handle;
                 alias;
                 name;
               }));
  f End_definitions

(* The output is formatted straight into [buf], and [emit buf 0 n] is called when it is full. Each
   writer is preceded by a check that there is room for it. *)
type out = {
  emit : bytes -> int -> int -> unit;
  mutable buf : bytes;
  mutable n : int;
  ids : string array;
  idmax : int;
}

let drain o =
  o.emit o.buf 0 o.n;
  o.n <- 0

(* Room for [k] more bytes. *)
let grow o k =
  drain o;
  if k > Bytes.length o.buf then o.buf <- Bytes.create k

let ensure o k = if k > Bytes.length o.buf - o.n then grow o k

let put_char o c =
  Bytes.unsafe_set o.buf o.n c;
  o.n <- o.n + 1

let put_string o s =
  let l = String.length s in
  Bytes.unsafe_blit_string s 0 o.buf o.n l;
  o.n <- o.n + l

let line o s =
  ensure o (String.length s);
  put_string o s

(* The id of handle [h] and a newline. *)
let put_id o h =
  let s = Array.unsafe_get o.ids (h - 1) in
  let n = o.n and buf = o.buf in
  for i = 0 to String.length s - 1 do
    Bytes.unsafe_set buf (n + i) (String.unsafe_get s i)
  done;
  Bytes.unsafe_set buf (n + String.length s) '\n';
  o.n <- n + String.length s + 1

let put_tail o h =
  put_char o ' ';
  put_id o h

let put_time o t =
  if Int64.compare t 0L >= 0 then begin
    ensure o 21;
    let digits = ref 1 and v = ref t in
    while Int64.compare !v 10L >= 0 do
      v := Int64.div !v 10L;
      incr digits
    done;
    put_char o '#';
    let v = ref t in
    for i = !digits - 1 downto 0 do
      Bytes.unsafe_set o.buf (o.n + i) (Char.unsafe_chr (48 + Int64.to_int (Int64.rem !v 10L)));
      v := Int64.div !v 10L
    done;
    o.n <- o.n + !digits;
    put_char o '\n'
  end
  else line o (Printf.sprintf "#%Lu\n" t)

type marker = Dumpvars | Dumpvars_end | Dumpon | Dumpoff

type sink = {
  definition : definition -> unit;
  time : int64 -> unit;
  frame : int64 -> unit;
  dump : marker -> unit;
  bit : int -> char -> unit;
  vector : int -> bytes -> int -> int -> unit;
  packed : (int -> bytes -> int -> int -> unit) option;
  port : int -> bytes -> int -> int -> unit;
  real : int -> float -> unit;
  varlen : int -> bytes -> int -> int -> unit;
}

let ignore_sink =
  {
    definition = ignore;
    time = ignore;
    frame = ignore;
    dump = ignore;
    bit = (fun _ _ -> ());
    vector = (fun _ _ _ _ -> ());
    packed = None;
    port = (fun _ _ _ _ -> ());
    real = (fun _ _ -> ());
    varlen = (fun _ _ _ _ -> ());
  }

type stage = Definitions | Blocks | Ended

type reader = {
  file : File.t;
  vc : Vc.reader;
  extensions : bool;
  signals : (Hier.kind * int) array;
  definition : definition -> unit;
  mutable stage : stage;
}

let reader ?(extensions = false) ?select ?max_bits ?(block = 0) (file : File.t) (s : sink) =
  let kinds = File.signals_exn file in
  let signals = Vc.tables kinds in
  let ports = Array.map (fun (k, _) -> k = Hier.Port) kinds in
  let has_ports = Array.exists Fun.id ports in
  let resume = block > 0 in
  let blackouts = file.blackouts in
  let dumpvars = ref (if resume then 2 else 0) and cur = ref 0 and first = ref resume in
  let new_time t =
    if !dumpvars = 1 then begin
      s.dump Dumpvars_end;
      dumpvars := 2
    end;
    s.time t;
    if !dumpvars = 0 then begin
      s.dump Dumpvars;
      dumpvars := 1
    end
  in
  let blackout t =
    if !cur < Array.length blackouts then begin
      let bt, on = blackouts.(!cur) in
      if Int64.equal bt t then begin
        s.dump (if on then Dumpon else Dumpoff);
        incr cur
      end
    end
  in
  let sink =
    {
      Vc.time =
        (fun t ->
          if !first then begin
            first := false;
            while !cur < Array.length blackouts && Int64.compare (fst blackouts.(!cur)) t < 0 do
              incr cur
            done
          end;
          new_time t;
          blackout t);
      frame =
        (fun t ->
          if resume then s.frame t
          else begin
            if not (Int64.equal t 0L) then new_time t;
            blackout t
          end);
      bit = s.bit;
      vector =
        (let vector = s.vector and port = s.port in
         if has_ports then fun h b off len ->
           if ports.(h - 1) then port h b off len else vector h b off len
         else vector);
      real = s.real;
      varlen = s.varlen;
    }
  in
  let packed =
    if not has_ports then s.packed
    else
      Option.map
        (fun packed h b off w ->
          if Array.unsafe_get ports (h - 1) then begin
            let sb = Bytes.create w in
            Vc.expand_into sb 0 b off w;
            s.port h sb 0 w
          end
          else packed h b off w)
        s.packed
  in
  let vc = Vc.start_at ?select ?packed ?max_bits ~file ~signals ~sink ~block () in
  { file; vc; extensions; signals = kinds; definition = s.definition; stage = Definitions }

let step r =
  File.check_open r.file;
  match r.stage with
  | Definitions ->
      r.stage <- Ended;
      definitions ~extensions:r.extensions r.file ~signals:r.signals r.definition;
      r.stage <- Blocks;
      true
  | Ended -> false
  | Blocks -> Vc.step r.vc

let events ?extensions ?select ?max_bits file sink =
  let r = reader ?extensions ?select ?max_bits file sink in
  while step r do
    ()
  done

let write ~extensions file ~emit =
  let defs = Buffer.create 131072 in
  let ids = Array.init (Array.length (File.signals_exn file)) (fun i -> id (i + 1)) in
  let idmax = Array.fold_left (fun m s -> max m (String.length s)) 0 ids in
  let o = { emit; buf = Bytes.create 65536; n = 0; ids; idmax } in
  let sink =
    {
      definition =
        (fun d ->
          print_definition defs d;
          match d with
          | End_definitions -> emit (Buffer.to_bytes defs) 0 (Buffer.length defs)
          | _ -> ());
      time = put_time o;
      frame = ignore;
      dump =
        (function
        | Dumpvars -> line o "$dumpvars\n"
        | Dumpvars_end -> line o "$end\n"
        | Dumpon -> line o "$dumpon $end\n"
        | Dumpoff -> line o "$dumpoff $end\n");
      bit =
        (fun h c ->
          ensure o (idmax + 2);
          put_char o c;
          put_id o h);
      vector =
        (fun h s off len ->
          ensure o (len + idmax + 3);
          put_char o 'b';
          Bytes.blit s off o.buf o.n len;
          o.n <- o.n + len;
          put_tail o h);
      packed =
        Some
          (fun h s off len ->
            ensure o (len + idmax + 3);
            put_char o 'b';
            Vc.expand_into o.buf o.n s off len;
            o.n <- o.n + len;
            put_tail o h);
      port =
        (fun h s off len ->
          ensure o (len + idmax + 3);
          put_char o 'p';
          Bytes.blit s off o.buf o.n len;
          o.n <- o.n + len;
          put_tail o h);
      real =
        (fun h d ->
          let g = g16 d in
          ensure o (String.length g + idmax + 3);
          put_char o 'r';
          put_string o g;
          put_tail o h);
      varlen =
        (fun h s off len ->
          ensure o 1;
          put_char o 's';
          let i = ref 0 in
          while !i < len do
            let n = min chunk (len - !i) in
            ensure o (4 * n);
            o.n <- escape_into o.buf o.n s (off + !i) n;
            i := !i + n
          done;
          ensure o (idmax + 2);
          put_tail o h);
    }
  in
  events ~extensions ~max_bits:(Sys.max_string_length - idmax - 3) file sink;
  drain o
