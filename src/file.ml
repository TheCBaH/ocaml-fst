[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic, OpenAI"]

(* Opening a file and indexing its blocks, as libfst's fstReaderInit does. *)

type source = {
  length : int64;
  read : int64 -> int -> string;  (** [read pos len]: exactly [len] bytes, or fewer at EOF. *)
  close : unit -> unit;
}

let bl_hdr = 0
let bl_vcdata = 1
let bl_blackout = 2
let bl_geom = 3
let bl_hier = 4
let bl_vcdata_dyn_alias = 5
let bl_hier_lz4 = 6
let bl_hier_lz4duo = 7
let bl_vcdata_dyn_alias2 = 8
let bl_zwrapper = 254
let bl_skip = 255
let is_vc t = t = bl_vcdata || t = bl_vcdata_dyn_alias || t = bl_vcdata_dyn_alias2

type block = { typ : int; pos : int64; seclen : int64 }

type t = {
  src : source;
  header : Header.t;
  vc_blocks : block list;  (** The ones libfst iterates: before the first SKIP block. *)
  vc_section_count : int;
  geometry : block option;
  hier : block option;
  blackouts : (int64 * bool) array;
}

let of_string s =
  let length = Int64.of_int (String.length s) in
  let read pos len =
    if Int64.compare pos 0L < 0 || Int64.compare pos length >= 0 then ""
    else
      let p = Int64.to_int pos in
      String.sub s p (min len (String.length s - p))
  in
  { length; read; close = ignore }

let of_channel ic =
  let length = In_channel.length ic in
  let read pos len =
    if Int64.compare pos 0L < 0 || Int64.compare pos length >= 0 then ""
    else begin
      let len = Int64.to_int (min (Int64.of_int len) (Int64.sub length pos)) in
      In_channel.seek ic pos;
      match In_channel.really_input_string ic len with
      | Some s -> s
      | None -> Error.fail Io pos "short read"
    end
  in
  { length; read; close = (fun () -> close_in_noerr ic) }

let read_exact src pos len what =
  if len < 0 || len > Sys.max_string_length then
    Error.fail Limit_exceeded pos "%s: %d bytes" what len;
  let s = src.read pos len in
  if String.length s < len then Error.fail Truncated pos "%s" what;
  s

(* The block's bytes after the type byte: its length field included, so that offsets inside
   match libfst's [blkpos]-relative arithmetic. *)
let block_data src b what =
  let len = Int64.to_int (min b.seclen (Int64.of_int max_int)) in
  read_exact src (Int64.add b.pos 1L) len what

let gunzip_file src =
  let what = "compressed wrapper" in
  let h = read_exact src 0L 17 what in
  let r = Bin_reader.create h in
  ignore (Bin_reader.u8 r what);
  let seclen = Bin_reader.u64 r what in
  let uclen = Bin_reader.to_int r what (Bin_reader.u64 r what) in
  if Int64.equal seclen 0L then Error.fail Malformed 1L "%s: unfinished" what;
  let rest = Int64.to_int (Int64.sub src.length 17L) in
  let data = read_exact src 0L (17 + rest) what in
  Codec.gzip ~what ~offset:17L data ~off:17 ~len:rest ~size:uclen

let parse_blackouts data b =
  let what = "blackout" in
  let r = Bin_reader.create ~base:(Int64.add b.pos 1L) ~pos:8 data in
  let n = Bin_reader.varint32_int r what in
  if n > Bin_reader.remaining r then Error.fail Malformed b.pos "%s: %d records" what n;
  let t = ref 0L in
  Array.init n (fun _ ->
      let on = Bin_reader.u8 r what <> 0 in
      t := Int64.add !t (Bin_reader.varint r what);
      (!t, on))

let scan src =
  let first = src.read 0L 1 in
  let src = if first = "\xfe" then of_string (gunzip_file src) else src in
  let header = ref None and incomplete = ref false in
  let vc = ref [] and vc_count = ref 0 and skip_seen = ref false in
  let geometry = ref None and hier = ref None and hier_kinds = ref (false, false) in
  let blackouts = ref [||] in
  let pos = ref 0L in
  let stop = ref false in
  while (not !stop) && Int64.compare !pos src.length < 0 do
    let h = src.read !pos 9 in
    if String.length h = 0 then stop := true
    else begin
      let typ = Char.code h.[0] in
      if String.length h < 9 then Error.fail Truncated !pos "block header";
      let seclen = Bin_reader.u64_at h 1 in
      if Int64.compare seclen 0L < 0 || Int64.compare seclen src.length > 0 then
        Error.fail Malformed !pos "block length %Lu" seclen;
      if !incomplete && Int64.equal seclen 0L then stop := true
      else if !header = None && typ <> bl_hdr then stop := true
      else begin
        let b = { typ; pos = !pos; seclen } in
        if typ = bl_hdr then begin
          if !header = None then begin
            let data = block_data src b "header" in
            let hd = Header.parse (Bin_reader.create ~base:(Int64.add b.pos 1L) ~pos:8 data) in
            incomplete := Int64.equal hd.start_time 0L && Int64.equal hd.end_time 0L;
            header := Some hd
          end
        end
        else if is_vc typ then begin
          incr vc_count;
          if not (!skip_seen || Int64.equal seclen 0L) then vc := b :: !vc else skip_seen := true
        end
        else if typ = bl_skip then skip_seen := true
        else if typ = bl_geom then (if not !incomplete then geometry := Some b)
        else if typ = bl_hier then begin
          hier := Some b;
          hier_kinds := (true, snd !hier_kinds)
        end
        else if typ = bl_hier_lz4 || typ = bl_hier_lz4duo then begin
          hier := Some b;
          hier_kinds := (fst !hier_kinds, true)
        end
        else if typ = bl_blackout then blackouts := parse_blackouts (block_data src b "blackout") b;
        pos := Int64.add !pos (Int64.add 1L seclen)
      end
    end
  done;
  match !header with
  | None -> Error.fail Malformed 0L "no FST header"
  | Some header ->
      let hier = match !hier_kinds with true, true -> None | _ -> !hier in
      {
        src;
        header;
        vc_blocks = List.rev !vc;
        vc_section_count = !vc_count;
        geometry = !geometry;
        hier;
        blackouts = !blackouts;
      }

(* Widths per handle: reals are 0, zero-length variables 0xFFFFFFFF. *)
let geometry t =
  match t.geometry with
  | None -> [||]
  | Some b ->
      let what = "geometry" in
      let data = block_data t.src b what in
      let base = Int64.add b.pos 1L in
      let r = Bin_reader.create ~base ~pos:8 data in
      let uclen = Bin_reader.to_int r what (Bin_reader.u64 r what) in
      let count = Bin_reader.to_int r what (Bin_reader.u64 r what) in
      let clen = String.length data - 24 in
      let off = Int64.add base 24L in
      let u =
        if clen <> uclen then Codec.zlib ~what ~offset:off data ~off:24 ~len:clen ~size:uclen
        else String.sub data 24 uclen
      in
      if count > String.length u then Error.fail Malformed b.pos "%s: %d entries" what count;
      let r = Bin_reader.create ~base:off u in
      Array.init count (fun _ -> Bin_reader.varint32 r what)

let hierarchy_data t =
  let what = "hierarchy" in
  match t.hier with
  | None -> Error.fail Malformed (-1L) "no hierarchy block"
  | Some b ->
      let data = block_data t.src b what in
      let base = Int64.add b.pos 1L in
      let r = Bin_reader.create ~base ~pos:8 data in
      let uclen = Bin_reader.to_int r what (Bin_reader.u64 r what) in
      let off = Int64.add base 16L in
      let clen = String.length data - 16 in
      if b.typ = bl_hier then Codec.gzip ~what ~offset:off data ~off:16 ~len:clen ~size:uclen
      else if b.typ = bl_hier_lz4 then
        Codec.lz4 ~what ~offset:off data ~off:16 ~len:clen ~size:uclen
      else begin
        let r = Bin_reader.create ~base ~pos:16 data in
        let uclen2 = Bin_reader.varint_int r what in
        let p = r.pos in
        let mid = Codec.lz4 ~what ~offset:off data ~off:p ~len:(16 + clen - p) ~size:uclen2 in
        Codec.lz4 ~what ~offset:off mid ~off:0 ~len:uclen2 ~size:uclen
      end

(* A value change block's waves compression: ['Z'], ['F'] or ['4']. *)
let packtype t b =
  let what = "value change block" in
  let data = block_data t.src b what in
  let r = Bin_reader.create ~base:(Int64.add b.pos 1L) ~pos:32 data in
  ignore (Bin_reader.varint r what);
  let frame_clen = Bin_reader.varint_int r what in
  ignore (Bin_reader.varint r what);
  Bin_reader.skip r frame_clen what;
  ignore (Bin_reader.varint r what);
  Char.chr (Bin_reader.u8 r what)
