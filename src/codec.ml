[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic, OpenAI"]

(* Decompressors for the FST sections. Every caller knows the decoded size, which must be met
   exactly; it is first checked against the format's best compression ratio, so a corrupt size
   never becomes a large allocation. *)

let ratio_check ~what ~offset ~ratio ~src_len size =
  if size < 0 || size > Sys.max_string_length || (size - 64) / ratio > src_len then
    Error.fail Limit_exceeded offset "%s: %d bytes cannot decode to %d" what src_len size

let size_check ~what ~offset ~size got =
  if got <> size then
    Error.fail Decompression offset "%s: decoded %d bytes, expected %d" what got size

(* zlib's deflate tops out near 1032:1. *)
let zlib_ratio = 1032

let zlib ~what ~offset s ~off ~len ~size =
  ratio_check ~what ~offset ~ratio:zlib_ratio ~src_len:len size;
  (* zipc 0.2.0 reads the Adler-32 at [len - 4], ignoring [start]: pass the exact range. *)
  let s = if off = 0 && len = String.length s then s else String.sub s off len in
  match Zipc_deflate.zlib_decompress ~decompressed_size:size s with
  | Ok (d, _) ->
      size_check ~what ~offset ~size (String.length d);
      d
  | Error (_, msg) -> Error.fail Decompression offset "%s: zlib: %s" what msg

(* A gzip member (RFC 1952) as written by zlib's gzwrite. Like gzread, data that does not start
   with the gzip magic is taken as stored. *)
let gzip ~what ~offset s ~off ~len ~size =
  let r =
    Bin_reader.create ~base:(Int64.sub offset (Int64.of_int off)) ~pos:off ~lim:(off + len) s
  in
  if len < 2 || s.[off] <> '\x1f' || s.[off + 1] <> '\x8b' then begin
    if len < size then Bin_reader.truncated r what;
    String.sub s off size
  end
  else begin
    Bin_reader.skip r 2 what;
    if Bin_reader.u8 r what <> 8 then Error.fail Unsupported offset "%s: gzip method" what;
    let flags = Bin_reader.u8 r what in
    Bin_reader.skip r 6 what;
    if flags land 4 <> 0 then begin
      let lo = Bin_reader.u8 r what in
      let hi = Bin_reader.u8 r what in
      Bin_reader.skip r (lo lor (hi lsl 8)) what
    end;
    if flags land 8 <> 0 then ignore (Bin_reader.cstring r ~max:0 what);
    if flags land 16 <> 0 then ignore (Bin_reader.cstring r ~max:0 what);
    if flags land 2 <> 0 then Bin_reader.skip r 2 what;
    let start = r.pos in
    let dlen = off + len - 8 - start in
    if dlen < 0 then Bin_reader.truncated r what;
    ratio_check ~what ~offset ~ratio:zlib_ratio ~src_len:dlen size;
    match Zipc_deflate.inflate_and_crc_32 ~decompressed_size:size ~start ~len:dlen s with
    | Error msg -> Error.fail Decompression offset "%s: gzip: %s" what msg
    | Ok (d, crc) ->
        size_check ~what ~offset ~size (String.length d);
        let le32 p =
          Int32.logor
            (Int32.of_int (Char.code s.[p] lor (Char.code s.[p + 1] lsl 8)))
            (Int32.shift_left
               (Int32.of_int (Char.code s.[p + 2] lor (Char.code s.[p + 3] lsl 8)))
               16)
        in
        (match Zipc_deflate.Crc_32.check ~expect:(le32 (off + len - 8)) ~found:crc with
        | Ok () -> ()
        | Error msg -> Error.fail Decompression offset "%s: gzip: %s" what msg);
        d
  end

(* One LZ4 match token adds at most 255 bytes per input byte. *)
let lz_ratio = 256

let lz4_into ~what ~offset s ~off ~len dst ~dst_off ~size =
  ratio_check ~what ~offset ~ratio:lz_ratio ~src_len:len size;
  match
    Unlz4.Block.decompress_into ~src:(Bytes.unsafe_of_string s) ~src_off:off ~src_len:len ~dst
      ~dst_off ~dst_len:size ()
  with
  | Ok n -> size_check ~what ~offset ~size n
  | Error e -> Error.fail Decompression offset "%s: %s" what (Unlz4.Error.message e)

let lz4 ~what ~offset s ~off ~len ~size =
  ratio_check ~what ~offset ~ratio:lz_ratio ~src_len:len size;
  let dst = Bytes.create size in
  lz4_into ~what ~offset s ~off ~len dst ~dst_off:0 ~size;
  Bytes.unsafe_to_string dst

let fastlz_into ~what ~offset s ~off ~len dst ~dst_off ~size =
  ratio_check ~what ~offset ~ratio:lz_ratio ~src_len:len size;
  match Fastlz.decompress_into s ~off ~len dst ~dst_off ~dst_len:size with
  | Some n -> size_check ~what ~offset ~size n
  | None -> Error.fail Decompression offset "%s: corrupt FastLZ data" what
