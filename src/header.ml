[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic, OpenAI"]

type t = {
  start_time : int64;
  end_time : int64;
  little_endian : bool;
  writer_memory : int64;
  scope_count : int64;
  var_count : int64;
  max_handle : int64;
  vc_section_count : int64;
  timescale : int;
  version : string;
  date : string;
  file_type : int;
  timezero : int64;
}

let version_size = 128
let date_size = 119
let length = 329

(* The writer's 2.718... in its own byte order. *)
let endian_test = Int64.bits_of_float 2.7182818284590452354

let le64_at s pos =
  let r = ref 0L in
  for i = 7 downto 0 do
    r := Int64.logor (Int64.shift_left !r 8) (Int64.of_int (Char.code s.[pos + i]))
  done;
  !r

let until_nul s = match String.index_opt s '\000' with Some i -> String.sub s 0 i | None -> s

(* [r] is positioned after the block type and length. *)
let parse r =
  let what = "header" in
  let start_time = Bin_reader.u64 r what in
  let end_time = Bin_reader.u64 r what in
  Bin_reader.need r 8 what;
  let off = Bin_reader.file_offset r in
  let little_endian =
    if Int64.equal (le64_at r.s r.pos) endian_test then true
    else if Int64.equal (Bin_reader.u64_at r.s r.pos) endian_test then false
    else Error.fail Malformed off "header: bad endianness test value"
  in
  Bin_reader.skip r 8 what;
  let writer_memory = Bin_reader.u64 r what in
  let scope_count = Bin_reader.u64 r what in
  let var_count = Bin_reader.u64 r what in
  let max_handle = Bin_reader.u64 r what in
  let vc_section_count = Bin_reader.u64 r what in
  let ts = Bin_reader.u8 r what in
  let timescale = if ts > 127 then ts - 256 else ts in
  let version = until_nul (Bin_reader.string r version_size what) in
  let date = until_nul (Bin_reader.string r date_size what) in
  let file_type = Bin_reader.u8 r what in
  let timezero = Bin_reader.u64 r what in
  {
    start_time;
    end_time;
    little_endian;
    writer_memory;
    scope_count;
    var_count;
    max_handle;
    vc_section_count;
    timescale;
    version;
    date;
    file_type;
    timezero;
  }

let float_at ~little_endian s pos =
  Int64.float_of_bits (if little_endian then le64_at s pos else Bin_reader.u64_at s pos)

let float_at_bytes ~little_endian b pos = float_at ~little_endian (Bytes.unsafe_to_string b) pos
