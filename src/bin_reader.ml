[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic, OpenAI"]

(* Bounded reads over a string holding part of the file. [base] is the file offset of [s.[0]],
   so errors report file offsets. Integers are big-endian and varints are LEB128, as in FST. *)

type t = { s : string; base : int64; mutable pos : int; lim : int }

let create ?(base = 0L) ?(pos = 0) ?lim s =
  let lim = match lim with Some l -> l | None -> String.length s in
  { s; base; pos; lim }

let file_offset t = Int64.add t.base (Int64.of_int t.pos)
let truncated t what = Error.fail Truncated (file_offset t) "%s" what
let remaining t = t.lim - t.pos
let need t n what = if n < 0 || n > t.lim - t.pos then truncated t what

let u8 t what =
  need t 1 what;
  let c = Char.code (String.unsafe_get t.s t.pos) in
  t.pos <- t.pos + 1;
  c

let u64_at s pos =
  let r = ref 0L in
  for i = 0 to 7 do
    r := Int64.logor (Int64.shift_left !r 8) (Int64.of_int (Char.code s.[pos + i]))
  done;
  !r

let u64 t what =
  need t 8 what;
  let v = u64_at t.s t.pos in
  t.pos <- t.pos + 8;
  v

let skip t n what =
  need t n what;
  t.pos <- t.pos + n

let string t n what =
  need t n what;
  let v = String.sub t.s t.pos n in
  t.pos <- t.pos + n;
  v

(* LEB128 into 64 bits, at most 10 bytes; bits past 64 are dropped as libfst does. *)
let varint t what =
  let rec go acc shift =
    if shift > 63 then Error.fail Malformed (file_offset t) "%s: varint longer than 10 bytes" what;
    let b = u8 t what in
    let acc =
      if shift < 64 then Int64.logor acc (Int64.shift_left (Int64.of_int (b land 0x7f)) shift)
      else acc
    in
    if b land 0x80 <> 0 then go acc (shift + 7) else acc
  in
  go 0L 0

(* libfst reads these with 32-bit accumulators. *)
let varint32 t what = Int64.logand (varint t what) 0xFFFF_FFFFL

let to_int t what v =
  if Int64.compare v 0L < 0 || Int64.compare v (Int64.of_int max_int) > 0 then
    Error.fail Limit_exceeded (file_offset t) "%s: %Lu does not fit in an int" what v
  else Int64.to_int v

let varint_int t what = to_int t what (varint t what)
let varint32_int t what = to_int t what (varint32 t what)

(* A NUL-terminated string, of which at most [max] bytes are kept. *)
let cstring t ~max what =
  match String.index_from_opt t.s t.pos '\000' with
  | Some e when e < t.lim ->
      let n = e - t.pos in
      let v = String.sub t.s t.pos (if n > max then max else n) in
      t.pos <- e + 1;
      v
  | _ -> truncated t what
