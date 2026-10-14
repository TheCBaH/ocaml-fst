[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic, OpenAI"]

(* Value change traversal, a port of libfst's fstReaderIterBlocks2: changes come out in the same
   order, which is what makes the VCD output identical. Each variable's changes in a block are
   decompressed into one traversal buffer; variables waiting on the same time index are chained
   LIFO through [tc_head] and [scatter]. *)

type sink = {
  time : int64 -> unit;
  frame : int64 -> unit;
  bit : int -> char -> unit;
  vector : int -> bytes -> int -> int -> unit;
  real : int -> float -> unit;
  varlen : int -> bytes -> int -> int -> unit;
}

(* What the traversal needs from the hierarchy: per handle (0-based), the declared length and
   whether it is a real. *)
type signals = { lens : int array; reals : bool array }

let rcv = "xzhuwl-?"

(* libfst's uint32_t arithmetic; on 32-bit hosts [int] has 31 bits and values that large are
   rejected instead. *)
let mask32 = if Sys.int_size > 32 then (1 lsl 32) - 1 else max_int

let to_int32 v =
  if Sys.int_size > 32 then (v land mask32) - if v land (1 lsl 31) <> 0 then 1 lsl 32 else 0 else v

(* A block is skipped when its chain table or traversal buffer is inconsistent, as libfst does
   with [goto block_err]; other corruption is an error. *)
exception Skip_block

let varint_at b pos lim what off =
  let rec go acc shift p =
    if p >= lim then Error.fail Truncated off "%s" what;
    let c = Char.code (Bytes.unsafe_get b p) in
    let acc = if shift < 32 then acc lor ((c land 0x7f) lsl shift) else acc in
    if c land 0x80 <> 0 then go acc (shift + 7) (p + 1)
    else if acc < 0 then Error.fail Limit_exceeded off "%s: varint" what
    else (acc land mask32, p + 1 - pos)
  in
  go 0 0 pos

(* Over a string with a NUL past the end, as libfst's chain table buffer has. *)
let svarint_str s pos =
  let rec go acc shift p =
    let c = Char.code s.[p] in
    let acc = Int64.logor acc (Int64.shift_left (Int64.of_int (c land 0x7f)) shift) in
    let shift = shift + 7 in
    if c land 0x80 <> 0 && p + 1 < String.length s then go acc shift (p + 1)
    else
      let acc =
        if shift < 64 && c land 0x40 <> 0 then
          Int64.logor acc (Int64.neg (Int64.shift_left 1L shift))
        else acc
      in
      (acc, p + 1 - pos)
  in
  go 0L 0 pos

let varint32_str s pos =
  let rec go acc shift p =
    let c = Char.code s.[p] in
    let acc = if shift < 32 then acc lor ((c land 0x7f) lsl shift) else acc in
    if c land 0x80 <> 0 && p + 1 < String.length s then go acc (shift + 7) (p + 1)
    else (acc land mask32, p + 1 - pos)
  in
  go 0 0 pos

(* fstReaderParseChainTable. [cm] ends with an extra NUL. Lengths of alias entries are
   negative; [lengths] keeps stale entries from earlier blocks, as libfst's does. *)
let parse_chain cm ~alias2 ~vc_maxhandle ~vc_len table lengths =
  let clen = String.length cm - 1 in
  let pnt = ref 0 and idx = ref 0 and pidx = ref 0 and pval = ref 0 and prev_alias = ref 0 in
  let room n = !idx + n <= vc_maxhandle in
  let positive v =
    if not (room 1) then raise Skip_block;
    pval := !pval + v;
    table.(!idx) <- !pval;
    if !idx > 0 then lengths.(!pidx) <- !pval - table.(!pidx);
    pidx := !idx;
    incr idx
  in
  let zeros n =
    if not (room n) then raise Skip_block;
    for _ = 1 to n do
      table.(!idx) <- 0;
      incr idx
    done
  in
  let alias l =
    if not (room 1) then raise Skip_block;
    table.(!idx) <- 0;
    lengths.(!idx) <- l;
    incr idx
  in
  while !pnt < clen do
    if alias2 then begin
      if Char.code cm.[!pnt] land 1 <> 0 then begin
        let v, skip = svarint_str cm !pnt in
        let shval = Int64.shift_right v 1 in
        if Int64.compare shval 0L > 0 then positive (Int64.to_int shval)
        else if Int64.compare shval 0L < 0 then begin
          prev_alias := to_int32 (Int64.to_int shval);
          alias !prev_alias
        end
        else alias !prev_alias;
        pnt := !pnt + skip
      end
      else begin
        let v, skip = varint32_str cm !pnt in
        zeros (v lsr 1);
        pnt := !pnt + skip
      end
    end
    else begin
      let v, skip = varint32_str cm !pnt in
      if v = 0 then begin
        if not (room 1) then raise Skip_block;
        pnt := !pnt + skip;
        if !pnt >= clen then raise Skip_block;
        let v, skip = varint32_str cm !pnt in
        alias (to_int32 (-v));
        pnt := !pnt + skip
      end
      else begin
        if v land 1 <> 0 then positive (v lsr 1) else zeros (v lsr 1);
        pnt := !pnt + skip
      end
    end
  done;
  table.(!idx) <- vc_len;
  lengths.(!pidx) <- table.(!idx) - table.(!pidx);
  for i = 0 to !idx - 1 do
    let v = to_int32 lengths.(i) in
    if v < 0 && table.(i) = 0 then begin
      let t = -v - 1 in
      if t < i then begin
        table.(i) <- table.(t);
        lengths.(i) <- lengths.(t)
      end
    end
  done;
  !idx

type state = {
  mutable previous_time : int64;  (** libfst starts from UINT64_MAX. *)
  mutable table : int array;
  mutable lengths : int array;
  scatter : int array;
  headptr : int array;
  remaining : int array;
  mutable trav : bytes;
  mutable scratch : bytes;
}

(* The traversal buffer grows on demand up to the block's declared need, so a corrupt
   declaration costs nothing until data actually arrives. *)
let reserve st n =
  if n > Bytes.length st.trav then begin
    let size = ref (max 65536 (Bytes.length st.trav)) in
    while !size < n do
      size := if !size > Sys.max_string_length / 2 then Sys.max_string_length else 2 * !size
    done;
    let b = Bytes.create !size in
    Bytes.blit st.trav 0 b 0 (Bytes.length st.trav);
    st.trav <- b
  end

let scratch st n =
  if n > Bytes.length st.scratch then
    st.scratch <- Bytes.create (max n (2 * Bytes.length st.scratch));
  st.scratch

let block ~(file : File.t) ~signals ~sink st ~secnum (b : File.block) =
  let little_endian = file.header.little_endian in
  let maxhandle = Array.length signals.lens in
  let data = File.block_data file.src b "value change block" in
  let base = Int64.add b.pos 1L in
  let len = String.length data in
  let at p = Int64.add base (Int64.of_int p) in
  let what = "value change block" in
  let r = Bin_reader.create ~base ~pos:8 data in
  let beg_tim = Bin_reader.u64 r what in
  let _end_tim = Bin_reader.u64 r what in
  let mem_required = Bin_reader.u64 r what in
  (* Time table, at the end of the block. *)
  if len < 24 + 32 then Error.fail Truncated b.pos "%s" what;
  let tr = Bin_reader.create ~base ~pos:(len - 24) data in
  let tsec_uclen = Bin_reader.to_int tr what (Bin_reader.u64 tr what) in
  let tsec_clen64 = Bin_reader.u64 tr what in
  let tsec_nitems = Bin_reader.u64 tr what in
  if Int64.compare tsec_clen64 b.seclen > 0 || Int64.compare tsec_clen64 0L < 0 then raise Exit;
  let tsec_clen = Int64.to_int tsec_clen64 in
  let tpos = len - 24 - tsec_clen in
  if tpos < 32 then Error.fail Truncated b.pos "%s: time table" what;
  let tdata =
    if tsec_uclen <> tsec_clen then
      Codec.zlib ~what:"time table" ~offset:(at tpos) data ~off:tpos ~len:tsec_clen ~size:tsec_uclen
    else String.sub data tpos tsec_uclen
  in
  if Int64.compare tsec_nitems 0L < 0 || Int64.compare tsec_nitems (Int64.of_int tsec_uclen) > 0
  then Error.fail Malformed (at tpos) "time table: %Lu items" tsec_nitems;
  let nitems = Int64.to_int tsec_nitems in
  let tt = Bin_reader.create ~base:(at tpos) tdata in
  let tv = ref 0L in
  let time_table =
    Array.init nitems (fun _ ->
        tv := Int64.add !tv (Bin_reader.varint tt "time table");
        !tv)
  in
  let tc_items = max nitems 1 in
  let tc_head = Array.make tc_items 0 in
  (* Frame: the values at the start of the block, emitted for the first block only. *)
  r.pos <- 32;
  let frame_uclen = Bin_reader.varint_int r what in
  let frame_clen = Bin_reader.varint_int r what in
  let frame_maxhandle = Bin_reader.varint_int r what in
  let fpos = r.pos in
  if frame_clen > len - fpos then Error.fail Truncated (at fpos) "frame";
  if secnum = 0 && (nitems = 0 || not (Int64.equal beg_tim time_table.(0))) then begin
    sink.frame beg_tim;
    let mu =
      if frame_uclen = frame_clen then String.sub data fpos frame_uclen
      else
        Codec.zlib ~what:"frame" ~offset:(at fpos) data ~off:fpos ~len:frame_clen ~size:frame_uclen
    in
    let mub = Bytes.unsafe_of_string mu in
    if frame_maxhandle > maxhandle then
      Error.fail Malformed (at fpos) "frame: %d handles, hierarchy has %d" frame_maxhandle maxhandle;
    let offs = ref 0 in
    for idx = 0 to frame_maxhandle - 1 do
      let l = signals.lens.(idx) in
      if l <= 1 then begin
        if l = 1 then begin
          if !offs >= frame_uclen then Error.fail Malformed (at fpos) "frame: too short";
          sink.bit (idx + 1) mu.[!offs]
        end
      end
      else begin
        if !offs + l > frame_uclen then Error.fail Malformed (at fpos) "frame: too short";
        if not signals.reals.(idx) then sink.vector (idx + 1) mub !offs l
        else sink.real (idx + 1) (Header.float_at ~little_endian mu !offs)
      end;
      offs := !offs + l
    done
  end;
  r.pos <- fpos + frame_clen;
  let vc_maxhandle = Bin_reader.varint_int r what in
  let vc_start = r.pos in
  let packtype = Char.chr (Bin_reader.u8 r what) in
  let indx_pntr = len - 24 - tsec_clen - 8 in
  if indx_pntr < vc_start then Error.fail Malformed b.pos "%s: chain table" what;
  let chain_clen64 = Bin_reader.u64_at data indx_pntr in
  if
    Int64.compare chain_clen64 0L < 0
    || Int64.compare chain_clen64 (Int64.of_int (indx_pntr - vc_start)) > 0
  then raise Skip_block;
  let chain_clen = Int64.to_int chain_clen64 in
  let indx_pos = indx_pntr - chain_clen in
  if vc_maxhandle > maxhandle then
    Error.fail Malformed b.pos "%s: %d handles, hierarchy has %d" what vc_maxhandle maxhandle;
  if vc_maxhandle + 1 > Array.length st.table then begin
    st.table <- Array.make (vc_maxhandle + 1) 0;
    st.lengths <- Array.make (vc_maxhandle + 1) 0
  end;
  let cm = String.sub data indx_pos chain_clen ^ "\000" in
  let idx =
    parse_chain cm
      ~alias2:(b.typ = File.bl_vcdata_dyn_alias2)
      ~vc_maxhandle ~vc_len:(indx_pos - vc_start) st.table st.lengths
  in
  let idx = min idx maxhandle in
  let mem_limit =
    if Int64.compare mem_required (Int64.of_int (Sys.max_string_length - 66)) > 0 then
      Sys.max_string_length
    else Int64.to_int mem_required + 66
  in
  let toffs = ref 0 in
  for i = 0 to idx - 1 do
    let ct = st.table.(i) in
    if ct <> 0 then begin
      let p = vc_start + ct in
      if p < 0 || p >= len then Error.fail Malformed b.pos "%s: chain offset" what;
      let cl = st.lengths.(i) in
      let vr = Bin_reader.create ~base ~pos:p data in
      let v = Bin_reader.varint32_int vr what in
      let skip = vr.pos - p in
      let dpos = vr.pos in
      let dest, clen = if v <> 0 then (v, cl - skip) else (cl - skip, cl - skip) in
      if v = 0 && cl <= skip then raise Skip_block;
      if !toffs + dest > mem_limit then raise Skip_block;
      if clen < 0 || dpos + clen > len then
        Error.fail Truncated (at dpos) "%s: handle %d" what (i + 1);
      reserve st (!toffs + dest);
      let w = "value changes" and offset = at dpos in
      if v = 0 then Bytes.blit_string data dpos st.trav !toffs dest
      else begin
        match packtype with
        | '4' ->
            Codec.lz4_into ~what:w ~offset data ~off:dpos ~len:clen st.trav ~dst_off:!toffs
              ~size:dest
        | 'F' ->
            Codec.fastlz_into ~what:w ~offset data ~off:dpos ~len:clen st.trav ~dst_off:!toffs
              ~size:dest
        | _ ->
            let d = Codec.zlib ~what:w ~offset data ~off:dpos ~len:clen ~size:dest in
            Bytes.blit_string d 0 st.trav !toffs dest
      end;
      st.headptr.(i) <- !toffs;
      st.remaining.(i) <- dest;
      toffs := !toffs + dest;
      let vli, _ = varint_at st.trav st.headptr.(i) (Bytes.length st.trav) w offset in
      let tdelta = if signals.lens.(i) = 1 then vli lsr 2 lsl (vli land 1) else vli lsr 1 in
      if tdelta >= tc_items then Error.fail Malformed offset "%s: time index %d" w tdelta;
      st.scatter.(i) <- tc_head.(tdelta);
      tc_head.(tdelta) <- i + 1
    end
  done;
  let trav = st.trav in
  let tlim = !toffs in
  let off = at vc_start in
  let w = "value changes" in
  for i = 0 to nitems - 1 do
    let t = time_table.(i) in
    if not (Int64.equal st.previous_time t) then begin
      sink.time t;
      st.previous_time <- t
    end;
    while tc_head.(i) <> 0 do
      let idx = tc_head.(i) - 1 in
      let hp = st.headptr.(idx) in
      let vli, skip = varint_at trav hp tlim w off in
      let l = signals.lens.(idx) in
      let consumed, shift1 =
        if l = 1 then begin
          let c =
            if vli land 1 = 0 then Char.unsafe_chr ((vli lsr 1) land 1 lor 0x30)
            else rcv.[(vli lsr 1) land 7]
          in
          sink.bit (idx + 1) c;
          (skip, true)
        end
        else if l = 0 then begin
          let vlen, skip2 = varint_at trav (hp + skip) tlim w off in
          let d = hp + skip + skip2 in
          if d + vlen > tlim then Error.fail Truncated off "%s: handle %d" w (idx + 1);
          if vli land 1 = 0 then sink.varlen (idx + 1) trav d vlen;
          (skip + skip2 + vlen, false)
        end
        else begin
          let d = hp + skip in
          if not signals.reals.(idx) then
            if vli land 1 = 0 then begin
              let n = ((l - 1) / 8) + 1 in
              if d + n > tlim then Error.fail Truncated off "%s: handle %d" w (idx + 1);
              let sb = scratch st l in
              for j = 0 to l - 1 do
                let byte = Char.code (Bytes.unsafe_get trav (d + (j lsr 3))) in
                Bytes.unsafe_set sb j
                  (Char.unsafe_chr ((byte lsr (7 - (j land 7))) land 1 lor 0x30))
              done;
              sink.vector (idx + 1) sb 0 l;
              (skip + n, false)
            end
            else begin
              if d + l > tlim then Error.fail Truncated off "%s: handle %d" w (idx + 1);
              sink.vector (idx + 1) trav d l;
              (skip + l, false)
            end
          else if vli land 1 = 0 then begin
            if d + 1 > tlim then Error.fail Truncated off "%s: handle %d" w (idx + 1);
            let byte = Char.code (Bytes.get trav d) in
            let buf =
              String.init 8 (fun j -> Char.unsafe_chr ((byte lsr (7 - j)) land 1 lor 0x30))
            in
            sink.real (idx + 1) (Header.float_at ~little_endian buf 0);
            (skip + 1, false)
          end
          else begin
            if d + 8 > tlim then Error.fail Truncated off "%s: handle %d" w (idx + 1);
            sink.real (idx + 1) (Header.float_at_bytes ~little_endian trav d);
            (skip + l, false)
          end
        end
      in
      st.headptr.(idx) <- hp + consumed;
      st.remaining.(idx) <- st.remaining.(idx) - consumed;
      tc_head.(i) <- st.scatter.(idx);
      st.scatter.(idx) <- 0;
      if st.remaining.(idx) > 0 then begin
        let vli, _ = varint_at trav st.headptr.(idx) tlim w off in
        let tdelta = if shift1 then vli lsr 2 lsl (vli land 1) else vli lsr 1 in
        if tdelta + i >= tc_items then Error.fail Malformed off "%s: time index %d" w (tdelta + i);
        st.scatter.(idx) <- tc_head.(i + tdelta);
        tc_head.(i + tdelta) <- idx + 1
      end
    done
  done

let iter ~(file : File.t) ~signals ~sink =
  let maxhandle = Array.length signals.lens in
  let st =
    {
      previous_time = -1L;
      table = [||];
      lengths = [||];
      scatter = Array.make maxhandle 0;
      headptr = Array.make maxhandle 0;
      remaining = Array.make maxhandle 0;
      trav = Bytes.empty;
      scratch = Bytes.create 64;
    }
  in
  let rec go secnum = function
    | [] -> ()
    | b :: rest when secnum < file.vc_section_count ->
        let continue_ =
          match block ~file ~signals ~sink st ~secnum b with
          | () -> true
          | exception Skip_block -> true
          | exception Exit -> false
        in
        if continue_ then go (secnum + 1) rest
    | _ -> ()
  in
  go 0 file.vc_blocks
