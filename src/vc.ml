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

let ignore_sink =
  {
    time = ignore;
    frame = ignore;
    bit = (fun _ _ -> ());
    vector = (fun _ _ _ _ -> ());
    real = (fun _ _ -> ());
    varlen = (fun _ _ _ _ -> ());
  }

(* What the traversal needs from the hierarchy: per handle (0-based), the declared length and
   whether it is a real. *)
type signals = { lens : int array; reals : bool array }

let tables (a : (Hier.kind * int) array) =
  { lens = Array.map snd a; reals = Array.map (fun (k, _) -> k = Hier.Real) a }

let rcv = "xzhuwl-?"

(* libfst's uint32_t arithmetic; on 32-bit hosts [int] has 31 bits and values that large are
   rejected instead. *)
let mask32 = if Sys.int_size > 32 then (1 lsl 32) - 1 else max_int

let to_int32 v =
  if Sys.int_size > 32 then (v land mask32) - if v land (1 lsl 31) <> 0 then 1 lsl 32 else 0 else v

(* The time delta of a bit change: libfst's [shcnt = 2 << (vli & 1)]. ocamlformat drops the
   parentheses of [vli lsr (2 lsl ...)], so the shift is named. *)
let bit_delta vli =
  let shcnt = 2 lsl (vli land 1) in
  vli lsr shcnt

(* A chain table that is inconsistent with its block; [block] turns it into [Malformed]. *)
exception Bad_chain

(* Entry [b] is the eight characters '0'/'1' of byte [b], most significant bit first. *)
let expand_tbl =
  String.init 2048 (fun i -> Char.chr (((i lsr 3) lsr (7 - (i land 7))) land 1 lor 0x30))

external get64u : string -> int -> int64 = "%caml_string_get64u"
external set64u : bytes -> int -> int64 -> unit = "%caml_bytes_set64u"

(* The [l] bits starting at byte [spos] of [src], as '0'/'1' characters at [dpos] of [dst]. *)
let expand_into dst dpos src spos l =
  let full = l lsr 3 in
  for j = 0 to full - 1 do
    let byte = Char.code (Bytes.unsafe_get src (spos + j)) in
    set64u dst (dpos + (j * 8)) (get64u expand_tbl (byte * 8))
  done;
  for j = full * 8 to l - 1 do
    let byte = Char.code (Bytes.unsafe_get src (spos + (j lsr 3))) in
    Bytes.unsafe_set dst (dpos + j) (Char.unsafe_chr ((byte lsr (7 - (j land 7))) land 1 lor 0x30))
  done

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
    if not (room 1) then raise Bad_chain;
    pval := !pval + v;
    table.(!idx) <- !pval;
    if !idx > 0 then lengths.(!pidx) <- !pval - table.(!pidx);
    pidx := !idx;
    incr idx
  in
  let zeros n =
    if not (room n) then raise Bad_chain;
    for _ = 1 to n do
      table.(!idx) <- 0;
      incr idx
    done
  in
  let alias l =
    if not (room 1) then raise Bad_chain;
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
        if not (room 1) then raise Bad_chain;
        pnt := !pnt + skip;
        if !pnt >= clen then raise Bad_chain;
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
  max_bits : int;
      (** With [packed]: the widest vector accepted, as {!scratch} limits the others. *)
  mutable skip : int;  (** The length of the varint [varint_at] read last. *)
}

(* Neither a closure nor a pair per call: the length goes to [st.skip]. *)
let rec varint_go st b pos p lim acc shift what off =
  if p >= lim then Error.fail Truncated off "%s" what;
  let c = Char.code (Bytes.unsafe_get b p) in
  let acc = if shift < 32 then acc lor ((c land 0x7f) lsl shift) else acc in
  if c land 0x80 <> 0 then varint_go st b pos (p + 1) lim acc (shift + 7) what off
  else if acc < 0 then Error.fail Limit_exceeded off "%s: varint" what
  else begin
    st.skip <- p + 1 - pos;
    acc land mask32
  end

let varint_at st b pos lim what off = varint_go st b pos pos lim 0 0 what off

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

let scratch st ~off n =
  if n > Bytes.length st.scratch then begin
    if n > Sys.max_string_length then
      Error.fail Limit_exceeded off "value changes: %d bits do not fit a string" n;
    st.scratch <- Bytes.create (min Sys.max_string_length (max n (2 * Bytes.length st.scratch)))
  end;
  st.scratch

let block ~(file : File.t) ~signals ~mask ~packed ~recovered ~sink st ~index ~first (b : File.block)
    =
  let selected i = match mask with None -> true | Some m -> Array.unsafe_get m i in
  let little_endian = file.header.little_endian in
  let maxhandle = Array.length signals.lens in
  let data = File.block_data file.src b "value change block" in
  let base = Int64.add b.pos 1L in
  let len = String.length data in
  let at p = Int64.add base (Int64.of_int p) in
  let what = "value change block" in
  let bad fmt = Error.fail Malformed b.pos ("%s %d: " ^^ fmt) what index in
  let r = Bin_reader.create ~base ~pos:8 data in
  let beg_tim = Bin_reader.u64 r what in
  let _end_tim = Bin_reader.u64 r what in
  let mem_required = Bin_reader.u64 r what in
  let { File.tpos = _; clen = tsec_clen; times = time_table } =
    File.time_table b ~read:(fun off n -> String.sub data off n)
  in
  let nitems = Array.length time_table in
  let tc_items = max nitems 1 in
  let tc_head = Array.make tc_items 0 in
  (* Frame: the values at the start of the block, emitted for the first block only. *)
  r.pos <- 32;
  let frame_uclen = Bin_reader.varint_int r what in
  let frame_clen = Bin_reader.varint_int r what in
  let frame_maxhandle = Bin_reader.varint_int r what in
  let fpos = r.pos in
  if frame_clen > len - fpos then Error.fail Truncated (at fpos) "frame";
  if recovered <> None || (first && (nitems = 0 || not (Int64.equal beg_tim time_table.(0)))) then begin
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
          if selected idx then sink.bit (idx + 1) mu.[!offs]
        end
      end
      else begin
        if l > frame_uclen - !offs then Error.fail Malformed (at fpos) "frame: too short";
        if selected idx then
          if not signals.reals.(idx) then sink.vector (idx + 1) mub !offs l
          else sink.real (idx + 1) (Header.float_at ~little_endian mu !offs)
      end;
      offs := !offs + l
    done;
    List.iter
      (fun (h, v) -> sink.varlen h (Bytes.unsafe_of_string v) 0 (String.length v))
      (Option.value recovered ~default:[])
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
  then bad "chain table size";
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
    try
      parse_chain cm
        ~alias2:(b.typ = File.bl_vcdata_dyn_alias2)
        ~vc_maxhandle ~vc_len:(indx_pos - vc_start) st.table st.lengths
    with Bad_chain -> bad "chain table"
  in
  let idx = min idx maxhandle in
  let mem_limit =
    if Int64.compare mem_required (Int64.of_int (Sys.max_string_length - 66)) > 0 then
      Sys.max_string_length
    else Int64.to_int mem_required + 66
  in
  (* [need] is what the traversal buffer would hold without selection, so that the checks
     against [mem_limit], and thus which blocks are skipped, do not depend on it. *)
  let toffs = ref 0 and need = ref 0 in
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
      if v = 0 && cl <= skip then bad "handle %d: chain shorter than its length" (i + 1);
      if dest > mem_limit - !need then bad "handle %d: chains exceed the declared memory" (i + 1);
      need := !need + dest;
      if clen < 0 || clen > len - dpos then
        Error.fail Truncated (at dpos) "%s: handle %d" what (i + 1);
      if selected i then begin
        let w = "value changes" and offset = at dpos in
        if v <> 0 then begin
          let ratio =
            if packtype = '4' || packtype = 'F' then Codec.lz_ratio else Codec.zlib_ratio
          in
          Codec.ratio_check ~what:w ~offset ~ratio ~src_len:clen dest
        end;
        reserve st (!toffs + dest);
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
        let vli = varint_at st st.trav st.headptr.(i) (Bytes.length st.trav) w offset in
        let tdelta = if signals.lens.(i) = 1 then bit_delta vli else vli lsr 1 in
        if tdelta >= tc_items then Error.fail Malformed offset "%s: time index %d" w tdelta;
        st.scatter.(i) <- tc_head.(tdelta);
        tc_head.(tdelta) <- i + 1
      end
    end
  done;
  let trav = st.trav in
  let tlim = !toffs in
  (* Boxed once: an unboxed Int64 would be allocated again at every call that takes it. *)
  let off = Sys.opaque_identity (at vc_start) in
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
      let vli = varint_at st trav hp tlim w off in
      let skip = st.skip in
      let l = signals.lens.(idx) in
      let consumed =
        if l = 1 then begin
          let c =
            if vli land 1 = 0 then Char.unsafe_chr ((vli lsr 1) land 1 lor 0x30)
            else rcv.[(vli lsr 1) land 7]
          in
          sink.bit (idx + 1) c;
          skip
        end
        else if l = 0 then begin
          let vlen = varint_at st trav (hp + skip) tlim w off in
          let skip2 = st.skip in
          let d = hp + skip + skip2 in
          if vlen > tlim - d then Error.fail Truncated off "%s: handle %d" w (idx + 1);
          if vli land 1 = 0 then sink.varlen (idx + 1) trav d vlen;
          skip + skip2 + vlen
        end
        else begin
          let d = hp + skip in
          if not signals.reals.(idx) then
            if vli land 1 = 0 then begin
              let n = ((l - 1) / 8) + 1 in
              if n > tlim - d then Error.fail Truncated off "%s: handle %d" w (idx + 1);
              (match packed with
              | Some f ->
                  if l > st.max_bits then
                    Error.fail Limit_exceeded off "value changes: %d bits do not fit a string" l;
                  f (idx + 1) trav d l
              | None ->
                  let sb = scratch st ~off l in
                  expand_into sb 0 trav d l;
                  sink.vector (idx + 1) sb 0 l);
              skip + n
            end
            else begin
              if l > tlim - d then Error.fail Truncated off "%s: handle %d" w (idx + 1);
              sink.vector (idx + 1) trav d l;
              skip + l
            end
          else if vli land 1 = 0 then begin
            if d + 1 > tlim then Error.fail Truncated off "%s: handle %d" w (idx + 1);
            let byte = Char.code (Bytes.get trav d) in
            let buf =
              String.init 8 (fun j -> Char.unsafe_chr ((byte lsr (7 - j)) land 1 lor 0x30))
            in
            sink.real (idx + 1) (Header.float_at ~little_endian buf 0);
            skip + 1
          end
          else begin
            if d + 8 > tlim then Error.fail Truncated off "%s: handle %d" w (idx + 1);
            sink.real (idx + 1) (Header.float_at_bytes ~little_endian trav d);
            skip + l
          end
        end
      in
      st.headptr.(idx) <- hp + consumed;
      st.remaining.(idx) <- st.remaining.(idx) - consumed;
      tc_head.(i) <- st.scatter.(idx);
      st.scatter.(idx) <- 0;
      if st.remaining.(idx) > 0 then begin
        let vli = varint_at st trav st.headptr.(idx) tlim w off in
        let tdelta = if l = 1 then bit_delta vli else vli lsr 1 in
        if tdelta + i >= tc_items then Error.fail Malformed off "%s: time index %d" w (tdelta + i);
        st.scatter.(idx) <- tc_head.(i + tdelta);
        tc_head.(i + tdelta) <- idx + 1
      end
    done
  done

type reader = {
  file : File.t;
  signals : signals;
  sink : sink;
  packed : (int -> bytes -> int -> int -> unit) option;
  st : state;
  mutable select : (int -> bool) option;  (** Until the first step builds [mask] from it. *)
  mutable mask : bool array option;
  blocks : File.block array;
  from : int;
  first : int;  (** The only block whose frame is emitted without a seek. *)
  stop : int;
  mutable next : int;
  mutable prepass : int;  (** Earlier blocks whose strings the first step recovers. *)
  mutable recovered : (int * string) list option;
      (** Set by {!start_at}: the first block's frame is then always emitted, followed by these
          strings. *)
}

(* Reads the blocks from index [from] up to, not including, [stop]. *)
let start ?select ?packed ?(max_bits = max_int) ?(from = 0) ?first ?stop ~(file : File.t) ~signals
    ~sink () =
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
      max_bits;
      skip = 0;
    }
  in
  {
    file;
    signals;
    sink;
    packed;
    st;
    select;
    mask = None;
    blocks = file.vc_blocks;
    from;
    first = Option.value first ~default:from;
    stop = Option.value stop ~default:(Array.length file.vc_blocks);
    next = from;
    prepass = 0;
    recovered = None;
  }

(* No block is pending while one is decoded, so that an exception, ours or the sink's, ends the
   iteration. *)
let rec step r =
  File.check_open r.file;
  (match r.select with
  | Some f ->
      r.mask <- Some (Array.init (Array.length r.signals.lens) (fun i -> f (i + 1)));
      r.select <- None
  | None -> ());
  if r.prepass > 0 then begin
    let k = r.prepass in
    r.prepass <- 0;
    r.recovered <- Some (strings_before r k)
  end;
  if r.next >= r.stop then false
  else begin
    let i = r.next in
    r.next <- r.stop;
    block ~file:r.file ~signals:r.signals ~mask:r.mask ~packed:r.packed ~recovered:r.recovered
      ~sink:r.sink r.st ~index:i ~first:(i = r.first) r.blocks.(i);
    r.recovered <- None;
    r.next <- i + 1;
    true
  end

(* The last value of each selected string variable in the first [k] blocks: a block's frame does
   not carry them. Only those handles' chains are read. The state after each block is kept in
   [file.seeks], so that a seek reads only the blocks since the nearest boundary an earlier seek
   reached; a block that fails is not kept, and fails again. *)
and strings_before r k =
  let lens = r.signals.lens in
  let want i = lens.(i) = 0 && match r.mask with None -> true | Some m -> m.(i) in
  let wanted = List.filter want (List.init (Array.length lens) Fun.id) in
  if wanted = [] then []
  else begin
    let cps =
      match Hashtbl.find_opt r.file.seeks wanted with
      | Some a -> a
      | None ->
          let a = Array.make (Array.length r.blocks + 1) None in
          a.(0) <- Some File.start_checkpoint;
          Hashtbl.replace r.file.seeks wanted a;
          a
    in
    let rec nearest j = match cps.(j) with Some c -> (j, c) | None -> nearest (j - 1) in
    let j, cp = nearest k in
    if j < k then begin
      let last = ref cp.strings in
      let sink =
        {
          ignore_sink with
          varlen =
            (fun h b off len -> last := File.Int_map.add h (Bytes.sub_string b off len) !last);
        }
      in
      let p =
        start
          ~select:(fun h -> want (h - 1))
          ~from:j ~first:0 ~stop:k ~file:r.file ~signals:r.signals ~sink ()
      in
      if cp.lengths_size > 0 then begin
        p.st.table <- Array.make cp.lengths_size 0;
        p.st.lengths <- Array.make cp.lengths_size 0;
        List.iter (fun (i, v) -> p.st.lengths.(i) <- v) cp.negative
      end;
      let prev = ref cp in
      for b = j + 1 to k do
        ignore (step p);
        let negative = ref [] in
        Array.iteri (fun i v -> if to_int32 v < 0 then negative := (i, v) :: !negative) p.st.lengths;
        let negative = List.rev !negative in
        let cp =
          {
            File.strings = !last;
            lengths_size = Array.length p.st.lengths;
            negative = (if negative = !prev.negative then !prev.negative else negative);
          }
        in
        cps.(b) <- Some cp;
        prev := cp
      done
    end;
    match cps.(k) with Some cp -> File.Int_map.bindings cp.strings | None -> assert false
  end

(* Whether the first and the last time step of a block have changes, decoding every handle. *)
let activity ~file ~signals j =
  let times = ref 0 and first = ref false and last = ref false in
  let change () =
    if !times > 0 then begin
      if !times = 1 then first := true;
      last := true
    end
  in
  let sink =
    {
      time =
        (fun _ ->
          incr times;
          last := false);
      frame = ignore;
      bit = (fun _ _ -> change ());
      vector = (fun _ _ _ _ -> change ());
      real = (fun _ _ -> change ());
      varlen = (fun _ _ _ _ -> change ());
    }
  in
  let p = start ~from:j ~stop:(j + 1) ~file ~signals ~sink () in
  while step p do
    ()
  done;
  (!first, !last)

(* Starts at block [block] (0-based, within [file.vc_blocks]). *)
let start_at ?select ?packed ?max_bits ~file ~signals ~sink ~block () =
  let r = start ?select ?packed ?max_bits ~from:block ~file ~signals ~sink () in
  if block > 0 then begin
    r.prepass <- block;
    r.recovered <- Some []
  end;
  r

let iter ?select ?packed ?max_bits ~file ~signals ~sink () =
  let r = start ?select ?packed ?max_bits ~file ~signals ~sink () in
  while step r do
    ()
  done
