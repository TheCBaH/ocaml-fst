[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic"]

type kind = Bit | Vector of int | Real | String

type signal = {
  name : string;
  kind : kind;
  alias : int option;
  decl_len : int option;
  var_type : int option;
}

type value = B of char | V of string | R of float | S of string

type block = {
  start_time : int64;
  end_time : int64;
  times : int64 list;
  frame : (int * value) list;
  changes : (int * int * value) list;
  packtype : char;
  mem_required : int64 option;
  chain_dest : (int * int) list;
  chain_table : string option;
  trailer : (int * int * int) option;
}

type t = { timescale : int; scope : string; signals : signal list; blocks : block list }

let signal ?alias name kind = { name; kind; alias; decl_len = None; var_type = None }

let block ?start ?(frame = []) ~times changes =
  let first = match times with t :: _ -> t | [] -> 0L in
  let last = List.fold_left (fun _ t -> t) first times in
  {
    start_time = Option.value start ~default:first;
    end_time = last;
    times;
    frame;
    changes;
    packtype = 'Z';
    mem_required = None;
    chain_dest = [];
    chain_table = None;
    trailer = None;
  }

let make ?(timescale = -9) signals blocks = { timescale; scope = "top"; signals; blocks }

let update_nth l n f =
  if n < 0 || n >= List.length l then invalid_arg "Fst_fixture: index";
  List.mapi (fun i x -> if i = n then f x else x) l

let set_length t ~signal n =
  { t with signals = update_nth t.signals (signal - 1) (fun s -> { s with decl_len = Some n }) }

let set_var_type t ~signal ty =
  { t with signals = update_nth t.signals (signal - 1) (fun s -> { s with var_type = Some ty }) }

let set_alias_target t ~signal target =
  { t with signals = update_nth t.signals (signal - 1) (fun s -> { s with alias = Some target }) }

let set_block t block f = { t with blocks = update_nth t.blocks block f }

let set_chain_dest t ~block ~handle dest =
  set_block t block (fun b ->
      {
        b with
        chain_dest = (handle, dest) :: b.chain_dest;
        mem_required = Some (Int64.of_int dest);
      })

let set_mem_required t ~block m = set_block t block (fun b -> { b with mem_required = Some m })
let set_chain_table t ~block s = set_block t block (fun b -> { b with chain_table = Some s })

let set_trailer t ~block ~uclen ~clen ~nitems =
  set_block t block (fun b -> { b with trailer = Some (uclen, clen, nitems) })

let varint n =
  let b = Buffer.create 4 in
  let rec go n =
    if n >= 0x80 then begin
      Buffer.add_char b (Char.chr (n land 0x7f lor 0x80));
      go (n lsr 7)
    end
    else Buffer.add_char b (Char.chr n)
  in
  go n;
  Buffer.contents b

let u64 n =
  String.init 8 (fun i ->
      Char.chr (Int64.to_int (Int64.shift_right_logical n (56 - (8 * i))) land 0xff))

let u64i n = u64 (Int64.of_int n)

let le64 n =
  String.init 8 (fun i -> Char.chr (Int64.to_int (Int64.shift_right_logical n (8 * i)) land 0xff))

let real_bytes d = le64 (Int64.bits_of_float d)

let pad s n c =
  if String.length s >= n then String.sub s 0 n else s ^ String.make (n - String.length s) c

let rcv = "xzhuwl-?"
let natural_len = function Bit -> 1 | Vector n -> n | Real -> 8 | String -> 0
let length s = Option.value s.decl_len ~default:(natural_len s.kind)

let var_type s =
  match (s.var_type, s.kind) with
  | Some ty, _ -> ty
  | None, (Bit | Vector _) -> 16
  | None, Real -> 3
  | None, String -> 21

(* Handles of the variables that are not aliases, in order. *)
let handle_signals t = List.filter (fun s -> s.alias = None) t.signals

let hierarchy t =
  let b = Buffer.create 64 in
  Printf.bprintf b "\254%c%s\000\000" '\000' t.scope;
  List.iter
    (fun s ->
      Printf.bprintf b "%c%c%s\000%s%s"
        (Char.chr (var_type s))
        '\000' s.name
        (varint (length s))
        (varint (Option.value s.alias ~default:0)))
    t.signals;
  Buffer.add_char b '\255';
  Buffer.contents b

let section typ body =
  let n = String.length body + 8 in
  Printf.sprintf "%c%s%s" (Char.chr typ) (u64i n) body

let pad_field s n = pad s n '\000'

let header t =
  let hs = handle_signals t in
  let first = match t.blocks with b :: _ -> b.start_time | [] -> 0L in
  let last = List.fold_left (fun _ b -> b.end_time) 0L t.blocks in
  section 0
    (String.concat ""
       [
         u64 first;
         u64 last;
         le64 (Int64.bits_of_float 2.7182818284590452354);
         u64i 0;
         u64i 1;
         u64i (List.length t.signals);
         u64i (List.length hs);
         u64i (List.length t.blocks);
         String.make 1 (Char.chr (t.timescale land 0xff));
         pad_field "ocaml-fst fixture" 128;
         pad_field "Thu Jan  1 00:00:00 1970\n" 119;
         "\000";
         u64i 0;
       ])

let hier_section t =
  let h = hierarchy t in
  section 4 (u64i (String.length h) ^ h)

let check_len what n l =
  if n <> l then invalid_arg (Printf.sprintf "Fst_fixture: %s needs %d" what l)

(* A value's bytes in the frame, which has one fixed width per variable. *)
let frame_value s v =
  let l = length s in
  match (s.kind, v) with
  | Bit, B c -> pad (String.make 1 c) l 'x'
  | Vector n, V x ->
      check_len "vector value" (String.length x) n;
      pad x l 'x'
  | Real, R d -> pad (real_bytes d) l '\000'
  | String, S _ -> ""
  | _ -> invalid_arg "Fst_fixture: value does not match the kind"

let default_value s =
  match s.kind with
  | Bit -> B 'x'
  | Vector n -> V (String.make n 'x')
  | Real -> R 0.
  | String -> S ""

(* One change in a chain: the time index delta, then the value. *)
let change s delta v =
  match (s.kind, v) with
  | Bit, B c -> (
      match String.index_opt rcv c with
      | None when c = '0' || c = '1' -> varint ((delta lsl 2) lor ((Char.code c - 48) lsl 1))
      | Some code -> varint ((delta lsl 4) lor (code lsl 1) lor 1)
      | None -> invalid_arg "Fst_fixture: bit value")
  | Vector n, V x ->
      check_len "vector value" (String.length x) n;
      if String.for_all (fun c -> c = '0' || c = '1') x then begin
        let bytes = Bytes.make (((n - 1) / 8) + 1) '\000' in
        String.iteri
          (fun j c ->
            if c = '1' then
              Bytes.set bytes (j / 8)
                (Char.chr (Char.code (Bytes.get bytes (j / 8)) lor (0x80 lsr (j land 7)))))
          x;
        varint (delta lsl 1) ^ Bytes.to_string bytes
      end
      else varint ((delta lsl 1) lor 1) ^ x
  | Real, R d -> varint ((delta lsl 1) lor 1) ^ real_bytes d
  | String, S x -> varint (delta lsl 1) ^ varint (String.length x) ^ x
  | _ -> invalid_arg "Fst_fixture: value does not match the kind"

let chain s changes =
  let prev = ref 0 in
  String.concat ""
    (List.map
       (fun (i, v) ->
         let d = i - !prev in
         prev := i;
         change s d v)
       changes)

let vc_block t (b : block) =
  let hs = Array.of_list (handle_signals t) in
  let nh = Array.length hs in
  let frame =
    String.concat ""
      (List.init nh (fun i ->
           let v = Option.value (List.assoc_opt (i + 1) b.frame) ~default:(default_value hs.(i)) in
           frame_value hs.(i) v))
  in
  let chains =
    Array.init nh (fun i ->
        match
          List.filter_map
            (fun (ti, h, v) -> if h = i + 1 then Some (ti, v) else None)
            (List.stable_sort (fun (a, _, _) (b, _, _) -> compare a b) b.changes)
        with
        | [] -> None
        | cs ->
            let payload = chain hs.(i) cs in
            let dest = Option.value (List.assoc_opt (i + 1) b.chain_dest) ~default:0 in
            Some (varint dest ^ payload, String.length payload))
  in
  let vc_maxhandle =
    Array.fold_left (fun (i, m) c -> (i + 1, if c <> None then i + 1 else m)) (0, 0) chains |> snd
  in
  let body = Buffer.create 256 in
  let off = ref 1 and prev = ref 0 and zeros = ref 0 and table = Buffer.create 16 in
  let flush () =
    if !zeros > 0 then Buffer.add_string table (varint (!zeros lsl 1));
    zeros := 0
  in
  for i = 0 to vc_maxhandle - 1 do
    match chains.(i) with
    | None -> incr zeros
    | Some (c, _) ->
        flush ();
        Buffer.add_string table (varint (((!off - !prev) lsl 1) lor 1));
        prev := !off;
        off := !off + String.length c;
        Buffer.add_string body c
  done;
  let table = Option.value b.chain_table ~default:(Buffer.contents table) in
  let mem =
    match b.mem_required with
    | Some m -> m
    | None ->
        Int64.of_int
          (Array.fold_left (fun a -> function Some (_, n) -> a + n | None -> a) 0 chains)
  in
  let tt =
    let prev = ref 0L in
    String.concat ""
      (List.map
         (fun x ->
           let d = Int64.sub x !prev in
           prev := x;
           varint (Int64.to_int d))
         b.times)
  in
  let uclen, clen, nitems =
    Option.value b.trailer ~default:(String.length tt, String.length tt, List.length b.times)
  in
  section 1
    (String.concat ""
       [
         u64 b.start_time;
         u64 b.end_time;
         u64 mem;
         varint (String.length frame);
         varint (String.length frame);
         varint nh;
         frame;
         varint vc_maxhandle;
         String.make 1 b.packtype;
         Buffer.contents body;
         table;
         u64i (String.length table);
         tt;
         u64i uclen;
         u64i clen;
         u64i nitems;
       ])

let build t = String.concat "" ((header t :: List.map (vc_block t) t.blocks) @ [ hier_section t ])
