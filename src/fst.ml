[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic, OpenAI"]

module Err = Error
module H = Hier
module V = Vcd

module Error = struct
  type kind = Error.kind =
    | Truncated
    | Malformed
    | Unsupported
    | Decompression
    | Limit_exceeded
    | Io

  type t = Error.t

  let kind = Error.kind
  let offset = Error.offset
  let message = Error.message
  let pp = Error.pp
  let to_string = Error.to_string
end

module Header = Header

module Hier = struct
  type entry = Hier.entry =
    | Scope of { scope_type : int; name : string; component : string }
    | Upscope
    | Attr_begin of { attr_type : int; subtype : int; name : string; arg : int64 }
    | Attr_end
    | Var of {
        var_type : int;
        direction : int;
        name : string;
        length : int;
        handle : int;
        alias : bool;
      }

  let var_type_name t =
    if t >= 0 && t < Array.length Hier.var_type_names then Some Hier.var_type_names.(t) else None

  let scope_type_name t =
    if t >= 0 && t < Array.length Hier.scope_type_names then Some Hier.scope_type_names.(t)
    else None

  let is_real = Hier.is_real
end

type t = File.t

let io_error fmt =
  Printf.ksprintf (fun m -> Err.E { Err.kind = Err.Io; offset = -1L; message = m }) fmt

let open_file path =
  Err.catch (fun () ->
      let ic = try open_in_bin path with Sys_error m -> raise (io_error "%s" m) in
      try File.scan (File.of_channel ic)
      with e ->
        close_in_noerr ic;
        raise e)

let of_string s = Err.catch (fun () -> File.scan (File.of_string s))
let close (t : t) = t.src.close ()
let header (t : t) = t.header
let blackouts (t : t) = Array.to_list t.blackouts
let geometry t = Err.catch (fun () -> File.geometry t)

let fold_hierarchy (t : t) ~init ~f =
  Err.catch (fun () ->
      let base = match t.hier with Some b -> b.pos | None -> -1L in
      H.fold ~base (File.hierarchy_data t) ~init ~f)

type sink = Vc.sink = {
  time : int64 -> unit;
  frame : int64 -> unit;
  bit : int -> char -> unit;
  vector : int -> bytes -> int -> int -> unit;
  real : int -> float -> unit;
  varlen : int -> bytes -> int -> int -> unit;
}

let signals t =
  let lens = ref [] and reals = ref [] in
  let base = match t.File.hier with Some b -> b.pos | None -> -1L in
  H.fold ~base (File.hierarchy_data t) ~init:() ~f:(fun () -> function
    | H.Var { var_type; length; alias = false; _ } ->
        lens := length :: !lens;
        reals := H.is_real var_type :: !reals
    | _ -> ());
  { Vc.lens = Array.of_list (List.rev !lens); reals = Array.of_list (List.rev !reals) }

let iter_changes ?select t sink =
  Err.catch (fun () -> Vc.iter ?select ~file:t ~signals:(signals t) ~sink ())

type reader = Vc.reader

let reader ?select t sink =
  Err.catch (fun () -> Vc.start ?select ~file:t ~signals:(signals t) ~sink ())

let step r = Err.catch (fun () -> Vc.step r)

module Vcd = struct
  let write ?(extensions = false) t oc =
    Err.catch (fun () ->
        V.write ~extensions t (Buffer.create 131072) ~flush:(fun b ->
            Buffer.output_buffer oc b;
            Buffer.clear b))

  let to_string ?(extensions = false) t =
    Err.catch (fun () ->
        let b = Buffer.create 131072 in
        V.write ~extensions t b ~flush:ignore;
        Buffer.contents b)

  let id = V.id
  let timescale = V.timescale
  let var_length = V.var_length

  let escape s off len =
    let b = Buffer.create (len + 8) in
    V.add_escaped b s off len;
    Buffer.contents b
end

module Private = struct
  let fastlz s ~size =
    let dst = Bytes.create size in
    match Fastlz.decompress_into s ~off:0 ~len:(String.length s) dst ~dst_off:0 ~dst_len:size with
    | Some n -> Some (Bytes.sub_string dst 0 n)
    | None -> None

  let vcd_id = V.id
  let timescale = V.timescale
  let g16 = V.g16

  let escape s =
    let b = Buffer.create 16 in
    V.add_escaped b (Bytes.of_string s) 0 (String.length s);
    Buffer.contents b

  let varint s = Bin_reader.varint (Bin_reader.create s) "varint"
  let svarint s = fst (Vc.svarint_str (s ^ "\000") 0)

  let block_types (t : t) =
    (match t.hier with Some b -> [ b.typ ] | None -> [])
    @ List.map (fun (b : File.block) -> b.typ) t.vc_blocks

  let packtypes (t : t) = Err.catch (fun () -> List.map (File.packtype t) t.vc_blocks)

  let parse_chain s ~alias2 ~vc_maxhandle ~vc_len =
    let table = Array.make (vc_maxhandle + 1) 0 and lengths = Array.make (vc_maxhandle + 1) 0 in
    match Vc.parse_chain (s ^ "\000") ~alias2 ~vc_maxhandle ~vc_len table lengths with
    | n -> Some (Array.sub table 0 n, Array.sub lengths 0 n)
    | exception Vc.Skip_block -> None
end
