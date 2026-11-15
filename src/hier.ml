[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic, OpenAI"]

type entry =
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

let vt_real = 3
let vt_real_parameter = 4
let vt_port = 18
let vt_realtime = 20
let vt_string = 21
let vt_shortreal = 29
let is_real t = t = vt_real || t = vt_real_parameter || t = vt_realtime || t = vt_shortreal

type kind = Bit | Vector | Port | Real | String

(* What the traversal delivers for a variable, which its declared length decides. *)
let kind_of ~var_type ~length =
  if is_real var_type then Real
  else if length = 0 then String
  else if var_type = vt_port then Port
  else if length = 1 then Bit
  else Vector

(* The traversal reads 8 bytes per real value. *)
let check_real ~base ~var_type ~length =
  if is_real var_type && length <> 8 then
    Error.fail Malformed base "real variable of %d bytes" length

(* libfst's longest kept name (FST_ID_NAM_ATTR_SIZ). *)
let max_name = 65536 + 4096

let var_type_names =
  [| "event"; "integer"; "parameter"; "real"; "real_parameter"; "reg"; "supply0"; "supply1";
     "time"; "tri"; "triand"; "trior"; "trireg"; "tri0"; "tri1"; "wand"; "wire"; "wor"; "port";
     "sparray"; "realtime"; "string"; "bit"; "logic"; "int"; "shortint"; "longint"; "byte";
     "enum"; "shortreal" |]
[@@ocamlformat "disable"]

let scope_type_names =
  [| "module"; "task"; "function"; "begin"; "fork"; "generate"; "struct"; "union"; "class";
     "interface"; "package"; "program"; "vhdl_architecture"; "vhdl_procedure"; "vhdl_function";
     "vhdl_record"; "vhdl_process"; "vhdl_block"; "vhdl_for_generate"; "vhdl_if_generate";
     "vhdl_generate"; "vhdl_package"; "sv_array" |]
[@@ocamlformat "disable"]

let tag_attr_begin = 252
let tag_attr_end = 253
let tag_scope = 254
let tag_upscope = 255

(* The tag stream as libfst's fstReaderProcessHier reads it: unknown tags are skipped, empty
   scope names become [$unnamed_scope_N], aliases name a 1-based handle and other variables take
   the next one. *)
let fold ~base data ~init ~f =
  let r = Bin_reader.create ~base data in
  let unnamed = ref 0 and handle = ref 0 in
  let acc = ref init in
  let what = "hierarchy" in
  while Bin_reader.remaining r > 0 do
    let tag = Bin_reader.u8 r what in
    if tag = tag_scope then begin
      let scope_type = Bin_reader.u8 r what in
      let name = Bin_reader.cstring r ~max:max_name what in
      let name =
        if name = "" then begin
          let n = Printf.sprintf "$unnamed_scope_%d" !unnamed in
          incr unnamed;
          n
        end
        else name
      in
      let component = Bin_reader.cstring r ~max:max_name what in
      acc := f !acc (Scope { scope_type; name; component })
    end
    else if tag = tag_upscope then acc := f !acc Upscope
    else if tag = tag_attr_begin then begin
      let attr_type = Bin_reader.u8 r what in
      let subtype = Bin_reader.u8 r what in
      let name = Bin_reader.cstring r ~max:max_name what in
      let arg = Bin_reader.varint r what in
      acc := f !acc (Attr_begin { attr_type; subtype; name; arg })
    end
    else if tag = tag_attr_end then acc := f !acc Attr_end
    else if tag < Array.length var_type_names then begin
      let direction = Bin_reader.u8 r what in
      let name = Bin_reader.cstring r ~max:max_name what in
      let length = Bin_reader.varint32_int r what in
      let alias = Bin_reader.varint32_int r what in
      let handle, alias =
        if alias = 0 then begin
          incr handle;
          (!handle, false)
        end
        else (alias, true)
      in
      acc := f !acc (Var { var_type = tag; direction; name; length; handle; alias })
    end
  done;
  !acc

(* The kind and declared length of each variable that is not an alias, in handle order, without
   building any name. *)
let signals ~base data =
  let r = Bin_reader.create ~base data in
  let acc = ref [] in
  let what = "hierarchy" in
  while Bin_reader.remaining r > 0 do
    let tag = Bin_reader.u8 r what in
    if tag = tag_scope then begin
      Bin_reader.skip r 1 what;
      Bin_reader.skip_cstring r what;
      Bin_reader.skip_cstring r what
    end
    else if tag = tag_attr_begin then begin
      Bin_reader.skip r 2 what;
      Bin_reader.skip_cstring r what;
      ignore (Bin_reader.varint r what)
    end
    else if tag < Array.length var_type_names then begin
      Bin_reader.skip r 1 what;
      Bin_reader.skip_cstring r what;
      let length = Bin_reader.varint32_int r what in
      if Bin_reader.varint32_int r what = 0 then begin
        check_real ~base ~var_type:tag ~length;
        acc := (kind_of ~var_type:tag ~length, length) :: !acc
      end
    end
  done;
  Array.of_list (List.rev !acc)
