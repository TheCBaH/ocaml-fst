[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic, OpenAI"]

type kind = Truncated | Malformed | Unsupported | Decompression | Limit_exceeded | Io
type t = { kind : kind; offset : int64; hier : bool; message : string }

exception E of t

let fail ?(hier = false) kind offset fmt =
  Printf.ksprintf (fun message -> raise (E { kind; offset; hier; message })) fmt

let make kind message = { kind; offset = -1L; hier = false; message }
let kind t = t.kind

let offset t =
  if Int64.compare t.offset 0L < 0 then None
  else if t.hier then Some (`Hierarchy t.offset)
  else Some (`File t.offset)

let message t = t.message

let kind_to_string = function
  | Truncated -> "truncated input"
  | Malformed -> "malformed input"
  | Unsupported -> "unsupported feature"
  | Decompression -> "decompression failed"
  | Limit_exceeded -> "limit exceeded"
  | Io -> "I/O error"

let pp ppf t =
  match offset t with
  | Some (`File o) ->
      Format.fprintf ppf "fst: %s: %s (offset %Ld)" (kind_to_string t.kind) t.message o
  | Some (`Hierarchy o) ->
      Format.fprintf ppf "fst: %s: %s (hierarchy offset %Ld)" (kind_to_string t.kind) t.message o
  | None -> Format.fprintf ppf "fst: %s: %s" (kind_to_string t.kind) t.message

let to_string t = Format.asprintf "%a" pp t
let catch f = try Ok (f ()) with E e -> Error e | Sys_error message -> Error (make Io message)
