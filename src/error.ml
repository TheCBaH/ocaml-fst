[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic, OpenAI"]

type kind = Truncated | Malformed | Unsupported | Decompression | Limit_exceeded | Io
type t = { kind : kind; offset : int64; message : string }

exception E of t

let fail kind offset fmt = Printf.ksprintf (fun message -> raise (E { kind; offset; message })) fmt
let kind t = t.kind
let offset t = if Int64.compare t.offset 0L < 0 then None else Some t.offset
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
  | Some o -> Format.fprintf ppf "fst: %s: %s (offset %Ld)" (kind_to_string t.kind) t.message o
  | None -> Format.fprintf ppf "fst: %s: %s" (kind_to_string t.kind) t.message

let to_string t = Format.asprintf "%a" pp t
let catch f = try Ok (f ()) with E e -> Error e
