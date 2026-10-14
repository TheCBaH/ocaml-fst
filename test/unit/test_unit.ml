[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic, OpenAI"]

module P = Fst.Private

let failures = ref 0

let check name pp expected actual =
  if expected <> actual then begin
    incr failures;
    Format.printf "FAIL %s: expected %a, got %a@." name pp expected pp actual
  end

let str = Format.pp_print_string
let qstr ppf s = Format.fprintf ppf "%S" s
let i64 ppf v = Format.fprintf ppf "%Ld" v

let opt pp ppf = function
  | None -> Format.pp_print_string ppf "None"
  | Some v -> Format.fprintf ppf "Some %a" pp v

let ints ppf a =
  Format.fprintf ppf "[%s]" (String.concat ";" (Array.to_list (Array.map string_of_int a)))

let pair ppf (a, b) = Format.fprintf ppf "(%a, %a)" ints a ints b

let () =
  List.iter
    (fun (s, v) -> check (Printf.sprintf "varint %S" s) i64 v (P.varint s))
    [
      ("\x00", 0L);
      ("\x7f", 127L);
      ("\x80\x01", 128L);
      ("\xff\xff\xff\xff\x0f", 0xFFFF_FFFFL);
      ("\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01", -1L);
    ];
  List.iter
    (fun (s, v) -> check (Printf.sprintf "svarint %S" s) i64 v (P.svarint s))
    [ ("\x7f", -1L); ("\x3f", 63L); ("\x40", -64L); ("\x80\x7f", -128L); ("\x03", 3L) ];
  List.iter
    (fun (s, size, v) -> check (Printf.sprintf "fastlz %S" s) (opt qstr) v (P.fastlz s ~size))
    [
      ("\x02abc", 3, Some "abc");
      ("\x02abc\x20\x02", 6, Some "abcabc");
      ("\x22abc\x20\x02", 6, Some "abcabc");
      ("\x00a\xe0\x05\x00", 15, Some (String.make 15 'a'));
      ("\x02abc\x20\x05", 6, None);
      ("\x02abc\x20\x02", 5, None);
      ("\x60", 1, None);
    ];
  List.iter
    (fun (n, v) -> check (Printf.sprintf "vcd_id %d" n) qstr v (P.vcd_id n))
    [ (1, "!"); (94, "~"); (95, "!!"); (96, "\"!"); (94 * 95, "~~") ];
  List.iter
    (fun (ts, v) -> check (Printf.sprintf "timescale %d" ts) str v (P.timescale ts))
    [
      (2, "100s");
      (1, "10s");
      (0, "1s");
      (-1, "100ms");
      (-3, "1ms");
      (-7, "100ns");
      (-9, "1ns");
      (-12, "1ps");
      (-20, "10zs");
      (-21, "1zs");
      (3, "1ns");
      (-22, "1ns");
    ];
  List.iter
    (fun (d, v) -> check (Printf.sprintf "g16 %h" d) str v (P.g16 d))
    [
      (0.5, "0.5");
      (1. /. 3., "0.3333333333333333");
      (-0., "-0");
      (1e300, "1e+300");
      (Float.nan, "nan");
      (Float.neg Float.nan, "-nan");
      (Float.infinity, "inf");
      (Float.neg_infinity, "-inf");
    ];
  List.iter
    (fun (s, v) -> check (Printf.sprintf "escape %S" s) qstr v (P.escape s))
    [
      ("a b", "a\\040b");
      ("\n\t\"\\?'", "\\n\\t\\\"\\\\\\?\\'");
      ("\x07\x08\x0b\x0c\r", "\\a\\b\\v\\f\\r");
      ("\x7f\xff\x00", "\\177\\377\\000");
      ("~!", "~!");
    ];
  let chain name s alias2 expected =
    check name (opt pair) expected (P.parse_chain s ~alias2 ~vc_maxhandle:4 ~vc_len:10)
  in
  chain "chain alias2" "\x03\x04\x7f" true (Some ([| 1; 0; 0; 1 |], [| 9; 0; 0; 9 |]));
  chain "chain alias" "\x03\x04\x00\x01" false (Some ([| 1; 0; 0; 1 |], [| 9; 0; 0; 9 |]));
  chain "chain offsets" "\x03\x05\x03" true (Some ([| 1; 3; 4 |], [| 2; 1; 6 |]));
  chain "chain overrun" "\x0c" false None;
  chain "chain unterminated alias" "\x03\x00" false None;
  if !failures > 0 then exit 1
