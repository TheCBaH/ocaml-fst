[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic, OpenAI"]

(* Truncated and corrupted inputs: every conversion must end in [Ok] or [Error], never an
   exception. Covers prefixes, seeded byte mutations of the small corpus files, and libfst's own
   malformed files. *)

let failures = ref 0
let cases = ref 0

let convert name s =
  incr cases;
  try
    match Fst.of_string s with
    | Error _ -> ()
    | Ok t ->
        ignore (Fst.Vcd.to_string t);
        ignore (Fst.Vcd.to_string ~extensions:true t)
  with e ->
    let bt = Printexc.get_backtrace () in
    incr failures;
    if !failures <= 10 then Printf.printf "FAIL %s: %s\n%s" name (Printexc.to_string e) bt

let () =
  Printexc.record_backtrace true;
  let corpus = Sys.argv.(1) and malformed = Sys.argv.(2) in
  let rng = Random.State.make [| 42 |] in
  List.iter
    (fun f ->
      let s = Common.read_file (Filename.concat corpus f) in
      let n = String.length s in
      if n <= 200_000 then begin
        let step = max 1 (n / 400) in
        let i = ref 0 in
        while !i < n do
          convert (Printf.sprintf "%s[0..%d]" f !i) (String.sub s 0 !i);
          i := !i + step
        done;
        for k = 1 to 300 do
          let b = Bytes.of_string s in
          let edits = 1 + Random.State.int rng 3 in
          for _ = 1 to edits do
            Bytes.set b (Random.State.int rng n) (Char.chr (Random.State.int rng 256))
          done;
          convert (Printf.sprintf "%s mutation %d" f k) (Bytes.to_string b)
        done
      end)
    (Common.files corpus ".fst");
  List.iter
    (fun f -> convert f (Common.read_file (Filename.concat malformed f)))
    (Common.files malformed ".fst");
  Printf.printf "robust: %d cases, %d failed\n" !cases !failures;
  if !failures > 0 then exit 1
