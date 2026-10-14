[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic, OpenAI"]

let () =
  let dir = Sys.argv.(1) in
  List.iter
    (fun f -> Printf.printf "%s %s\n" (Digest.to_hex (Digest.file (Filename.concat dir f))) f)
    (Common.files dir ".vcd")
