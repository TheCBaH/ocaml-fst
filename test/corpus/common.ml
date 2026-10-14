[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic, OpenAI"]

let read_file path = In_channel.with_open_bin path In_channel.input_all

let files dir suffix =
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f suffix)
  |> List.sort compare
