[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic"]

module V = Fst.Vcd

type mode = Push | Pull

let definition b d =
  let add fmt = Printf.bprintf b fmt in
  match d with
  | V.Date s -> add "$date\n\t%s\n$end\n" s
  | Version s -> add "$version\n\t%s\n$end\n" s
  | Timezero z -> add "$timezero\n\t%Ld\n$end\n" z
  | Timescale s -> add "$timescale\n\t%s\n$end\n" s
  | Scope { scope_type; name } -> add "$scope %s %s $end\n" scope_type name
  | Upscope -> add "$upscope $end\n"
  | Comment s -> add "$comment\n\t%s\n$end\n" s
  | Attr_begin { attr_type; subtype; name; arg } ->
      add "$attrbegin %s %s %s %Ld $end\n" attr_type subtype name arg
  | Attr_end -> add "$attrend $end\n"
  | Var { var_type; size; id; name; _ } -> add "$var %s %d %s %s $end\n" var_type size id name
  | End_definitions -> add "$enddefinitions $end\n"

let bits b off width =
  String.init width (fun j ->
      if (Char.code (Bytes.get b (off + (j lsr 3))) lsr (7 - (j land 7))) land 1 = 1 then '1'
      else '0')

let render ?extensions ?select ?(packed = false) ~mode t =
  let b = Buffer.create 4096 in
  let add fmt = Printf.bprintf b fmt in
  let id = V.id in
  let text prefix h s off len = add "%c%s %s\n" prefix (Bytes.sub_string s off len) (id h) in
  let sink =
    {
      V.definition = definition b;
      time = (fun t -> add "#%Lu\n" t);
      frame = (fun _ -> failwith "frame");
      dump =
        (function
        | Dumpvars -> add "$dumpvars\n"
        | Dumpvars_end -> add "$end\n"
        | Dumpon -> add "$dumpon $end\n"
        | Dumpoff -> add "$dumpoff $end\n");
      bit = (fun h c -> add "%c%s\n" c (id h));
      vector = text 'b';
      packed =
        (if packed then Some (fun h s off w -> add "b%s %s\n" (bits s off w) (id h)) else None);
      port = text 'p';
      real = (fun h d -> add "r%s %s\n" (Fst.Private.g16 d) (id h));
      varlen = (fun h s off len -> add "s%s %s\n" (V.escape s off len) (id h));
    }
  in
  let run =
    match mode with
    | Push -> V.events ?extensions ?select t sink
    | Pull ->
        Result.bind (V.reader ?extensions ?select t sink) (fun r ->
            let first = V.step r in
            let ends = "$enddefinitions $end\n" and s = Buffer.contents b in
            let n = String.length ends in
            if
              first = Ok true
              && not (String.length s >= n && String.sub s (String.length s - n) n = ends)
            then failwith "the first step passed more than the definitions";
            let rec go = function
              | Ok true -> go (V.step r)
              | Ok false -> Ok ()
              | Error e -> Error e
            in
            go first)
  in
  Result.map (fun () -> Buffer.contents b) run

let definitions ?extensions t =
  let l = ref [] in
  let sink = { V.ignore_sink with definition = (fun d -> l := d :: !l) } in
  Result.map (fun () -> List.rev !l) (V.events ?extensions t sink)

let resumed ~block t =
  let l = ref [] in
  let add fmt = Printf.ksprintf (fun s -> l := s :: !l) fmt in
  let sink =
    {
      V.ignore_sink with
      time = (fun t -> add "#%Lu" t);
      frame = (fun t -> add "frame %Lu" t);
      dump =
        (function
        | Dumpvars | Dumpvars_end -> failwith "dumpvars in a resumed read"
        | Dumpon -> add "dumpon"
        | Dumpoff -> add "dumpoff");
      bit = (fun h c -> add "%c %d" c h);
      vector = (fun h s off len -> add "b%s %d" (Bytes.sub_string s off len) h);
      port = (fun h s off len -> add "b%s %d" (Bytes.sub_string s off len) h);
      real = (fun h d -> add "r%Lx %d" (Int64.bits_of_float d) h);
      varlen = (fun h s off len -> add "s%S %d" (Bytes.sub_string s off len) h);
    }
  in
  Result.bind (V.reader ~block t sink) (fun r ->
      let rec go = function
        | Ok true -> go (V.step r)
        | Ok false -> Ok (List.rev !l)
        | Error e -> Error e
      in
      go (V.step r))
