[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic"]

(* One record per value change: "time handle v:CHARS", "r:HEX64" or "s:HEX", as [fst_ref calls]
   prints them. *)

let hex s =
  String.concat ""
    (List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) (List.of_seq (String.to_seq s)))

(* The changes of [Fst.reader_at t ~block:k], the frame's included, at the time they are reported.
   The strings that [reader_at] recovers from earlier blocks come between the frame and the first
   time: libfst's frame has none, so they are left out. *)
let reader_at t ~block =
  let out = ref [] and now = ref 0L and head = ref false in
  let add h s = out := Printf.sprintf "%Lu %d %s" !now h s :: !out in
  let sink =
    {
      Fst.time =
        (fun t ->
          now := t;
          head := false);
      frame =
        (fun t ->
          now := t;
          head := true);
      bit = (fun h c -> add h ("v:" ^ String.make 1 c));
      vector = (fun h b off len -> add h ("v:" ^ Bytes.sub_string b off len));
      real = (fun h d -> add h (Printf.sprintf "r:%016Lx" (Int64.bits_of_float d)));
      varlen =
        (fun h b off len -> if not !head then add h ("s:" ^ hex (Bytes.sub_string b off len)));
    }
  in
  let rec drain r =
    match Fst.step r with Ok true -> drain r | Ok false -> Ok () | Error e -> Error e
  in
  match Result.bind (Fst.reader_at t sink ~block) drain with
  | Ok () -> Ok (List.rev !out)
  | Error e -> Error (Fst.Error.to_string e)

(* What libfst's callbacks report for [path], from block [start] on if a time range start is
   given. [exe] is fst_ref. *)
let libfst exe ?start path =
  let out = Filename.temp_file "calls" ".txt" in
  let cmd =
    Printf.sprintf "%s calls %s%s > %s" (Filename.quote exe) (Filename.quote path)
      (match start with Some s -> Printf.sprintf " %Lu" s | None -> "")
      (Filename.quote out)
  in
  let rc = Sys.command cmd in
  let text = In_channel.with_open_bin out In_channel.input_all in
  Sys.remove out;
  if rc <> 0 then Error (Printf.sprintf "fst_ref calls exited with %d" rc)
  else Ok (List.filter (fun l -> l <> "") (String.split_on_char '\n' text))

(* The start time that makes libfst skip exactly the blocks before [k]: past the end of all of
   them, and not past the end of any block from [k] on. None where no start time does: block ends
   that do not increase, an end time of UINT64_MAX before [k]. *)
let start_for (bs : Fst.block array) k =
  let n = Array.length bs in
  if k <= 0 || k >= n then None
  else begin
    let before = ref 0L in
    for j = 0 to k - 1 do
      if Int64.unsigned_compare bs.(j).end_time !before > 0 then before := bs.(j).end_time
    done;
    if Int64.equal !before (-1L) then None
    else begin
      let s = Int64.succ !before in
      let ok = ref true in
      for m = k to n - 1 do
        if Int64.unsigned_compare bs.(m).end_time s < 0 then ok := false
      done;
      if !ok then Some s else None
    end
  end

(* A description of the first record where [expected] and [actual] differ. *)
let difference ~expected ~actual =
  let rec go i = function
    | e :: es, a :: acts when e = a -> go (i + 1) (es, acts)
    | [], [] -> None
    | e :: _, a :: _ -> Some (Printf.sprintf "record %d: libfst %S, reader_at %S" i e a)
    | e :: _, [] -> Some (Printf.sprintf "record %d: libfst %S, reader_at has ended" i e)
    | [], a :: _ -> Some (Printf.sprintf "record %d: libfst has ended, reader_at %S" i a)
  in
  go 0 (expected, actual)
