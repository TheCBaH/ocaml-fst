[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic"]

module F = Fst_fixture

let failures = ref 0

let check name ok =
  if not ok then begin
    incr failures;
    Printf.printf "FAIL %s\n%!" name
  end

let sink =
  {
    Fst.time = ignore;
    frame = ignore;
    bit = (fun _ _ -> ());
    vector = (fun _ _ _ _ -> ());
    real = (fun _ _ -> ());
    varlen = (fun _ _ _ _ -> ());
  }

(* [f] returns an [Error] of [kind]; [Ok] or an exception fails. *)
let expect name kind f =
  match f () with
  | Error e -> check (Printf.sprintf "%s: %s" name (Fst.Error.to_string e)) (Fst.Error.kind e = kind)
  | Ok () -> check (name ^ ": Ok") false
  | exception e -> check (name ^ ": raised " ^ Printexc.to_string e) false

let open_ s =
  match Fst.of_string (F.build s) with Ok t -> t | Error e -> failwith (Fst.Error.to_string e)

let drain r =
  let rec go () =
    match Fst.step r with Ok true -> go () | Ok false -> Ok () | Error e -> Error e
  in
  go ()

(* Every entry point that decodes value changes. *)
let entry_points name kind spec =
  let t = open_ spec in
  expect (name ^ " iter_changes") kind (fun () -> Fst.iter_changes t sink);
  expect (name ^ " Vcd.to_string") kind (fun () -> Result.map ignore (Fst.Vcd.to_string t));
  expect (name ^ " reader/step") kind (fun () -> Result.bind (Fst.reader t sink) drain);
  Fst.close t

(* D2: a real whose declared length is not 8. *)
let () =
  let spec =
    F.make
      [ F.signal "a" F.Bit; F.signal "r" F.Real ]
      [
        F.block ~start:5L
          ~frame:[ (2, F.R 1.5) ]
          ~times:[ 10L; 20L ]
          [ (0, 1, F.B '1'); (1, 2, F.R 2.5) ];
      ]
  in
  let t = open_ spec in
  check "real of 8 bytes is fine" (Fst.iter_changes t sink = Ok ());
  Fst.close t;
  List.iter
    (fun n ->
      entry_points
        (Printf.sprintf "real length %d" n)
        Fst.Error.Malformed (F.set_length spec ~signal:2 n))
    [ 0; 4; 7; 9; 16 ]

(* D3: a chain declares a huge size, which must be refused before any buffer is allocated. *)
let () =
  let spec =
    F.make
      [ F.signal "a" F.Bit; F.signal "v" (F.Vector 8) ]
      [ F.block ~times:[ 10L; 20L ] [ (0, 1, F.B '1'); (0, 2, F.V "10100101") ] ]
  in
  let dest = min (1 lsl 30) (Sys.max_string_length - 1024) in
  let t = open_ (F.set_chain_dest spec ~block:0 ~handle:2 dest) in
  let before = Gc.allocated_bytes () in
  let r = Fst.iter_changes t sink in
  let allocated = Gc.allocated_bytes () -. before in
  (match r with
  | Error e -> check "huge dest: Limit_exceeded" (Fst.Error.kind e = Fst.Error.Limit_exceeded)
  | Ok () -> check "huge dest: Ok" false);
  check (Printf.sprintf "huge dest allocated %.0f bytes" allocated) (allocated < 1e6);
  Fst.close t

let () = if !failures > 0 then exit 1
