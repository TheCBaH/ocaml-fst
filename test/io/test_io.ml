[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic"]

module F = Fst_fixture

let failures = ref 0

let check name ok =
  if not ok then begin
    incr failures;
    Printf.printf "FAIL %s\n%!" name
  end

let spec =
  F.make
    [ F.signal "a" F.Bit; F.signal "v" (F.Vector 8) ]
    [
      F.block ~times:[ 10L; 20L ] [ (0, 1, F.B '1'); (1, 2, F.V "10100101") ];
      F.block ~times:[ 20L; 30L ] [ (0, 1, F.B '0') ];
    ]

let data = F.build spec

let sink =
  {
    Fst.time = ignore;
    frame = ignore;
    bit = (fun _ _ -> ());
    vector = (fun _ _ _ _ -> ());
    real = (fun _ _ -> ());
    varlen = (fun _ _ _ _ -> ());
  }

let is_io name = function
  | Error e -> check (name ^ ": " ^ Fst.Error.to_string e) (Fst.Error.kind e = Fst.Error.Io)
  | Ok _ -> check (name ^ ": expected an I/O error") false

let drain r =
  let rec go () =
    match Fst.step r with Ok true -> go () | Ok false -> Ok () | Error e -> Error e
  in
  go ()

(* Every entry point that reads the source, each as an [Error Io] and not an exception. *)
let all_fail name t =
  let guard what f =
    match f () with
    | r -> is_io (name ^ " " ^ what) r
    | exception e -> check (name ^ " " ^ what ^ " raised " ^ Printexc.to_string e) false
  in
  guard "blocks" (fun () -> Result.map ignore (Fst.blocks t));
  guard "fold_hierarchy" (fun () -> Fst.fold_hierarchy t ~init:() ~f:(fun () _ -> ()));
  guard "iter_changes" (fun () -> Fst.iter_changes t sink);
  guard "reader" (fun () -> Result.bind (Fst.reader t sink) drain);
  guard "reader_at" (fun () -> Result.bind (Fst.reader_at t sink ~block:1) drain);
  guard "aligned_block" (fun () -> Result.map ignore (Fst.aligned_block t 1));
  guard "Vcd.to_string" (fun () -> Result.map ignore (Fst.Vcd.to_string t))

let reader ~fail s =
  let length = Int64.of_int (String.length s) in
  let read pos len =
    if !fail then raise (Sys_error "disk on fire");
    let p = Int64.to_int pos in
    if p >= String.length s then "" else String.sub s p (min len (String.length s - p))
  in
  Fst.Private.of_reader ~length ~read ()

let temp_file s =
  let path = Filename.temp_file "io" ".fst" in
  Out_channel.with_open_bin path (fun oc -> output_string oc s);
  path

let () =
  (* A failing read while opening. *)
  (match reader ~fail:(ref true) data with
  | exception e -> check ("open, read fails raised " ^ Printexc.to_string e) false
  | r -> is_io "open, read fails" (Result.map ignore r));
  (* A failing read after opening. *)
  let fail = ref false in
  (match reader ~fail data with
  | Error e -> check ("open: " ^ Fst.Error.to_string e) false
  | Ok t ->
      fail := true;
      all_fail "read fails" t);
  (* A FIFO is not seekable, so it has no length. The read end opens without a writer because
     the read-write descriptor keeps it busy. *)
  let dir = Filename.temp_file "io" ".d" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  let fifo = Filename.concat dir "fifo" in
  Unix.mkfifo fifo 0o600;
  let hold = Unix.openfile fifo [ Unix.O_RDWR ] 0 in
  (match Fst.open_file fifo with
  | exception e -> check ("open_file fifo raised " ^ Printexc.to_string e) false
  | r -> is_io "open_file fifo" (Result.map ignore r));
  Unix.close hold;
  Sys.remove fifo;
  Unix.rmdir dir;
  (* Use after close. *)
  let path = temp_file data in
  (match Fst.open_file path with
  | Error e -> check ("open_file: " ^ Fst.Error.to_string e) false
  | Ok t ->
      check "reads before close" (Result.is_ok (Fst.Vcd.to_string t));
      Fst.close t;
      Fst.close t;
      all_fail "after close" t);
  Sys.remove path;
  if !failures > 0 then exit 1
