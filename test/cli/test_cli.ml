[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic"]

module F = Fst_fixture

let failures = ref 0

let check name ok =
  if not ok then begin
    incr failures;
    Printf.printf "FAIL %s\n%!" name
  end

let contains s sub =
  let n = String.length sub in
  let rec go i = i + n <= String.length s && (String.sub s i n = sub || go (i + 1)) in
  go 0

let read path = In_channel.with_open_bin path In_channel.input_all
let spec = F.make [ F.signal "a" F.Bit ] [ F.block ~times:[ 10L; 20L ] [ (0, 1, F.B '1') ] ]

(* Runs [exe args] with [stdout] as given, returning the exit status and what it wrote to stderr. *)
let run exe args ~stdout =
  let err = Filename.temp_file "cli" ".err" in
  let efd = Unix.openfile err [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  let pid = Unix.create_process exe (Array.of_list (exe :: args)) Unix.stdin stdout efd in
  let _, status = Unix.waitpid [] pid in
  Unix.close efd;
  let text = read err in
  Sys.remove err;
  (status, text)

let () =
  let exe = Sys.argv.(1) in
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let fst = Filename.temp_file "cli" ".fst" in
  Out_channel.with_open_bin fst (fun oc -> output_string oc (F.build spec));
  let nul = Unix.openfile Filename.null [ Unix.O_WRONLY ] 0 in
  (* Control: an output that works. *)
  let status, err = run exe [ fst ] ~stdout:nul in
  check "control exits 0" (status = Unix.WEXITED 0 && err = "");
  let failed name (status, err) =
    check (name ^ ": exits nonzero") (match status with Unix.WEXITED n -> n <> 0 | _ -> false);
    check (name ^ ": no exception: " ^ err) (not (contains err "exception" || contains err "Fatal"));
    check (name ^ ": says why: " ^ err) (err <> "")
  in
  if Sys.file_exists "/dev/full" then
    failed "-o /dev/full" (run exe [ fst; "-o"; "/dev/full" ] ~stdout:nul);
  (* A reader that went away. *)
  let r, w = Unix.pipe () in
  Unix.close r;
  failed "closed pipe" (run exe [ fst ] ~stdout:w);
  Unix.close w;
  Unix.close nul;
  Sys.remove fst;
  if !failures > 0 then exit 1
