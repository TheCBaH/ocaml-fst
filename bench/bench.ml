[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic"]

(* Usage: bench GROUP [options]; GROUP is conv, sparse, seek, hier or micro. Each group prints a
   summary table and appends its repetitions to a CSV. *)

let ok = function Ok v -> v | Error e -> failwith (Fst.Error.to_string e)
let reps = ref 5
let warmup = ref 1
let csv_path = ref "bench.csv"
let trace = ref "_bench/random.fst"
let tag = ref ""
let ref_exes = ref []
let hier_fst = ref "_build/default/test/reference/corpus/a2_big_hier_z.fst"
let seek_blocks = ref 1000
let hier_vars = ref 100_000
let only = ref ""

type row = {
  variant : string;
  rep : int;
  wall : float;
  cpu : float;
  minor : float;
  promoted : float;
  major : float;
  minor_gcs : int;
  major_gcs : int;
  bytes : float;
  heap_mb : float; (* OCaml top heap so far *)
  rss_mb : int; (* process peak RSS so far; the converter's own for an external one *)
  info : string;
}

let word_mb = float_of_int (Sys.word_size / 8) /. 1e6
let alloc_mb r = (r.minor +. r.major -. r.promoted) *. word_mb
let group = ref ""
let rows : row list ref = ref []

let csv_row r =
  Printf.sprintf "%s,%s,%d,%.6f,%.6f,%.3f,%.0f,%.0f,%.0f,%d,%d,%.0f,%.1f,%d,%s" !group r.variant
    r.rep r.wall r.cpu (alloc_mb r) r.minor r.promoted r.major r.minor_gcs r.major_gcs r.bytes
    r.heap_mb r.rss_mb r.info

let header =
  "group,variant,rep,wall_s,cpu_s,alloc_mb,minor_words,promoted_words,major_words,minor_gcs,major_gcs,out_bytes,top_heap_mb,peak_rss_mb,info"

let append line =
  let fresh = not (Sys.file_exists !csv_path) in
  let oc = open_out_gen [ Open_append; Open_creat ] 0o644 !csv_path in
  if fresh then output_string oc (header ^ "\n");
  output_string oc (line ^ "\n");
  close_out oc

let record r =
  rows := r :: !rows;
  append (csv_row r)

let vm_hwm_kb pid =
  try
    let ic = open_in (Printf.sprintf "/proc/%s/status" pid) in
    let rec go () =
      match input_line ic with
      | l when String.length l > 6 && String.sub l 0 6 = "VmHWM:" ->
          Scanf.sscanf l "VmHWM: %d" (fun n -> n)
      | _ -> go ()
    in
    let r = try go () with End_of_file -> 0 in
    close_in ic;
    r
  with Sys_error _ -> 0

(* [f] returns the bytes it produced (0 if none) and a note. *)
let measure variant rep f =
  Gc.full_major ();
  let g0 = Gc.quick_stat () and c0 = Sys.time () and w0 = Unix.gettimeofday () in
  let bytes, info = f () in
  let w1 = Unix.gettimeofday () and c1 = Sys.time () and g1 = Gc.quick_stat () in
  {
    variant;
    rep;
    wall = w1 -. w0;
    cpu = c1 -. c0;
    minor = g1.minor_words -. g0.minor_words;
    promoted = g1.promoted_words -. g0.promoted_words;
    major = g1.major_words -. g0.major_words;
    minor_gcs = g1.minor_collections - g0.minor_collections;
    major_gcs = g1.major_collections - g0.major_collections;
    bytes;
    heap_mb = float_of_int g1.top_heap_words *. word_mb;
    rss_mb = vm_hwm_kb "self" / 1024;
    info;
  }

let selected variant =
  !only = ""
  ||
  let n = String.length !only and m = String.length variant in
  let rec at i = i + n <= m && (String.sub variant i n = !only || at (i + 1)) in
  at 0

let bench variant f =
  if selected variant then begin
    Printf.eprintf "  %s\n%!" variant;
    for _ = 1 to !warmup do
      ignore (f ())
    done;
    for rep = 1 to !reps do
      record (measure variant rep f)
    done
  end

(* An external converter: wall time, its CPU time, and its peak RSS sampled while it runs. *)
let bench_exe variant exe args =
  let once rep =
    let t0 = (Unix.times ()).tms_cutime +. (Unix.times ()).tms_cstime in
    let w0 = Unix.gettimeofday () in
    let pid =
      Unix.create_process exe (Array.of_list (exe :: args)) Unix.stdin Unix.stdout Unix.stderr
    in
    let hwm = ref 0 in
    let rec wait () =
      match Unix.waitpid [ Unix.WNOHANG ] pid with
      | 0, _ ->
          hwm := max !hwm (vm_hwm_kb (string_of_int pid));
          Unix.sleepf 0.001;
          wait ()
      | _, Unix.WEXITED 0 -> ()
      | _ -> failwith (exe ^ " failed")
    in
    wait ();
    let w1 = Unix.gettimeofday () in
    let t = Unix.times () in
    {
      variant;
      rep;
      wall = w1 -. w0;
      cpu = t.tms_cutime +. t.tms_cstime -. t0;
      minor = 0.;
      promoted = 0.;
      major = 0.;
      minor_gcs = 0;
      major_gcs = 0;
      bytes = 0.;
      heap_mb = 0.;
      rss_mb = !hwm / 1024;
      info = "";
    }
  in
  if selected variant then begin
    Printf.eprintf "  %s\n%!" variant;
    for _ = 1 to !warmup do
      ignore (once 0)
    done;
    for rep = 1 to !reps do
      record (once rep)
    done
  end

let sorted f l = List.sort compare (List.map f l)
let median l = List.nth l (List.length l / 2)

let summary () =
  let rows = List.rev !rows in
  let variants =
    List.fold_left (fun a r -> if List.mem r.variant a then a else a @ [ r.variant ]) [] rows
  in
  Printf.printf "\n### %s (%d repetitions, median / min)\n\n" !group !reps;
  print_endline
    "| variant | wall s med | wall s min | cpu s med | alloc MB med | VCD MB/s med | heap / RSS MB \
     | note |";
  print_endline "| --- | ---: | ---: | ---: | ---: | ---: | ---: | --- |";
  List.iter
    (fun v ->
      let rs = List.filter (fun r -> r.variant = v) rows in
      let w = sorted (fun r -> r.wall) rs in
      let mbs =
        match (List.hd rs).bytes with 0. -> "-" | b -> Printf.sprintf "%.1f" (b /. 1e6 /. median w)
      in
      let last = List.nth rs (List.length rs - 1) in
      Printf.printf "| %s | %.3f | %.3f | %.3f | %.1f | %s | %.0f / %d | %s |\n" v (median w)
        (List.hd w)
        (median (sorted (fun r -> r.cpu) rs))
        (median (sorted alloc_mb rs))
        mbs last.heap_mb last.rss_mb (List.hd rs).info)
    variants

type counts = {
  mutable times : int;
  mutable frames : int;
  mutable bits : int;
  mutable vectors : int;
  mutable reals : int;
  mutable strings : int;
  mutable packed : int;
}

let counting () =
  let c = { times = 0; frames = 0; bits = 0; vectors = 0; reals = 0; strings = 0; packed = 0 } in
  let sink =
    {
      Fst.time = (fun _ -> c.times <- c.times + 1);
      frame = (fun _ -> c.frames <- c.frames + 1);
      bit = (fun _ _ -> c.bits <- c.bits + 1);
      vector = (fun _ _ _ _ -> c.vectors <- c.vectors + 1);
      real = (fun _ _ -> c.reals <- c.reals + 1);
      varlen = (fun _ _ _ _ -> c.strings <- c.strings + 1);
    }
  in
  (c, sink)

let changes c = c.bits + c.vectors + c.reals + c.strings + c.packed

let note c =
  Printf.sprintf "times=%d changes=%d (bit %d, vec %d, packed %d, real %d, str %d)" c.times
    (changes c) c.bits c.vectors c.packed c.reals c.strings

let file_size path = (Unix.LargeFile.stat path).st_size
let read_file path = In_channel.with_open_bin path In_channel.input_all

(* Plain I/O error handling is not needed here: a failure aborts the run. *)
let null_vcd write =
  let oc = open_out_bin Filename.null in
  write oc;
  let n = LargeFile.pos_out oc in
  close_out oc;
  Int64.to_float n

let conv () =
  let path = !trace in
  Printf.printf "trace %s: %.1f MB\n" path (Int64.to_float (file_size path) /. 1e6);
  let vcd_file () =
    let t = ok (Fst.open_file path) in
    let n = null_vcd (fun oc -> ok (Fst.Vcd.write t oc)) in
    Fst.close t;
    (n, "")
  in
  bench "ocaml_vcd_write" vcd_file;
  List.iteri
    (fun i exe ->
      bench_exe (if i = 0 then "libfst_ref" else "libfst_ref_o2") exe [ "vcd"; path; Filename.null ])
    !ref_exes;
  let s = lazy (read_file path) in
  if selected "ocaml_vcd_write_in_memory" then ignore (Lazy.force s);
  bench "ocaml_vcd_write_in_memory" (fun () ->
      let t = ok (Fst.of_string (Lazy.force s)) in
      (null_vcd (fun oc -> ok (Fst.Vcd.write t oc)), ""));
  let iter name ?select ?packed () =
    bench name (fun () ->
        let t = ok (Fst.open_file path) in
        let c, sink = counting () in
        let packed = Option.map (fun () _ _ _ _ -> c.packed <- c.packed + 1) packed in
        ok (Fst.iter_changes ?select ?packed t sink);
        Fst.close t;
        (0., note c))
  in
  iter "iter_all_expanded" ();
  iter "iter_all_packed" ~packed:() ();
  iter "iter_select_none" ~select:(fun _ -> false) ();
  bench "blocks_only" (fun () ->
      let t = ok (Fst.open_file path) in
      let n = Array.length (ok (Fst.blocks t)) in
      Fst.close t;
      (0., Printf.sprintf "blocks=%d" n));
  bench "open_and_signals" (fun () ->
      let t = ok (Fst.open_file path) in
      let n = Array.length (ok (Fst.signals t)) in
      Fst.close t;
      (0., Printf.sprintf "handles=%d" n))

let sparse () =
  let path = !trace in
  let t = ok (Fst.open_file path) in
  let handles = Array.length (ok (Fst.signals t)) in
  Fst.close t;
  Printf.printf "trace %s: %.1f MB, %d handles\n" path
    (Int64.to_float (file_size path) /. 1e6)
    handles;
  let run name select =
    bench name (fun () ->
        let t = ok (Fst.open_file path) in
        let c, sink = counting () in
        ok (Fst.iter_changes ?select t sink);
        Fst.close t;
        (0., note c))
  in
  run "full" None;
  run "select_10pct" (Some (fun h -> h mod 10 = 0));
  run "select_1pct" (Some (fun h -> h mod 100 = 0));
  run "select_one_handle" (Some (fun h -> h = handles / 2));
  run "select_none" (Some (fun _ -> false))

(* Deterministic shuffle. *)
let shuffle n =
  let a = Array.init n Fun.id and st = Random.State.make [| 12345 |] in
  for i = n - 1 downto 1 do
    let j = Random.State.int st (i + 1) in
    let x = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- x
  done;
  a

let string_fixture nblocks =
  let open Fst_fixture in
  let nstr = 12 and nbit = 20 and nvec = 8 in
  let signals =
    List.init nstr (fun i -> signal (Printf.sprintf "s%d" i) String)
    @ List.init nbit (fun i -> signal (Printf.sprintf "b%d" i) Bit)
    @ List.init nvec (fun i -> signal (Printf.sprintf "v%d" i) (Vector 16))
  in
  let steps = 8 in
  let block b =
    let times = List.init steps (fun j -> Int64.of_int ((b * 1000) + (j * 10))) in
    let strs =
      List.concat
        (List.init nstr (fun s ->
             if (b + s) mod 7 = 0 then [ (b mod steps, s + 1, S (Printf.sprintf "msg-%d-%d" s b)) ]
             else []))
    in
    let bits =
      List.init nbit (fun i ->
          ((b + i) mod steps, nstr + i + 1, B (if (b + i) land 1 = 0 then '0' else '1')))
    in
    let vecs =
      List.concat
        (List.init nvec (fun v ->
             List.init steps (fun j ->
                 ( j,
                   nstr + nbit + v + 1,
                   V (String.init 16 (fun k -> if (b + j + v + k) land 3 = 0 then '1' else '0')) ))))
    in
    (* One change per variable and time step, in handle order within a step. *)
    let changes =
      List.sort (fun (t1, h1, _) (t2, h2, _) -> compare (t1, h1) (t2, h2)) (strs @ bits @ vecs)
    in
    Fst_fixture.block ~times changes
  in
  Fst_fixture.build (Fst_fixture.make signals (List.init nblocks block))

let seek () =
  let nblocks = !seek_blocks in
  let data = string_fixture nblocks in
  let order = shuffle nblocks in
  Printf.printf
    "string fixture: %d blocks, %d bytes, 12 string variables; %d seeks in random order\n" nblocks
    (String.length data) nblocks;
  let seek_all ?select t order =
    let c, sink = counting () in
    Array.iter
      (fun k ->
        let r = ok (Fst.reader_at ?select t sink ~block:k) in
        ignore (ok (Fst.step r)))
      order;
    c
  in
  let fresh ?select () =
    let c = ref None in
    Array.iter
      (fun k ->
        let t = ok (Fst.of_string data) in
        let c', sink = counting () in
        let r = ok (Fst.reader_at ?select t sink ~block:k) in
        ignore (ok (Fst.step r));
        c := Some c')
      order;
    Option.get !c
  in
  let nostr h = h > 12 in
  bench "strings_one_handle" (fun () ->
      let t = ok (Fst.of_string data) in
      (0., note (seek_all t order)));
  bench "strings_fresh_handle" (fun () -> (0., note (fresh ())));
  bench "no_strings_one_handle" (fun () ->
      let t = ok (Fst.of_string data) in
      (0., note (seek_all ~select:nostr t order)));
  bench "no_strings_fresh_handle" (fun () -> (0., note (fresh ~select:nostr ())));
  bench "strings_one_handle_ascending" (fun () ->
      let t = ok (Fst.of_string data) in
      (0., note (seek_all t (Array.init nblocks Fun.id))));
  bench "sequential_iter_changes" (fun () ->
      let t = ok (Fst.of_string data) in
      let c, sink = counting () in
      ok (Fst.iter_changes t sink);
      (0., note c));
  let path = !trace in
  if Sys.file_exists path then begin
    let t = ok (Fst.open_file path) in
    let n = Array.length (ok (Fst.blocks t)) in
    Fst.close t;
    let order = shuffle n in
    bench (Printf.sprintf "verilator_trace_%d_seeks_one_handle" n) (fun () ->
        let t = ok (Fst.open_file path) in
        let c = seek_all t order in
        Fst.close t;
        (0., note c))
  end

let synthetic_hier n =
  let open Fst_fixture in
  let signals =
    List.init n (fun i ->
        signal
          (Printf.sprintf "signal_%06d_padding_padding_padding_padding_pad" i)
          (if i mod 5 = 0 then Vector 8 else Bit))
  in
  let times = [ 0L; 10L; 20L ] in
  let changes =
    List.init 20 (fun j ->
        (j mod 3, (j * 997 mod n) + 1, if j * 997 mod n mod 5 = 0 then V "10101010" else B '1'))
    |> List.sort (fun (t1, h1, _) (t2, h2, _) -> compare (t1, h1) (t2, h2))
  in
  Fst_fixture.build (Fst_fixture.make signals [ Fst_fixture.block ~times changes ])

let hier () =
  let n = !hier_vars in
  let synth = synthetic_hier n in
  Printf.printf "synthetic hierarchy: %d variables, %d bytes\n" n (String.length synth);
  let file = !hier_fst in
  let target name open_ =
    let handles t = Array.length (ok (Fst.signals t)) in
    bench (name ^ "_scan") (fun () ->
        let t = open_ () in
        Fst.close t;
        (0., ""));
    bench (name ^ "_signals") (fun () ->
        let t = open_ () in
        (0., Printf.sprintf "handles=%d" (handles t)));
    bench (name ^ "_fold_hierarchy") (fun () ->
        let t = open_ () in
        let k = ok (Fst.fold_hierarchy t ~init:0 ~f:(fun a _ -> a + 1)) in
        (0., Printf.sprintf "entries=%d" k));
    bench (name ^ "_vcd_write") (fun () ->
        let t = open_ () in
        (null_vcd (fun oc -> ok (Fst.Vcd.write t oc)), ""))
  in
  target "synthetic" (fun () -> ok (Fst.of_string synth));
  if Sys.file_exists file then begin
    target "a2_big_hier" (fun () -> ok (Fst.open_file file));
    List.iteri
      (fun i exe ->
        bench_exe
          (if i = 0 then "a2_big_hier_libfst_ref" else "a2_big_hier_libfst_ref_o2")
          exe [ "vcd"; file; Filename.null ])
      !ref_exes
  end

(* Printing the same $var lines two ways: what Vcd.definitions does, and the Buffer.add_*
   alternative of P5.5. *)
let micro () =
  let n = !hier_vars in
  let names =
    Array.init n (fun i -> Printf.sprintf "signal_%06d_padding_padding_padding_padding_pad" i)
  in
  let ids = Array.init n (fun i -> Fst.Vcd.id (i + 1)) in
  let widths = Array.init n (fun i -> if i mod 5 = 0 then 8 else 1) in
  let size = ref 0. in
  bench "var_lines_bprintf" (fun () ->
      let b = Buffer.create 65536 in
      let total = ref 0 in
      for i = 0 to n - 1 do
        Printf.bprintf b "$var %s %d %s %s $end\n" "wire" widths.(i) ids.(i) names.(i);
        if Buffer.length b >= 65536 then begin
          total := !total + Buffer.length b;
          Buffer.clear b
        end
      done;
      size := float_of_int (!total + Buffer.length b);
      (!size, ""));
  bench "var_lines_buffer_add" (fun () ->
      let b = Buffer.create 65536 in
      let total = ref 0 in
      for i = 0 to n - 1 do
        Buffer.add_string b "$var ";
        Buffer.add_string b "wire";
        Buffer.add_char b ' ';
        Buffer.add_string b (string_of_int widths.(i));
        Buffer.add_char b ' ';
        Buffer.add_string b ids.(i);
        Buffer.add_char b ' ';
        Buffer.add_string b names.(i);
        Buffer.add_string b " $end\n";
        if Buffer.length b >= 65536 then begin
          total := !total + Buffer.length b;
          Buffer.clear b
        end
      done;
      (float_of_int (!total + Buffer.length b), ""));
  (* Per-value cost of a varint decoder that returns a pair against one that does not. *)
  let data = Bytes.init 1_000_000 (fun i -> Char.chr (if i land 3 = 3 then 5 else 0x85)) in
  let lim = Bytes.length data in
  let rec go b pos acc shift =
    let c = Char.code (Bytes.unsafe_get b pos) in
    let acc = acc lor ((c land 0x7f) lsl shift) in
    if c land 0x80 <> 0 then go b (pos + 1) acc (shift + 7) else (acc, pos + 1)
  in
  let pairs () =
    let p = ref 0 and s = ref 0 in
    while !p < lim - 8 do
      let v, q = go data !p 0 0 in
      s := !s + v;
      p := q
    done;
    !s
  in
  let pos = ref 0 in
  let rec go2 b p acc shift =
    let c = Char.code (Bytes.unsafe_get b p) in
    let acc = acc lor ((c land 0x7f) lsl shift) in
    if c land 0x80 <> 0 then go2 b (p + 1) acc (shift + 7)
    else begin
      pos := p + 1;
      acc
    end
  in
  let unboxed () =
    let p = ref 0 and s = ref 0 in
    while !p < lim - 8 do
      let v = go2 data !p 0 0 in
      s := !s + v;
      p := !pos
    done;
    !s
  in
  bench "varint_pair_x100" (fun () ->
      let s = ref 0 in
      for _ = 1 to 100 do
        s := !s + pairs ()
      done;
      (0., Printf.sprintf "sum=%d" !s));
  bench "varint_unboxed_x100" (fun () ->
      let s = ref 0 in
      for _ = 1 to 100 do
        s := !s + unboxed ()
      done;
      (0., Printf.sprintf "sum=%d" !s))

let () =
  let g = ref None in
  let spec =
    [
      ("-r", Arg.Set_int reps, "N repetitions (default 5)");
      ("-w", Arg.Set_int warmup, "N discarded warm-up runs (default 1)");
      ("--csv", Arg.Set_string csv_path, "FILE results (appended)");
      ("--trace", Arg.Set_string trace, "FST a large FST file (default _bench/random.fst)");
      ( "--ref",
        Arg.String (fun s -> ref_exes := !ref_exes @ [ s ]),
        "EXE libfst converter, run as EXE vcd IN OUT (repeatable)" );
      ("--tag", Arg.Set_string tag, "NAME label for the group in the results");
      ("--only", Arg.Set_string only, "STR only the variants whose name contains STR");
      ("--hier", Arg.Set_string hier_fst, "FST a file with a large hierarchy");
      ("--blocks", Arg.Set_int seek_blocks, "N blocks of the seek fixture (default 1000)");
      ("--vars", Arg.Set_int hier_vars, "N variables of the synthetic hierarchy (default 100000)");
    ]
  in
  Arg.parse (Arg.align spec)
    (fun a -> g := Some a)
    "bench (conv|sparse|seek|hier|micro) [OPTION]...";
  match !g with
  | Some (("conv" | "sparse" | "seek" | "hier" | "micro") as name) ->
      group := if !tag = "" then name else name ^ ":" ^ !tag;
      (match name with
      | "conv" -> conv ()
      | "sparse" -> sparse ()
      | "seek" -> seek ()
      | "hier" -> hier ()
      | _ -> micro ());
      summary ()
  | _ ->
      prerr_endline "usage: bench (conv|sparse|seek|hier|micro) [OPTION]...";
      exit 2
