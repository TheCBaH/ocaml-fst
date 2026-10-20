(** Reading FST waveforms and converting them to VCD.

    FST is the compressed waveform format of {{:https://github.com/gtkwave/gtkwave}GTKWave},
    written by Verilator, Icarus Verilog, GHDL and others. There is no official specification; the
    reference is libfst's [fstapi.c], whose reader this library follows closely enough that
    {!Vcd} output is byte-for-byte that of libfst's [fst2vcd].

    All four compressions are decoded in OCaml: zlib and gzip with
    {{:https://erratique.ch/software/zipc}zipc}, LZ4 with
    {{:https://github.com/TheCBaH/ocaml-unlz4}unlz4}, and FastLZ here. Malformed input yields an
    {!Error.t}, never an exception. *)

(** Errors. *)
module Error : sig
  type kind =
    | Truncated  (** The input ends inside a block or record. *)
    | Malformed  (** A block or record breaks the format. *)
    | Unsupported  (** A feature this reader does not handle. *)
    | Decompression  (** A compressed section does not decode to its declared size. *)
    | Limit_exceeded  (** A size does not fit the platform's [int] or strings. *)
    | Io  (** The file cannot be opened or read. *)

  type t

  val kind : t -> kind

  val offset : t -> int64 option
  (** The file offset of the offending block or field, when known. For hierarchy records, it is
      the offset into the decompressed hierarchy, plus the block's offset. *)

  val message : t -> string
  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end

(** The header block. *)
module Header : sig
  type t = {
    start_time : int64;  (** Unsigned, in timescale units, like every time here. *)
    end_time : int64;
    little_endian : bool;  (** The byte order of the writer's doubles. *)
    writer_memory : int64;
    scope_count : int64;
    var_count : int64;  (** Including aliases. *)
    max_handle : int64;  (** Distinct variables. *)
    vc_section_count : int64;  (** As declared; the file may have fewer. *)
    timescale : int;  (** A power of ten of seconds: [-9] is 1 ns. *)
    version : string;  (** The writer. *)
    date : string;  (** [asctime] output, with its newline. *)
    file_type : int;  (** 0 Verilog, 1 VHDL, 2 mixed. *)
    timezero : int64;
  }
end

(** The hierarchy: scopes, variables and attributes in declaration order. *)
module Hier : sig
  type entry =
    | Scope of { scope_type : int; name : string; component : string }
        (** An empty name becomes [$unnamed_scope_N], as in libfst. *)
    | Upscope
    | Attr_begin of { attr_type : int; subtype : int; name : string; arg : int64 }
    | Attr_end
    | Var of {
        var_type : int;
        direction : int;
        name : string;
        length : int;  (** As declared: bits, 8 for reals, 0 for strings, 3n+2 for ports. *)
        handle : int;  (** 1-based. *)
        alias : bool;  (** [handle] is another variable's. *)
      }

  val var_type_name : int -> string option
  (** E.g. [wire], as in VCD. *)

  val scope_type_name : int -> string option
  val is_real : int -> bool
end

type t
(** An open file. *)

val open_file : string -> (t, Error.t) result
val of_string : string -> (t, Error.t) result

val close : t -> unit
(** Closes the file of {!open_file}. *)

val header : t -> Header.t

val blackouts : t -> (int64 * bool) list
(** [$dumpoff] ([false]) and [$dumpon] ([true]) times. *)

val geometry : t -> (int64 array, Error.t) result
(** Each variable's width as the geometry block records it: 0 for reals, [0xFFFFFFFF] for
    zero-length variables. *)

val fold_hierarchy : t -> init:'a -> f:('a -> Hier.entry -> 'a) -> ('a, Error.t) result

type sink = {
  time : int64 -> unit;  (** Changes that follow are at this time, which differs from the last. *)
  frame : int64 -> unit;
      (** The initial values of the first block follow, at this time. libfst emits them only
          when the block starts before its first change. *)
  bit : int -> char -> unit;  (** One of [01xzhuwl-?]. *)
  vector : int -> bytes -> int -> int -> unit;
      (** One character per bit, most significant first. *)
  real : int -> float -> unit;
  varlen : int -> bytes -> int -> int -> unit;  (** A string variable's raw bytes. *)
}
(** Receives the value changes, in time order. Handles are 1-based and never aliases; bytes are
    only valid during the call and must not be modified. *)

val iter_changes :
  ?select:(int -> bool) ->
  ?packed:(int -> bytes -> int -> int -> unit) ->
  t ->
  sink ->
  (unit, Error.t) result
(** [select], when given, is called once per handle before the first block is read, and the
    sink then receives the [bit], [vector], [real] and [varlen] calls of the selected handles
    only: the others are neither decompressed nor traversed. [time] and [frame] calls are
    unchanged, so the calls are those without [select] minus the unselected handles' ones.

    [packed h bytes off width], when given, receives each changed vector that the file stores as
    0 and 1 only, instead of [sink.vector]: [(width + 7) / 8] bytes, most significant bit
    first, valid only during the call. Vectors with other values, the frame's vectors, reals and
    strings go to the sink as before. *)

type reader
(** The value changes of a file, read one value change block at a time, so that only that
    block is decompressed in memory. *)

val reader :
  ?select:(int -> bool) ->
  ?packed:(int -> bytes -> int -> int -> unit) ->
  t ->
  sink ->
  (reader, Error.t) result
(** Reads the hierarchy; no value change block yet. [select] is as in {!iter_changes}, called
    once per handle by the first {!step}, not here; later changes to what it consults have no
    effect. *)

val step : reader -> (bool, Error.t) result
(** Passes the changes of the next value change block to the sink, which is only called
    from [step]. [Ok false] when no block remains. After an error, or an exception raised by
    the sink, no block remains. Together, the steps make the same calls as {!iter_changes}. *)

type block = { start_time : int64; end_time : int64; pos : int }
(** A value change block: the times in its header and its file offset. *)

val blocks : t -> (block array, Error.t) result
(** The value change blocks {!iter_changes} reads, in order. Only each block's first 16 bytes
    after its length are read: no frame, chain or time table is decoded. (A gzip-wrapped file is
    inflated whole when it is opened.) *)

val reader_at :
  ?select:(int -> bool) ->
  ?packed:(int -> bytes -> int -> int -> unit) ->
  t ->
  sink ->
  block:int ->
  (reader, Error.t) result
(** A {!reader} that starts at the [block]th of {!blocks} (0-based; an error if there is none).
    [select] and [packed] are as in {!reader}.

    [block = 0] is {!reader}. For a later block, the first step first passes a frame: [frame]
    with the block's start time and every selected handle's value after all changes of the
    earlier blocks and before any of this block's, including the selected string variables'
    (found by reading only those handles' chains in the earlier blocks), then the block's
    changes. [time] is called for the block's first time even when the previous block ended on
    it, so a timestamp that spans the boundary is seen in two parts: use {!aligned_block} to
    start where none does. *)

val aligned_block : t -> int -> (int, Error.t) result
(** [aligned_block t k] is the largest [j <= k] such that no time step has changes both in block
    [j] and in an earlier block (0 if there is none): a reader at [j] then sees whole time steps.
    The blocks' time tables decide it, except where a block's first time is the one an earlier
    block ended on: those blocks are then decoded to see whether both have changes there. *)

(** VCD output. *)
module Vcd : sig
  val write : ?extensions:bool -> t -> out_channel -> (unit, Error.t) result
  (** Writes what libfst's [fst2vcd] writes; [extensions] is its [-e] (attributes as
      [$attrbegin]). *)

  val to_string : ?extensions:bool -> t -> (string, Error.t) result

  (** How {!write} spells things, for consumers that build their own VCD view. *)

  val id : int -> string
  (** A handle's identifier code. *)

  val timescale : int -> string
  (** E.g. [1ns] for [-9]. *)

  val var_length : var_type:int -> length:int -> int
  (** The [$var] size of a {!Hier.Var}. *)

  val escape : bytes -> int -> int -> string
  (** A string variable's value, escaped as in an [s] record. *)
end

(**/**)

(* Internals for the tests. *)
module Private : sig
  val fastlz : string -> size:int -> string option

  val of_reader : length:int64 -> read:(int64 -> int -> string) -> (t, Error.t) result
  (** A file over [read pos len], which returns [len] bytes or fewer at the end. *)

  val vcd_id : int -> string
  val timescale : int -> string
  val g16 : float -> string
  val escape : string -> string
  val varint : string -> int64
  val svarint : string -> int64

  val block_types : t -> int list
  (** The hierarchy block's type, then each value change block's. *)

  val packtypes : t -> (char list, Error.t) result

  val parse_chain :
    string -> alias2:bool -> vc_maxhandle:int -> vc_len:int -> (int array * int array) option
end
