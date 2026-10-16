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

val iter_changes : t -> sink -> (unit, Error.t) result

type reader
(** The value changes of a file, read one value change block at a time, so that only that
    block is decompressed in memory. *)

val reader : t -> sink -> (reader, Error.t) result
(** Reads the hierarchy; no value change block yet. *)

val step : reader -> (bool, Error.t) result
(** Passes the changes of the next value change block to the sink, which is only called
    from [step]. [Ok false] when no block remains. After an error, or an exception raised by
    the sink, no block remains. Together, the steps make the same calls as {!iter_changes}. *)

(** VCD output. *)
module Vcd : sig
  val write : ?extensions:bool -> t -> out_channel -> (unit, Error.t) result
  (** Writes what libfst's [fst2vcd] writes; [extensions] is its [-e] (attributes as
      [$attrbegin]). *)

  val to_string : ?extensions:bool -> t -> (string, Error.t) result
end

(**/**)

(* Internals for the tests. *)
module Private : sig
  val fastlz : string -> size:int -> string option
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
