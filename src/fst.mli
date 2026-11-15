(** Reading FST waveforms and converting them to VCD.

    FST is the compressed waveform format of {{:https://github.com/gtkwave/gtkwave}GTKWave},
    written by Verilator, Icarus Verilog, GHDL and others. There is no official specification; the
    reference is libfst's [fstapi.c], whose reader this library follows closely enough that
    {!Vcd} output is byte-for-byte that of libfst's [fst2vcd].

    All four compressions are decoded in OCaml: zlib and gzip with
    {{:https://erratique.ch/software/zipc}zipc}, LZ4 with
    {{:https://github.com/TheCBaH/ocaml-unlz4}unlz4}, and FastLZ here.

    {1 Errors}

    Damaged input, and a file or read that fails, yield an {!Error.t}: a damaged value change block
    is [Malformed], never skipped, and the file is not read past it. Exceptions are for what is not
    input: [Invalid_argument] for a bad argument ({!reader_at}, {!aligned_block}), and whatever a
    sink raises, which propagates to the caller of {!iter_changes} or {!step}.

    {1 Domains and threads}

    Nothing here is thread-safe. A {!t}, the {!reader}s made from it and their sinks belong to one
    domain or thread at a time: the handle caches {!signals} and reads its source through a
    position the readers share. Separate handles share no state and may be used from separate
    domains.

    {1 Lifetime}

    {!close} ends the use of a handle, and is harmless when repeated. Afterwards every function
    that takes the handle, or a {!reader} made from it, returns [Error Io] ("file is closed");
    this holds even where the answer is cached, and the sink gets no call. {!header} and
    {!blackouts} return what was read when the file was opened, so they still work. A {!reader}
    reads from the handle when stepped, so one that outlives {!close} fails at its next {!step},
    and dropping one needs no cleanup. The
    bytes passed to a sink are valid only during the call. An exception from a sink ends that
    {!iter_changes}, or that reader's steps, but leaves the handle usable. *)

(** Errors. *)
module Error : sig
  type kind =
    | Truncated  (** The input ends inside a block or record. *)
    | Malformed  (** A block or record breaks the format. *)
    | Unsupported
        (** A feature this reader does not handle: a gzip method other than deflate, or a file with
            both a gzip and an LZ4 hierarchy block (libfst keeps the last). *)
    | Decompression  (** A compressed section does not decode to its declared size. *)
    | Limit_exceeded  (** A size does not fit the platform's [int] or strings. *)
    | Io  (** The file cannot be opened or read. *)

  type t

  val kind : t -> kind

  val offset : t -> [ `File of int64 | `Hierarchy of int64 ] option
  (** Where the error is, when known. [`File o] is the offset of the offending block or field in the
      file. [`Hierarchy o] is an offset into the decompressed hierarchy, which no file offset
      corresponds to. [None] when no position applies, such as an I/O error. *)

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
(** Closes the file of {!open_file}; see Lifetime above. *)

val header : t -> Header.t

val blackouts : t -> (int64 * bool) list
(** [$dumpoff] ([false]) and [$dumpon] ([true]) times. *)

val geometry : t -> (int64 array, Error.t) result
(** Each variable's width as the geometry block records it: 0 for reals, [0xFFFFFFFF] for
    zero-length variables. *)

val fold_hierarchy : t -> init:'a -> f:('a -> Hier.entry -> 'a) -> ('a, Error.t) result
(** Folds over the hierarchy in declaration order. An alias to a handle that the hierarchy
    does not declare is [Malformed]. *)

type kind =
  | Bit  (** One bit: the sink's [bit]. *)
  | Vector  (** More than one bit: [vector]. *)
  | Port
      (** A [port] variable: [vector], as 3n+2 characters, or [bit] if its declared length is 1. *)
  | Real  (** [real]. *)
  | String  (** Zero length: [varlen]. *)

val signals : t -> ((kind * int) array, Error.t) result
(** The kind and declared length of each variable that is not an alias, indexed by handle minus
    one: bits, 3n+2 for a port, 8 for a real, 0 for a string. Its length is the number of
    handles, and the sink's calls for a handle are those of its kind. The hierarchy is read on
    the first call and the outcome is kept for the life of the handle: later calls return the very
    same value. An [Io] error is the exception: it is not kept, so a later call reads the
    hierarchy again. A real whose declared length is not 8 is [Malformed], here and so from
    every function that reads values. *)

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

val ignore_sink : sink
(** Ignores every call. Write [{ Fst.ignore_sink with bit = ... }] to receive some of them: the
    code then compiles unchanged if [sink] gains a field. *)

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
    strings go to the sink as before.

    A block with an inconsistent chain table or chain sizes is [Malformed], naming it by its
    index among {!blocks}; libfst skips it silently. Likewise for {!step}, {!reader_at} and
    {!Vcd}. An exception raised by the sink, [Exit] included, propagates to the caller. *)

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
    the sink (which propagates, as from {!iter_changes}), no block remains. Together, the steps make the same calls as {!iter_changes}. *)

type block = { start_time : int64; end_time : int64; pos : int64 }
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
(** A {!reader} that starts at the [block]th of {!blocks} (0-based). [select] and [packed] are as in {!reader}.

    [block = 0] is {!reader}. For a later block, the calls are those of libfst's time-limited read
    from that block ([fstReaderSetLimitTimeRange]). The first step first passes a frame: [frame]
    with the block's start time and every selected handle's value after all changes of the
    earlier blocks and before any of this block's, including the selected string variables'
    (found by reading only those handles' chains in the earlier blocks, which libfst's frame
    lacks). Then come the block's changes, as a sequential read makes them. [time] is called for
    the block's first time even when the previous block ended on it, so a timestamp that spans
    the boundary is seen in two parts: use {!aligned_block} to start where none does.

    The handle remembers what the earlier blocks held, per set of selected string handles, so
    that seeks in any order read each earlier block at most once; {!close} drops it. A damaged
    earlier block fails every seek past it, with the same error each time.

    One exception: libfst's parser leaves a chain length negative for a forward alias (one that
    names a later handle, which only a hand-built file has), and a sequential read carries it
    into a later block that lists the handle as unchanged. A seek does not carry it into the
    [block]th, as libfst's time-limited read does not, so that block's changes for such a handle
    are missing.

    @raise Invalid_argument if there is no such block: a bad argument is a caller error, not bad input. *)

val aligned_block : t -> int -> (int, Error.t) result
(** [aligned_block t k] is the largest [j <= k] such that no time step has changes both in block
    [j] and in an earlier block (0 if there is none): a reader at [j] then sees whole time steps.
    The blocks' time tables decide it, except where a block's first time is the one an earlier
    block ended on: those blocks are then decoded to see whether both have changes there.

    @raise Invalid_argument if [k] is not the index of a block, as for {!reader_at}. *)

(** VCD output, and the same content as events. *)
module Vcd : sig
  val write : ?extensions:bool -> t -> out_channel -> (unit, Error.t) result
  (** Writes what libfst's [fst2vcd] writes; [extensions] is its [-e] (attributes as
      [$attrbegin]). It is {!events} printed. *)

  val to_string : ?extensions:bool -> t -> (string, Error.t) result

  (** {1 Events}

      The content {!write} prints, as calls instead of text: the same items in the same order and
      with the same values, so that a consumer that wants a VCD's structure (a parser's, a
      viewer's) need not print and parse. The value calls are those of {!Fst.sink}, with the same
      handles (1-based, never aliases) and byte views, except that a port's vector has its own
      call and [packed] is a field. *)

  type definition =
    | Date of string  (** [$date], with the newline of the header. *)
    | Version of string
    | Timezero of int64  (** Only when it is not 0. *)
    | Timescale of string  (** E.g. [1ns]. *)
    | Scope of { scope_type : string; name : string }  (** [$scope]: [module], [task], ... *)
    | Upscope
    | Comment of string
        (** [$comment], for a hierarchy attribute of type [misc] and subtype 0: only with
            [extensions]. An empty text is [""] with its two quotes, as printed. *)
    | Attr_begin of { attr_type : string; subtype : string; name : string; arg : int64 }
        (** [$attrbegin], only with [extensions]: [attr_type] is [misc], [array], [enum] or
            [class], and [subtype] and [name] are spelt as printed. *)
    | Attr_end  (** Only with [extensions]. *)
    | Var of {
        var_type : string;  (** [wire], [reg], ... *)
        kind : kind;  (** What the value calls for this variable's handle are. *)
        size : int;  (** The [$var] size, {!var_length} of the declared length. *)
        id : string;  (** The identifier code, [id handle]. *)
        handle : int;
        alias : bool;  (** [handle] is another variable's,. *)
        name : string;
      }
    | End_definitions  (** [$enddefinitions]; the last definition. No value call precedes it. *)

  type marker =
    | Dumpvars
        (** [$dumpvars]: the values that follow, up to [Dumpvars_end], are the initial ones. It
            comes right after the first [time]. *)
    | Dumpvars_end
        (** [$end] of the [$dumpvars]; right before the second [time]. A file with a single
            time never gets one, as in the printed output. *)
    | Dumpon  (** A whole [$dumpon $end]. *)
    | Dumpoff  (** A whole [$dumpoff $end]. *)

  type sink = {
    definition : definition -> unit;
    time : int64 -> unit;  (** [#time]: the changes that follow are at this time. *)
    frame : int64 -> unit;
        (** Only from a {!reader} with [~block] above 0: the values of the block's frame follow
            at this time, up to the next [time], which is the block's first. *)
    dump : marker -> unit;
        (** [Dumpvars], [Dumpon] and [Dumpoff] come right after the [time] they are at, before its
            changes; [Dumpvars_end] right before the [time] that ends the dump. *)
    bit : int -> char -> unit;  (** One of [01xzhuwl-?]. *)
    vector : int -> bytes -> int -> int -> unit;
        (** One character per bit, most significant first; not a port's. *)
    packed : (int -> bytes -> int -> int -> unit) option;
        (** When given, receives the vectors that {!Fst.iter_changes} gives to its [packed], except
            a port's, instead of [vector]. *)
    port : int -> bytes -> int -> int -> unit;
        (** A port's vector, [3n+2] characters, which a VCD prints with [p]. *)
    real : int -> float -> unit;
        (** A VCD prints it with [%.16g] (and nan and the infinities as glibc does). *)
    varlen : int -> bytes -> int -> int -> unit;
        (** A string variable's raw bytes: {!escape} gives the text of the [s] record. *)
  }
  (** Receives the content in the order {!write} prints it. Bytes are only valid during the call
      and must not be modified. *)

  val ignore_sink : sink
  (** Ignores every call; write [{ Fst.Vcd.ignore_sink with ... }] as for {!Fst.ignore_sink}. *)

  val events : ?extensions:bool -> ?select:(int -> bool) -> t -> sink -> (unit, Error.t) result
  (** Passes the whole content to the sink: the definitions, then the changes. [extensions] is as
      for {!write}. [select] is as for {!Fst.iter_changes}: the value calls of the unselected
      handles are dropped, and nothing else is. Errors are those of {!Fst.fold_hierarchy} and of
      {!Fst.iter_changes}, with [Io] for a closed file before any call; a damaged hierarchy may
      be found after some definitions were passed on. An exception raised by the sink propagates.
      The handle stays usable. *)

  type reader
  (** {!events}, read one step at a time. *)

  val reader :
    ?extensions:bool -> ?select:(int -> bool) -> ?block:int -> t -> sink -> (reader, Error.t) result
  (** Reads the hierarchy and calls nothing yet. With [block] above 0 it starts at that block of
      {!blocks} as {!reader_at} does and the output differs from {!events}': there is no
      [Dumpvars], the first change block's [frame] stands for its time step's [#time] and
      the blackouts before the block's first time are dropped. The definitions are still passed.

      @raise Invalid_argument if [block] is given and there is no such block, as for
      {!reader_at}. *)

  val step : reader -> (bool, Error.t) result
  (** The first step passes the definitions and nothing else, each later step the content of the
      next value change block, as {!Fst.step} does. [Ok false] when none remains; after an error
      or a sink's exception none does. [Io] if the file is closed. *)

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
  val zlib : string -> off:int -> len:int -> size:int -> (string, Error.t) result

  val of_reader :
    ?close:(unit -> unit) ->
    length:int64 ->
    read:(int64 -> int -> string) ->
    unit ->
    (t, Error.t) result
  (** A file over [read pos len], which returns [len] bytes or fewer at the end. [close] is
      called exactly once: by {!close}, or by [of_reader] itself if opening fails. *)

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
