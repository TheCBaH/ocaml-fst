(** Value changes as comparable records, "time handle v:CHARS", "r:HEX64" or "s:HEX", from
    [Fst.reader_at] and from libfst's callback API ([fst_ref calls], test/reference). *)

val reader_at : Fst.t -> block:int -> (string list, string) result
(** The frame's and the block's changes, without the strings [reader_at] recovers from earlier
    blocks (libfst's frame has none). *)

val libfst : string -> ?start:int64 -> string -> (string list, string) result
(** [libfst exe ?start path]: what libfst reports for the file at [path]. With [start],
    [fstReaderSetLimitTimeRange (start, UINT64_MAX)] skips the blocks that end before it, without
    reading them. [exe] is [fst_ref]. *)

val start_for : Fst.block array -> int -> int64 option
(** The start time for which libfst skips exactly blocks 0..k-1 and reads the rest; none if
    there is none (block 0, or block ends that do not increase past it, or an end time of
    UINT64_MAX before it). Only then is libfst's read comparable with [reader_at] at [k]. *)

val difference : expected:string list -> actual:string list -> string option
(** The first record where the two differ. *)
