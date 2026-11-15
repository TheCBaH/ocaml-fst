(** Minimal valid FST files, written with stored (uncompressed) sections: a header, a hierarchy
    without the gzip magic, and plain value change blocks (type 1). A spec describes the file;
    the [set_*] functions return a spec with one field replaced by a hostile value. *)

type kind = Bit | Vector of int | Real | String

type signal = {
  name : string;
  kind : kind;
  alias : int option;  (** The 1-based handle this one aliases; [kind] is then only the type. *)
  decl_len : int option;  (** Overrides the hierarchy's declared length. *)
  var_type : int option;  (** Overrides the hierarchy's variable type. *)
}

(** [B] is one of [01xzhuwl-?]. [V] has one character per bit. *)
type value = B of char | V of string | R of float | S of string

type block = {
  start_time : int64;
  end_time : int64;
  times : int64 list;  (** The time table, in increasing order. *)
  frame : (int * value) list;  (** Initial values by handle; the others default to unknown. *)
  changes : (int * int * value) list;  (** [(time index, handle, value)], handles 1-based. *)
  packtype : char;
  mem_required : int64 option;  (** Overrides the traversal memory the block declares. *)
  chain_dest : (int * int) list;  (** [(handle, dest)]: the chain's leading size varint. *)
  chain_table : string option;  (** Replaces the encoded chain table. *)
  trailer : (int * int * int) option;  (** Replaces the time table's [(uclen, clen, nitems)]. *)
}

type t = { timescale : int; scope : string; signals : signal list; blocks : block list }

val signal : ?alias:int -> string -> kind -> signal

val block :
  ?start:int64 -> ?frame:(int * value) list -> times:int64 list -> (int * int * value) list -> block
(** Starts at [start], by default the first time, and ends at the last time. *)

val make : ?timescale:int -> signal list -> block list -> t
val build : t -> string

val set_length : t -> signal:int -> int -> t
(** The hierarchy's declared length of the [signal]th variable (aliases included, 1-based).
    Reals take that many bytes in the frame. *)

val set_var_type : t -> signal:int -> int -> t
(** The hierarchy's variable type of the [signal]th variable (aliases included, 1-based), e.g.
    18 for a port. *)

val set_chain_dest : t -> block:int -> handle:int -> int -> t
(** Also sets the block's declared traversal memory to [dest], so that the chain is not skipped
    for exceeding it. *)

val set_chain_table : t -> block:int -> string -> t
(** Blocks are 0-based in the [set_*] functions. *)

val set_trailer : t -> block:int -> uclen:int -> clen:int -> nitems:int -> t

val set_alias_target : t -> signal:int -> int -> t
(** Makes the [signal]th variable (1-based) an alias of the given handle. *)

val varint : int -> string
(** LEB128. *)
