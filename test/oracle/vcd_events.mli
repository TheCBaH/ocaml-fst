(** A reference printer of {!Fst.Vcd.events}, independent of the library's own, to compare with
    {!Fst.Vcd.to_string}. *)

type mode = Push  (** [Fst.Vcd.events]. *) | Pull  (** [Fst.Vcd.reader] and [step]. *)

val render :
  ?extensions:bool ->
  ?select:(int -> bool) ->
  ?packed:bool ->
  mode:mode ->
  Fst.t ->
  (string, Fst.Error.t) result
(** The VCD text that the events spell. [packed] asks for the packed vectors, which are expanded
    here. In [Pull] mode the first step must pass the definitions and nothing else, or [Failure]
    is raised. *)

val definitions : ?extensions:bool -> Fst.t -> (Fst.Vcd.definition list, Fst.Error.t) result

val resumed : block:int -> Fst.t -> (string list, Fst.Error.t) result
(** The calls of a [Fst.Vcd.reader ~block] after its definitions, one string each, in the format of
    the [Fst.sink] calls of [reader_at]: ["#T"], ["frame T"], ["C H"], ["bBITS H"], ["rBITS H"],
    ["sQUOTED H"]. A [Dumpvars] or [Dumpvars_end] is [Failure]; [Dumpon] and [Dumpoff] are
    ["dumpon"] and ["dumpoff"]. *)
