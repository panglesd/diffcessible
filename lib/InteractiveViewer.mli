val start : ?term:Notty_unix.Term.t -> string Patch.t list -> unit
(** [start patches] starts diffcessible with the given patches *)

val start_test : string Patch.t list -> char list -> int -> int -> unit
(** [start_test patches input start_line start_col] starts diffcessible with the given patches and input *)
