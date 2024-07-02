(** Render and navigate through a diff. *)

val start : ?term:Notty_unix.Term.t -> Patch.t list -> unit
val start_test : Patch.t list -> char list -> int -> int -> unit
