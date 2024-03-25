(** Render and navigate through a diff. *)

val start : Patch.t list -> unit
val start_test : Patch.t list -> char list -> unit
