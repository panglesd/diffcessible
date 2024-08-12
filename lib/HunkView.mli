type rendering_mode = Color | TextMarkers

val current_hunks : string Patch.t Zipper.t -> rendering_mode -> Nottui.ui
(** [current_hunks zipper] returns the current hunks in a patch zipper for
    normal view. *)

val current_hunks_side_by_side :
  string Patch.t Zipper.t -> rendering_mode -> Nottui.ui
(** [current_hunks_side_by_side zipper] returns the current hunks in a patch
    zipper for side-by-side view. *)
