val ui_hunk_summary : Patch.hunk -> Nottui.ui
val ui_unified_diff : Patch.hunk -> Nottui.ui
val current_hunks : Patch.t Zipper.t -> Nottui.ui

type line = Change of string | Common of string | Empty

val split_and_align_hunk :
  [> `Common of string | `Mine of string | `Their of string ] list ->
  line list * line list

val lines_with_numbers : line list -> Notty.attr -> string -> Nottui.ui list
val create_summary : int -> int -> Notty.attr -> [ `Add | `Remove ] -> Nottui.ui
val ui_of_hunk_side_by_side : Patch.hunk -> Nottui.ui
val current_hunks_side_by_side : Patch.t Zipper.t -> Nottui.ui
