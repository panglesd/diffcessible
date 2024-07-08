module W = Nottui_widgets
val operation_info : Patch.t Zipper.t Lwd.var -> Nottui.ui Lwd.t
val ui_of_operation : Patch.operation -> Nottui.ui
val string_of_hunk : Patch.hunk -> string
val current_operation : Patch.t Zipper.t Lwd.var -> Nottui.ui Lwd.t
val current_hunks : Patch.t Zipper.t Lwd.var -> Nottui.ui Lwd.t
type direction = Prev | Next
val navigate : 'a Zipper.t Lwd.var -> direction -> unit
val quit : bool Lwd.var
val help : bool Lwd.var
val additions_and_removals :
  [< `Common of 'a | `Mine of 'b | `Their of 'c ] list -> int * int
val accumulate_count : Patch.hunk list -> int * int
val change_summary : Patch.t Zipper.t Lwd.var -> Nottui.ui Lwd.t
type view_mode = SideBySide | Normal
val view_mode : view_mode Lwd.var
val toggle_view_mode : unit -> unit
type line = Change of string | Common of string | Empty
val split_and_align_hunk :
  [< `Common of string | `Mine of string | `Their of string > `Common ] list ->
  line list -> line list -> line list * line list
val lines_with_numbers : line list -> Notty.attr -> string -> Nottui.ui list
val create_summary :
  int -> int -> Notty.attr -> [< `Add | `Remove ] -> Nottui.ui
val ui_of_hunk_side_by_side : Patch.hunk -> Nottui.ui
val current_hunks_side_by_side : Patch.t Zipper.t Lwd.var -> Nottui.ui Lwd.t
val view : Patch.t list -> Nottui.ui Lwd.t
val start : ?term:Notty_unix.Term.t -> Patch.t list -> unit
val start_test : Patch.t list -> char list -> int -> int -> unit
