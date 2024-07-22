val operation_info : string Patch.t Zipper.t -> Nottui.ui
(** [operation_info zipper] returns a UI element that displays information about the
    current operation in the zipper. *)

val change_summary : string Patch.t Zipper.t -> Nottui.ui
(** [change_summary zipper] returns a UI element that displays a summary of the changes
    that will be made by the current operation in the zipper. *)

val current_operation : string Patch.t Zipper.t -> Nottui.ui
(** [current_operation zipper] returns a UI element that displays the current operation
    in the zipper. *)
