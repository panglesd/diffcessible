(** This module defines UI components for displaying information about operations in a diff viewer using the Nottui library. These components visualize different types of patch operations like creation, deletion, and edits. *)

val operation_info : Patch.t Zipper.t -> Nottui.ui
(** [operation_info z_patches] creates a reactive UI element that displays information about the current operation in a zipper of patches.
    It shows the current operation index, the total number of operations, and the number of hunks in the current patch.
    @param z_patches A reactive variable containing a zipper of patches.
    @return A reactive UI element that displays operation-related information. *)

val change_summary : Patch.t Zipper.t -> Nottui.ui
(** [change_summary z_patches] creates a reactive UI element that displays a summary of the changes in the current operation.
    It shows the number of lines added, removed, and modified in the current patch.
    @param z_patches A reactive variable containing a zipper of patches.
    @return A reactive UI element that displays a summary of the changes in the current operation. *)

val current_operation : Patch.t Zipper.t -> Nottui.ui
(** [current_operation z_patches] generates a reactive UI element that displays the current operation from a zipper of patches.
    This function uses internal mechanisms to render the details of the operation at the current focus of the zipper.
    @param z_patches A reactive variable containing a zipper of patches.
    @return A reactive UI element that displays the details of the current operation based on the focus of the zipper. *)
