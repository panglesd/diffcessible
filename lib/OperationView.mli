(** This module defines UI components for displaying information about operations in a diff viewer using the Nottui library. These components visualize different types of patch operations like creation, deletion, and edits. *)

module W = Nottui_widgets
(** Module alias for Nottui_widgets for easier access to widget creation functions. *)

val operation_info : Patch.t Zipper.t Lwd.var -> Nottui.ui Lwd.t
(** [operation_info z_patches] creates a reactive UI element that displays information about the current operation in a zipper of patches.
    It shows the current operation index, the total number of operations, and the number of hunks in the current patch.
    @param z_patches A reactive variable containing a zipper of patches.
    @return A reactive UI element that displays operation-related information. *)

val ui_of_operation : Patch.operation -> Nottui.ui
(** [ui_of_operation operation] generates a UI element that visually represents a single patch operation.
    Depending on the type of operation (create, delete, rename, etc.), it displays the operation along with paths involved in appropriate colors.
    @param operation The operation to display, which can be a creation, deletion, rename with or without modifications, or an edit.
    @return A UI element describing the operation with visual emphasis on paths and changes. *)

val current_operation : Patch.t Zipper.t Lwd.var -> Nottui.ui Lwd.t
(** [current_operation z_patches] generates a reactive UI element that displays the current operation from a zipper of patches.
    This function integrates [ui_of_operation] to render the details of the operation at the current focus of the zipper.
    @param z_patches A reactive variable containing a zipper of patches.
    @return A reactive UI element that displays the details of the current operation based on the focus of the zipper. *)

