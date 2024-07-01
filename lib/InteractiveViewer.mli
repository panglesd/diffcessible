(** This module defines the main user interface and interaction logic for a diff viewer application using the Nottui library. It provides mechanisms to navigate through patches and view them in different formats. *)

val view : Patch.t list -> Nottui.ui Lwd.t
(** [view patches] constructs a reactive UI for navigating and viewing a list of patches. It supports toggling between different views and handling navigation commands.
    @param patches A list of patches to be displayed and navigated through.
    @return A reactive UI element constructed using Lwd combinators. *)

val start : Patch.t list -> unit
(** [start patches] initializes and runs the main UI loop for the application, displaying the UI constructed by [view].
    @param patches A list of patches to be processed and displayed in the UI.
    @return Executes indefinitely, handling UI events until termination conditions are met (e.g., user quits). *)
