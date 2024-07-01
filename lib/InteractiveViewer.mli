(** This module defines the main user interface and interaction logic for a diff viewer application using the Nottui library. It provides mechanisms to navigate through patches and view them in different formats, respond to user inputs, and run test simulations. *)

val view : Patch.t list -> Nottui.ui Lwd.t
(** [view patches] constructs a reactive UI for navigating and viewing a list of patches.
    It supports toggling between help view and normal view, and handling navigation commands.
    @param patches A list of patches to be displayed and navigated through.
    @return A reactive UI element constructed using Lwd combinators. *)

val start : Patch.t list -> unit
(** [start patches] initializes and runs the main UI loop for the application, displaying the UI constructed by [view].
    @param patches A list of patches to be processed and displayed in the UI.
    @return Executes indefinitely, handling UI events until termination conditions are met (e.g., user quits). *)

val start_test : Patch.t list -> char list -> int -> int -> unit
(** [start_test patch events width height] facilitates automated UI testing by simulating keypress events and rendering the UI to the terminal.
    This function is useful for automated or manual testing of UI behaviors in response to specific inputs.
    @param patch A list of patches to be displayed in the test.
    @param events A list of characters representing simulated keypresses to interact with the UI.
    @param width The width of the rendering area in characters.
    @param height The height of the rendering area in characters.
    @return Outputs the resulting UI state to the terminal, providing visual feedback on the UI's response to the simulated events. *)
