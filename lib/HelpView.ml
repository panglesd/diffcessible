open Nottui
module W = Nottui_widgets

(* Colors for the help panel *)
let header_color = Notty.A.(fg lightred ++ st bold)
let action_color = Notty.A.(fg green)
let info_color = Notty.A.(fg yellow)
let help_visible = Lwd.var false

(* Help panel content *)
let help_panel =
  Ui.vcat
    [
      W.string ~attr:header_color "Help Panel:\n";
      W.string ~attr:action_color "h:   Open the help panel";
      W.string ~attr:action_color "q:   Quit the diffcessible viewer";
      W.string ~attr:info_color "n:   Move to the next operation, if present";
      W.string ~attr:info_color
        "p:   Move to the previous operation, if present";
      W.string ~attr:info_color "t:   Toggle view mode";
      W.string ~attr:info_color "l:   Toggle line numbers";
    ]

(* Keyboard area for interacting with the help panel *)
let help_keyboard_area =
  Ui.keyboard_area
    (function
      | `ASCII 'q', [] ->
          Lwd.set help_visible false;
          `Handled
      | _ -> `Unhandled)
    (W.string ~attr:Notty.A.(fg lightblue) "Type 'q' to exit the help panel")

(* Function to toggle the visibility of the help panel *)
let toggle_help_visibility () =
  Lwd.set help_visible (not (Lwd.peek help_visible))
