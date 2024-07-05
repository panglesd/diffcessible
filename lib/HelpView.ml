open Nottui
module W = Nottui_widgets

let header_attr = Notty.A.(fg lightred ++ st bold)
let shortcut_attr = Notty.A.(fg cyan)

let help_panel =
  Ui.vcat
    [
      W.string ~attr:header_attr "Help Panel:\n";
      W.string ~attr:shortcut_attr "h:   Open the help panel";
      W.string ~attr:shortcut_attr "q:   Quit the diffcessible viewer";
      W.string ~attr:shortcut_attr "n:   Move to the next operation, if present";
      W.string ~attr:shortcut_attr
        "p:   Move to the previous operation, if present";
      W.string ~attr:shortcut_attr "t:   Toggle view mode";
      W.string ~attr:shortcut_attr "l:   Toggle line numbers";
    ]
