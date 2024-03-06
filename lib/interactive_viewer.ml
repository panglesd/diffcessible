open Nottui
module W = Nottui_widgets
open Lwd_infix

let index_v = Lwd.var 0
let index = Lwd.get index_v

let current_patch patches =
  let$ index = index in
  List.nth_opt patches index

let ui_of_operation operation =
  let green_bold_attr = Notty.A.(fg green ++ st bold) in
  let red_bold_attr = Notty.A.(fg red ++ st bold) in
  let blue_bold_attr = Notty.A.(fg blue ++ st bold) in
  match operation with
  | Patch.Create path ->
    Ui.hcat [
          W.string "Creation of ";
          W.string ~attr:green_bold_attr path; 
    ]
  | Patch.Delete path ->
    Ui.hcat [
      W.string "Deletion of ";
      W.string ~attr:red_bold_attr path;
    ]
  | Patch.Rename (old_path, new_path) ->
    Ui.hcat [
      W.string "Rename with modifications ";
      W.string ~attr:blue_bold_attr old_path;
      W.string " to ";
      W.string ~attr:green_bold_attr new_path;
    ]
  | Patch.Rename_only (old_path, new_path) ->
    Ui.hcat [
      W.string "Rename ";
      W.string ~attr:blue_bold_attr old_path;
      W.string " to ";
      W.string ~attr:green_bold_attr new_path;
    ]
  | Patch.Edit path ->
    Ui.hcat [
      W.string "Modification of ";
      W.string ~attr:blue_bold_attr path;
    ]

let string_of_hunk = Format.asprintf "%a" Patch.pp_hunk

let current_operation patches =
  let$ current_patch = current_patch patches in
  match current_patch with
  | Some p -> ui_of_operation p.Patch.operation
  | None -> W.string "No operation"

let current_hunks patches =
  let$ current_patch = current_patch patches in
  match current_patch with
  | Some p ->
      Ui.vcat @@ List.map (fun p -> W.string @@ string_of_hunk p) p.Patch.hunks
  | None -> W.string "No operation"

(* let pure_str s = Lwd.pure (W.string s) *)
let quit = Lwd.var false

let view (patches : Patch.t list) =
  W.vbox
    [
      current_operation patches;
      W.scrollbox @@ current_hunks patches;
      Lwd.pure
      @@ Ui.keyboard_area
           (function
             | `ASCII 'q', [] ->
                 Lwd.set quit true;
                 `Handled
             | `ASCII 'n', [] ->
                 Lwd.set index_v (Lwd.peek index_v + 1);
                 `Handled
             | `ASCII 'p', [] ->
                 Lwd.set index_v (Lwd.peek index_v - 1);
                 `Handled
             | _ -> `Unhandled)
           (W.string "Type 'q' to quit.");
    ]

let start patch = Ui_loop.run ~quit ~tick_period:0.2 (view patch)
