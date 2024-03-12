open Nottui
module W = Nottui_widgets
open Lwd_infix

let index_v = Lwd.var 0
let index = Lwd.get index_v

let current_patch patches =
  let$ index = index in
  List.nth_opt patches index

let string_of_operation = Format.asprintf "%a" (Patch.pp_operation ~git:false)
let string_of_hunk = Format.asprintf "%a" Patch.pp_hunk

let current_operation patches =
  let$ current_patch = current_patch patches in
  match current_patch with
  | Some p -> W.string @@ string_of_operation p.Patch.operation
  | None -> W.string "No operation"

let current_hunks patches =
  let$ current_patch = current_patch patches in
  match current_patch with
  | Some p ->
      Ui.vcat @@ List.map (fun p -> W.string @@ string_of_hunk p) p.Patch.hunks
  | None -> W.string "No operation"

(* let pure_str s = Lwd.pure (W.string s) *)
let quit = Lwd.var false
let help = Lwd.var false

<<<<<<< HEAD
let view (patches : Patch.t list) =
  let help_panel =
    Ui.vcat
      [
        W.string "Help Panel:\n";
        W.string "h:   Toggle the help panel";
        W.string "q:   Quit the diffcessible viewer";
        W.string "n:   Move to the next operation, if present";
        W.string "p:   Move to the previous operation, if present";
      ]
  in
  let z_patches : 'a Zipper.t Lwd.var =
    match Zipper.zipper_of_list patches with
    | Some z -> Lwd.var z
    | None -> failwith "zipper_of_list: empty list"
  in
  let ui =
    let$* help_visible = Lwd.get help in
    if help_visible then
      W.vbox
        [
          W.scrollbox @@ Lwd.pure @@ help_panel;
          Lwd.pure
          @@ Ui.keyboard_area
               (function
                 | `ASCII 'q', [] ->
                     Lwd.set help false;
                     `Handled
                 | _ -> `Unhandled)
               (W.string "Type 'q' to exit the help panel");
        ]
    else
      W.vbox
        [
          operation_info z_patches;
          current_operation z_patches;
          W.scrollbox @@ current_hunks z_patches;
          Lwd.pure
          @@ Ui.keyboard_area
               (function
                 | `ASCII 'q', [] ->
                     Lwd.set quit true;
                     `Handled
                 | `ASCII 'n', [] ->
                     navigate z_patches Next;
                     `Handled
                 | `ASCII 'p', [] ->
                     navigate z_patches Prev;
                     `Handled
                 | `ASCII 'h', [] ->
                     Lwd.set help true;
                     `Handled
                 | _ -> `Unhandled)
               (W.string
                  "Type 'q' to quit, 'n' to go to the next operation, 'p' to \
                   go to the previous operation, 'h' to go to the help panel");
        ]
  in
  W.vbox [ ui ]

let start patch = Ui_loop.run ~quit ~tick_period:0.2 (view patch)
