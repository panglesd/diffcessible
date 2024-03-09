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
  let z_patches : 'a Zipper.t Lwd.var =
    match Zipper.zipper_of_list patches with
    | Some z -> Lwd.var z
    | None -> failwith "zipper_of_list: empty list"
  in
  let help_panel = Ui.vcat
  [
     W.string "h:  This key can be used to toggle the help panel";
     W.string "q:  This key can be used to quit the diffcessible viewer";
     W.string "n:  This key can be used to move to the next operation, if present";
     W.string "p:  This key can be used to move to the previous operation, if present" ;
  ] in
  let help = Lwd.var false  in
  let help_visible=Lwd.get help in
  if Lwd.peek help_visible then W.vbox[
  Lwd.pure @@ help_panel;
  Lwd.pure
      @@ Ui.keyboard_area
           (function
             | `ASCII 'q', [] ->
                 Lwd.set quit true;
                 `Handled
             | `ASCII 'h', [] ->
                 Lwd.set help_visible false;             
                 `Handled
             | _ -> `Unhandled)
            (W.string "Type 'q' to quit, 'h' to exit the help panel");

   ]
  else
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
                 navigate z_patches Prev;
                 `Handled
             | `ASCII 'h', [] ->
                 Lwd.set help_visible true;
                 `Handled
             | `ASCII 'h', [] ->
                 Lwd.set help true;
                 `Handled
             | _ -> `Unhandled)
           (W.string
              "Type 'q' to quit, 'n' to go to the next operation, 'p' to go to \
               the previous operation, 'h' to go to the help panel");
    ]

let start patch = Ui_loop.run ~quit ~tick_period:0.2 (view patch)
