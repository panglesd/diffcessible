open Nottui
module W = Nottui_widgets
open Lwd_infix
open Zipper

let index_v = Lwd.var 0
(* let index = Lwd.get index_v *)

(* convert the list of patches into a zipper *)
let zipper_of_patches patches = Lwd.var (Zipper.zipper_of_list patches)

let current_patch (patches : Patch.t list) : (Patch.t option Lwd.t) =
  let z_patches = zipper_of_patches patches in 
  let z = Lwd.peek z_patches in 
  let current_patch = Zipper.get_focus z in
  Lwd.pure (Some current_patch)


(* let current_patch patches = *)
(*   let$ index = index in *)
(*   List.nth_opt patches index *)

let string_of_operation = Format.asprintf "%a" (Patch.pp_operation ~git:false)
let string_of_hunk = Format.asprintf "%a" Patch.pp_hunk

let current_operation (patches : Patch.t list) =
  let$ current_patch = current_patch patches in
  match current_patch with
  | Some p -> W.string @@ string_of_operation p.Patch.operation
  | None -> W.string "No operation"

let current_hunks (patches : Patch.t list) =
  let$ current_patch = current_patch patches in
  match current_patch with
  | Some p ->
      Ui.vcat @@ List.map (fun p -> W.string @@ string_of_hunk p) p.Patch.hunks
  | None -> W.string "No operation"

(* * helper function to navigate between patches via `Next or `Prev *)
let navigate direction zipper_of_patches =
  let z = Lwd.peek zipper_of_patches in
  match direction with
  | `Next ->
      if List.length (Zipper.get_after z) > 0 then begin
        Lwd.set zipper_of_patches (Zipper.next z);
        Lwd.set index_v ((Lwd.peek index_v) + 1)
      end
  | `Prev ->
      if List.length (Zipper.get_before z) > 0 then begin
        Lwd.set zipper_of_patches (Zipper.prev z);
        Lwd.set index_v ((Lwd.peek index_v) - 1)
      end
(* let pure_str s = Lwd.pure (W.string s) *)
let quit = Lwd.var false

let view (patches : Patch.t list) =
  let zipper_of_patches = zipper_of_patches patches in 
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
                  navigate `Next zipper_of_patches;
                 `Handled
             | `ASCII 'p', [] ->
                  navigate `Prev zipper_of_patches;
                 `Handled
             | _ -> `Unhandled)
           (W.string "Type 'q' to quit, 'n' for next hunk, 'p' for previous hunk.");
    ]

let start patch = Ui_loop.run ~quit ~tick_period:0.2 (view patch)
