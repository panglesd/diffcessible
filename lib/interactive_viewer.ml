open Nottui
module W = Nottui_widgets
open Lwd_infix

let index_v = Lwd.var 0
let index = Lwd.get index_v

let current_patch patches =
  let$ index = index in
  List.nth_opt patches index

(* let string_of_operation = Format.asprintf "%a" (Patch.pp_operation ~git:false) *)

let string_of_operation operation =
  match operation with
  | Patch.Create path -> "Creation of `" ^ path ^ "`"
  | Patch.Rename (old_path, new_path) ->
      "Rename from `" ^ old_path ^ "` to `" ^ new_path ^ "`"
  | Patch.Delete path -> "Deletion of `" ^ path ^ "`"
  | Patch.Edit path -> "Modification of `" ^ path ^ "`"
  | Patch.Rename_only (old_path, new_path) ->
    "Rename only from `" ^ old_path ^ "` to `" ^ new_path ^ "`"

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
