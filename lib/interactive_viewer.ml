open Nottui
module W = Nottui_widgets
open Lwd_infix
open Zipper

let zipper_of_list (lst : 'a list) : 'a t Lwd.var = Lwd.var (zipper_of_list lst)

(* let current_patch (lst : 'a list) : 'a Lwd.t = *)
(*   let z = zipper_of_list lst in *)
(*   Lwd.pure (get_focus (Lwd.peek z)) *)

let string_of_operation (op : Patch.operation) : string =
  match op with
  | Edit e -> Printf.sprintf "Edited %s" e
  | Rename (n1, n2) -> Printf.sprintf "Renamed with modifications %s to %s" n1 n2
  | Delete d -> Printf.sprintf "Deleted %s" d
  | Create c -> Printf.sprintf "Created %s" c
  | Rename_only (n1, n2) -> Printf.sprintf "Renamed %s to %s" n1 n2

(* let attr_of_operation (op : Patch.operation) : Notty.A.t = *)
(*   match op with *)
(*   | Edit _ -> Notty.A.(fg lightblue) *)
(*   | Rename _ -> Notty.A.(fg lightgreen) *)
(*   | Delete _ -> Notty.A.(fg lightred) *)
(*   | Create _ -> Notty.A.(fg lightyellow) *)
(*   | Rename_only _ -> Notty.A.(fg lightmagenta)  *)
(**)
(* let display_operation (op : Patch.operation) : Ui.t = *)
(*   W.string ~attr:(attr_of_operation op) (string_of_operation op) *)

let string_of_hunk (hunk : Patch.hunk) : string =
  Format.asprintf "%a" Patch.pp_hunk hunk

let operation_info (z_patches : Patch.t t Lwd.var) : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  W.string (Printf.sprintf "Operation %d out of %d" (get_current_index z + 1) (get_total_length z))

let current_operation (z_patches : Patch.t t Lwd.var) : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  let p = get_focus z in
  W.string (string_of_operation p.Patch.operation)

let current_hunks (z_patches : Patch.t t Lwd.var) : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  let p = get_focus z in
  Ui.vcat (List.map (fun h -> W.string (string_of_hunk h)) p.Patch.hunks)

type direction = Prev | Next

let navigate (z_patches : Patch.t t Lwd.var) (dir : direction) : unit =
  let z = Lwd.peek z_patches in
  match dir with
  | Prev -> Lwd.set z_patches (prev z)
  | Next -> Lwd.set z_patches (next z)
  
(* let pure_str s = Lwd.pure (W.string s) *)
let quit = Lwd.var false

let view (patches : Patch.t list) =
  let z_patches = zipper_of_list patches in 
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
             | _ -> `Unhandled)
           (W.string "Type 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation");
    ]

let start patch = Ui_loop.run ~quit ~tick_period:0.2 (view patch)
