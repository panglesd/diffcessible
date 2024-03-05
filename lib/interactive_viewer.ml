open Nottui
module W = Nottui_widgets
open Lwd_infix
open Zipper

let zipper_of_list (lst : 'a list) : 'a t Lwd.var = Lwd.var (zipper_of_list lst)

let string_of_operation (op : Patch.operation) : string =
  match op with
  | Edit e -> Printf.sprintf "Edited:\n%s" e
  | Rename (n1, n2) -> Printf.sprintf "Renamed with modifications:\n %s\nto\n%s" n1 n2
  | Delete d -> Printf.sprintf "Deleted:\n%s" d
  | Create c -> Printf.sprintf "Created:\n%s" c
  | Rename_only (n1, n2) -> Printf.sprintf "Renamed:\n%s\nto\n%s" n1 n2

let attr_of_operation (op : Patch.operation) : Notty.A.t =
  match op with
  | Edit _ -> Notty.A.(fg lightblue)
  | Rename _ -> Notty.A.(fg lightgreen)
  | Delete _ -> Notty.A.(fg lightred)
  | Create _ -> Notty.A.(fg lightyellow)
  | Rename_only _ -> Notty.A.(fg lightmagenta) 

let string_of_hunk (hunk : Patch.hunk) : string =
  Format.asprintf "%a" Patch.pp_hunk hunk

let operation_info (z_patches : Patch.t t Lwd.var) : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  W.string ~attr:Notty.A.(fg lightcyan) (Printf.sprintf "Operation %d of %d" (get_current_index z + 1) (get_total_length z))

let current_operation (z_patches : Patch.t t Lwd.var) : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  let p = get_focus z in
  let attr = attr_of_operation p.Patch.operation in
  W.string ~attr (string_of_operation p.Patch.operation)

let compute_additions_removals (hunk : Patch.hunk) : (int * int) =
  let add_count, remove_count =
    List.fold_left (fun (adds, rems) line ->
      match line with
      | `Mine _ -> (adds, rems + 1)    (* `Mine line represents a removal *)
      | `Their _ -> (adds + 1, rems)   (* `Their line represents an addition *)
      | `Common _ -> (adds, rems)      (* `Common lines do not count towards additions or removals *)
    ) (0, 0) hunk.lines
  in
  (add_count, remove_count)

let current_hunks (z_patches : Patch.t t Lwd.var) : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  let p = get_focus z in
  let hunks = List.map (fun h -> W.string ~attr:Notty.A.(fg lightblue) (string_of_hunk h)) p.Patch.hunks in

  (* compute additions and removals *)
  let add_count, remove_count = List.fold_left (fun (adds, rems) hunk ->
    let a, r = compute_additions_removals hunk in
    (adds + a, rems + r)
  ) (0, 0) p.Patch.hunks in

  (* helper function to conditionally append elements based on count *)
  let append_if_positive count make_string acc =
    match count with
    | 0 -> acc
    | _ -> acc @ [make_string count]
  in

  let summary_strings = [] in
  let summary_strings = append_if_positive (List.length p.Patch.hunks)
    (fun c -> W.string ~attr:Notty.A.(fg yellow) (Printf.sprintf "Total hunks: %d" c))
    summary_strings in
  let summary_strings = append_if_positive add_count
    (fun c -> W.string ~attr:Notty.A.(fg green) (Printf.sprintf "Total additions: %d" c))
    summary_strings in
  let summary_strings = append_if_positive remove_count
    (fun c -> W.string ~attr:Notty.A.(fg red) (Printf.sprintf "Total removals: %d" c))
    summary_strings in
  let combined = hunks @ summary_strings in
  Ui.vcat combined

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
