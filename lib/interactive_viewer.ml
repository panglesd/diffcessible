open Nottui
module W = Nottui_widgets
open Lwd_infix

let operation_info z_patches : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  W.string
    ~attr:Notty.A.(fg lightcyan)
    (Printf.sprintf "Operation %d of %d"
       (Zipper.get_current_index z + 1)
       (Zipper.get_total_length z))

let ui_of_operation operation =
  let green_bold_attr = Notty.A.(fg green ++ st bold) in
  let red_bold_attr = Notty.A.(fg red ++ st bold) in
  let blue_bold_attr = Notty.A.(fg blue ++ st bold) in
  match operation with
  | Patch.Create path ->
      Ui.hcat [ W.string "Creation of "; W.string ~attr:green_bold_attr path ]
  | Patch.Delete path ->
      Ui.hcat [ W.string "Deletion of "; W.string ~attr:red_bold_attr path ]
  | Patch.Rename (old_path, new_path) ->
      Ui.hcat
        [
          W.string "Rename with modifications ";
          W.string ~attr:blue_bold_attr old_path;
          W.string " to ";
          W.string ~attr:green_bold_attr new_path;
        ]
  | Patch.Rename_only (old_path, new_path) ->
      Ui.hcat
        [
          W.string "Rename ";
          W.string ~attr:blue_bold_attr old_path;
          W.string " to ";
          W.string ~attr:green_bold_attr new_path;
        ]
  | Patch.Edit path ->
      Ui.hcat
        [ W.string "Modification of "; W.string ~attr:blue_bold_attr path ]

let string_of_hunk = Format.asprintf "%a" Patch.pp_hunk

let current_operation z_patches : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  let p = Zipper.get_focus z in
  ui_of_operation p.Patch.operation

let compute_additions_removals (hunk : Patch.hunk) : int * int =
  let add_count, remove_count =
    List.fold_left
      (fun (adds, rems) line ->
        match line with
        | `Mine _ -> (adds, rems + 1) (* `Mine line represents a removal *)
        | `Their _ -> (adds + 1, rems) (* `Their line represents an addition *)
        | `Common _ -> (adds, rems)
        (* `Common lines do not count towards additions or removals *))
      (0, 0) hunk.lines
  in
  (add_count, remove_count)

let current_hunks z_patches : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  let p = Zipper.get_focus z in
  let hunks =
    List.map
      (fun h -> W.string ~attr:Notty.A.(fg lightblue) (string_of_hunk h))
      p.Patch.hunks
  in

  (* compute additions and removals *)
  let add_count, remove_count =
    List.fold_left
      (fun (adds, rems) hunk ->
        let a, r = compute_additions_removals hunk in
        (adds + a, rems + r))
      (0, 0) p.Patch.hunks
  in

  (* helper function to conditionally append elements based on count *)
  let append_if_positive count make_string acc =
    match count with 0 -> acc | _ -> acc @ [ make_string count ]
  in

  let summary_strings = [] in
  let summary_strings =
    append_if_positive
      (List.length p.Patch.hunks)
      (fun c ->
        W.string ~attr:Notty.A.(fg yellow) (Printf.sprintf "Total hunks: %d" c))
      summary_strings
  in
  let summary_strings =
    append_if_positive add_count
      (fun c ->
        W.string
          ~attr:Notty.A.(fg green)
          (Printf.sprintf "Total additions: %d" c))
      summary_strings
  in
  let summary_strings =
    append_if_positive remove_count
      (fun c ->
        W.string ~attr:Notty.A.(fg red) (Printf.sprintf "Total removals: %d" c))
      summary_strings
  in
  let combined = hunks @ summary_strings in
  Ui.vcat combined

type direction = Prev | Next

let navigate z_patches (dir : direction) : unit =
  let z = Lwd.peek z_patches in
  match dir with
  | Prev -> Lwd.set z_patches (Zipper.prev z)
  | Next -> Lwd.set z_patches (Zipper.next z)

(* let pure_str s = Lwd.pure (W.string s) *)
let quit = Lwd.var false

let view (patches : Patch.t list) =
  let z_patches : 'a Zipper.t Lwd.var =
    match Zipper.zipper_of_list patches with
    | Some z -> Lwd.var z
    | None -> failwith "zipper_of_list: empty list"
  in
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
           (W.string
              "Type 'q' to quit, 'n' to go to the next operation, 'p' to go to \
               the previous operation");
    ]

let start patch = Ui_loop.run ~quit ~tick_period:0.2 (view patch)
