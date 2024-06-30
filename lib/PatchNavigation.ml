open Nottui
module W = Nottui_widgets
open Lwd_infix

type direction = Prev | Next

let navigate z_patches (dir : direction) : unit =
  let z = Lwd.peek z_patches in
  match dir with
  | Prev -> Lwd.set z_patches (Zipper.prev z)
  | Next -> Lwd.set z_patches (Zipper.next z)

let quit = Lwd.var false

let additions_and_removals lines =
  let add_line (additions, removals) line =
    match line with
    | `Their _ -> (additions + 1, removals)
    | `Mine _ -> (additions, removals + 1)
    | `Common _ -> (additions, removals)
  in
  List.fold_left add_line (0, 0) lines

let accumulate_count hunks =
  List.fold_left
    (fun (add_acc, remove_acc) hunk ->
      let add_in_hunk, remove_in_hunk =
        additions_and_removals hunk.Patch.lines
      in
      (add_acc + add_in_hunk, remove_acc + remove_in_hunk))
    (0, 0) hunks

let change_summary z_patches : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  let p = Zipper.get_focus z in
  let total_additions, total_removals = accumulate_count p.Patch.hunks in
  let format_plural n singular plural =
    if n = 1 then Printf.sprintf "%d %s" n singular
    else Printf.sprintf "%d %s" n plural
  in
  let operation_count =
    Printf.sprintf "%s, %s"
      (format_plural total_additions "addition" "additions")
      (format_plural total_removals "removal" "removals")
  in
  W.string ~attr:Notty.A.(fg lightcyan) operation_count
