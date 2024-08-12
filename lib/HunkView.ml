open Nottui
module W = Nottui_widgets

(* Constants and Formatting Strings *)
let added_marker = "+"
let removed_marker = "-"
let unchanged_marker = " "
let empty_line_content = "        "

(* Types *)

type line = Change of string | Common of string | Empty
type rendering_mode = Color | TextMarkers

(* Utility Functions *)

let split_and_align_hunk hunks : line list * line list =
  let rec process_hunk mine_acc their_acc = function
    | [] -> (List.rev mine_acc, List.rev their_acc)
    | `Common line :: rest ->
        process_hunk (Common line :: mine_acc) (Common line :: their_acc) rest
    | changes ->
        let rec buffer_changes mine their = function
          | `Mine line :: rest ->
              buffer_changes (Change line :: mine) their rest
          | `Their line :: rest ->
              buffer_changes mine (Change line :: their) rest
          | remaining -> (List.rev mine, List.rev their, remaining)
        in
        let mine_changes, their_changes, rest = buffer_changes [] [] changes in
        let max_len =
          max (List.length mine_changes) (List.length their_changes)
        in
        let rec pad_and_append orig_acc changes i =
          if i < max_len then
            if i < List.length changes then
              pad_and_append (List.nth changes i :: orig_acc) changes (i + 1)
            else pad_and_append (Empty :: orig_acc) changes (i + 1)
          else orig_acc
        in
        let new_mine_acc = pad_and_append mine_acc mine_changes 0 in
        let new_their_acc = pad_and_append their_acc their_changes 0 in
        process_hunk new_mine_acc new_their_acc rest
  in
  process_hunk [] [] hunks

(* Styling Functions *)

let style_text (text : string) (attr : Notty.attr) (mode : rendering_mode) :
    Ui.t =
  match mode with Color -> W.string ~attr text | TextMarkers -> W.string text

let style_diff_line (prefix : string) (text : string) (mode : rendering_mode) :
    string =
  match mode with
  | Color -> text
  | TextMarkers -> Printf.sprintf "%s%s" prefix text

let style_word (word : string)
    (change_type : [ `Added | `Removed | `Unchanged ]) (mode : rendering_mode) :
    Ui.t =
  let styled_word =
    match (mode, change_type) with
    | Color, `Added -> W.string ~attr:Notty.A.(fg green) word
    | Color, `Removed -> W.string ~attr:Notty.A.(fg red) word
    | Color, `Unchanged -> W.string word
    | TextMarkers, `Added -> Ui.hcat [ W.string added_marker; W.string word ]
    | TextMarkers, `Removed ->
        Ui.hcat [ W.string removed_marker; W.string word ]
    | TextMarkers, `Unchanged -> W.string word
  in
  Ui.hcat [ styled_word; W.string " " ]

(* Rendering Functions *)

let render_hunk_summary (hunk : string Patch.hunk) (mode : rendering_mode) :
    Ui.t =
  let mine_info =
    Printf.sprintf "%d,%d" (hunk.Patch.mine_start + 1) hunk.Patch.mine_len
  in
  let their_info =
    Printf.sprintf "%d,%d" (hunk.Patch.their_start + 1) hunk.Patch.their_len
  in
  let mine_summary =
    style_text
      (Printf.sprintf "%s%s" removed_marker mine_info)
      Notty.A.(fg red)
      mode
  in
  let their_summary =
    style_text
      (Printf.sprintf "%s%s" added_marker their_info)
      Notty.A.(fg green)
      mode
  in
  let at_symbols = style_text "@@" Notty.A.(fg lightblue) mode in
  Ui.hcat
    [
      at_symbols;
      W.string " ";
      mine_summary;
      W.string " ";
      their_summary;
      W.string " ";
      at_symbols;
    ]

let render_line_number (mine_num : int) (their_num : int)
    (diff_type : [ `Added | `Removed | `Unchanged ]) : Ui.t =
  match diff_type with
  | `Added -> W.string (Printf.sprintf "   %2d + " (their_num + 1))
  | `Removed -> W.string (Printf.sprintf "%2d    - " (mine_num + 1))
  | `Unchanged ->
      W.string (Printf.sprintf "%2d %2d   " (mine_num + 1) (their_num + 1))

let render_word_diff (words : WordDiff.word list)
    (diff_type : [ `Added | `Removed | `Unchanged ]) (mode : rendering_mode) :
    Ui.t =
  let render_word = function
    | WordDiff.Changed word -> style_word word diff_type mode
    | WordDiff.Unchanged word -> style_word word `Unchanged mode
  in
  Ui.hcat (List.map render_word words)

let render_diff_line (mine_num : int) (their_num : int)
    (diff_type : [ `Added | `Removed | `Unchanged ])
    (content : WordDiff.word list) (mode : rendering_mode) : Ui.t =
  let line_number = render_line_number mine_num their_num diff_type in
  let content = render_word_diff content diff_type mode in
  Ui.hcat [ line_number; content ]

let render_hunk_lines (hunk_lines : WordDiff.line_content Patch.line list)
    (mode : rendering_mode) : Ui.t =
  let rec process_lines mine_num their_num acc = function
    | [] -> List.rev acc
    | line :: rest ->
        let new_mine, new_their, ui =
          match line with
          | `Common words ->
              ( mine_num + 1,
                their_num + 1,
                render_diff_line mine_num their_num `Unchanged words mode )
          | `Mine words ->
              ( mine_num + 1,
                their_num,
                render_diff_line mine_num their_num `Removed words mode )
          | `Their words ->
              ( mine_num,
                their_num + 1,
                render_diff_line mine_num their_num `Added words mode )
        in
        process_lines new_mine new_their (ui :: acc) rest
  in
  Ui.vcat (process_lines 0 0 [] hunk_lines)

let render_hunk (hunk : string Patch.hunk) (mode : rendering_mode) : Ui.t =
  let content =
    let blocks = Block.of_hunk hunk.Patch.lines in
    let word_diff_blocks = List.map WordDiff.compute blocks in
    let word_diff_lines = Block.to_hunk word_diff_blocks in
    render_hunk_lines word_diff_lines mode
  in
  Ui.vcat [ content ]

(* Helper functions for side-by-side view *)

let lines_with_numbers (lines : line list) (attr : Notty.attr) (prefix : string)
    (mode : rendering_mode) : Ui.t list =
  let rec process_lines line_num acc = function
    | [] -> List.rev acc
    | line :: rest ->
        let content, next_num =
          match line with
          | Common s -> (style_diff_line unchanged_marker s mode, line_num + 1)
          | Change s -> (style_diff_line prefix s mode, line_num + 1)
          | Empty -> (empty_line_content, line_num)
        in
        let line_ui =
          W.string ~attr (Printf.sprintf "%3d %s" line_num content)
        in
        process_lines next_num (line_ui :: acc) rest
  in
  process_lines 1 [] lines

let create_summary (start_line_num : int) (hunk_length : int)
    (attr : Notty.attr) (change_type : [ `Add | `Remove ]) : Ui.t =
  let sign =
    match change_type with `Add -> added_marker | `Remove -> removed_marker
  in
  let summary =
    Printf.sprintf "@@ %s%d,%d @@" sign start_line_num hunk_length
  in
  W.string ~attr summary

(* Main View Functions *)

let ui_unified_diff (hunk : string Patch.hunk) (mode : rendering_mode) : Ui.t =
  let hunk_summary = render_hunk_summary hunk mode in
  let hunk_content =
    let blocks = Block.of_hunk hunk.Patch.lines in
    let single_line_changes =
      List.for_all
        (function
          | Block.Changed { mine; their; _ } ->
              List.length mine = 1 && List.length their = 1
          | _ -> true)
        blocks
    in
    if single_line_changes then
      let word_diff_blocks = List.map WordDiff.compute blocks in
      let word_diff_lines = Block.to_hunk word_diff_blocks in
      render_hunk_lines word_diff_lines mode
    else render_hunk hunk mode
  in
  Ui.vcat [ hunk_summary; hunk_content ]

let current_hunks (z_patches : string Patch.t Zipper.t) (mode : rendering_mode)
    : Ui.t =
  let p = Zipper.get_focus z_patches in
  let hunks = List.map (fun hunk -> ui_unified_diff hunk mode) p.Patch.hunks in
  Ui.vcat hunks

let current_hunks_side_by_side (z_patches : string Patch.t Zipper.t)
    (mode : rendering_mode) : Ui.t =
  let p = Zipper.get_focus z_patches in
  let render_side_by_side hunk =
    let mine_lines, their_lines = split_and_align_hunk hunk.Patch.lines in
    let render_lines lines prefix attr =
      lines_with_numbers lines attr prefix mode
    in
    let content_mine =
      render_lines mine_lines removed_marker Notty.A.(fg red)
    in
    let content_their =
      render_lines their_lines added_marker Notty.A.(fg green)
    in
    let summary_mine =
      create_summary
        (hunk.Patch.mine_start + 1)
        hunk.Patch.mine_len
        Notty.A.(fg red)
        `Remove
    in
    let summary_their =
      create_summary
        (hunk.Patch.their_start + 1)
        hunk.Patch.their_len
        Notty.A.(fg green)
        `Add
    in
    Ui.hcat
      [
        Ui.resize ~w:0 ~sw:2 (Ui.vcat (summary_mine :: content_mine));
        Ui.space 1 0;
        Ui.resize ~w:0 ~sw:2 (Ui.vcat (summary_their :: content_their));
      ]
  in
  Ui.vcat (List.map render_side_by_side p.Patch.hunks)
