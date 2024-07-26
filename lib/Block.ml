(*
Hunk:
+A
-B
+C
-D

Interpretation: We start taking from whichever sign we see first in the diff. 

Block:
[
  Note: Careful when constructing back the block to a hunk. Take note of the order. .
  Changed {mine = [A]; their = [B]};  
  Changed {mine = [C]; their = [D]};
]


*)

(* type order = Mine | Their *)
(**)
(* type 'a t = *)
(*   | Common of 'a *)
(*   | Changed of { mine : 'a list; their : 'a list; order : order } *)
(**)
(* let of_hunk (hunk : 'a Patch.hunk) : 'a t list = *)
(*   let rec aux lines acc current_mine current_their = *)
(*     match lines with *)
(*     | [] -> *)
(*         if current_mine <> [] || current_their <> [] then *)
(*           List.rev *)
(*             (Changed *)
(*                { *)
(*                  mine = List.rev current_mine; *)
(*                  their = List.rev current_their; *)
(*                  order = (if current_mine <> [] then Mine else Their); *)
(*                } *)
(*             :: acc) *)
(*         else List.rev acc *)
(*     | `Common line :: rest -> *)
(*         let acc' = *)
(*           if current_mine <> [] || current_their <> [] then *)
(*             Changed *)
(*               { *)
(*                 mine = List.rev current_mine; *)
(*                 their = List.rev current_their; *)
(*                 order = (if current_mine <> [] then Mine else Their); *)
(*               } *)
(*             :: acc *)
(*           else acc *)
(*         in *)
(*         aux rest (Common line :: acc') [] [] *)
(*     | `Mine line :: rest -> aux rest acc (line :: current_mine) current_their *)
(*     | `Their line :: rest -> aux rest acc current_mine (line :: current_their) *)
(*   in *)
(*   aux hunk.lines [] [] [] *)
