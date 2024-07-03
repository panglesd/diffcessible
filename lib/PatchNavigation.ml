open Nottui
module W = Nottui_widgets

type direction = Prev | Next

let navigate (z_patches : Patch.t Zipper.t ref) (dir : direction) : unit =
  let z = !z_patches in
  match dir with
  | Prev -> z_patches := Zipper.prev z
  | Next -> z_patches := Zipper.next z
