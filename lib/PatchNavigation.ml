module W = Nottui_widgets

type direction = Prev | Next

let navigate (dir : direction) : Patch.t Zipper.t -> Patch.t Zipper.t =
  match dir with Prev -> Zipper.prev | Next -> Zipper.next
