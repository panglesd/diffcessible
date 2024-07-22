module W = Nottui_widgets

type direction = Prev | Next

let navigate (dir : direction) =
  match dir with Prev -> Zipper.prev | Next -> Zipper.next
