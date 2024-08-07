let navigate (dir : Types.navigation_direction) =
  match dir with Types.Prev -> Zipper.prev | Types.Next -> Zipper.next
