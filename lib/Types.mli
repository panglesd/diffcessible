type rendering_mode = Color | TextMarkers
type block_origin = Mine | Their | None
type navigation_direction = Prev | Next
type word = Unchanged of string | Changed of string
type line_content = word list
