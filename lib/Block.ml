type 'a t = Common of 'a | Changed of { mine : 'a list; their : 'a list }

let of_hunk _hunk = failwith "TODO"
let to_hunk _blocks = failwith "TODO"
