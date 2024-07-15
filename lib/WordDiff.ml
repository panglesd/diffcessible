let lcs (xs : 'a array) (ys : 'a array) : 'a list =
  let m = Array.length xs in
  let n = Array.length ys in
  let dp = Array.make_matrix (m + 1) (n + 1) 0 in

  for i = 1 to m do
    for j = 1 to n do
      if xs.(i - 1) = ys.(j - 1) then dp.(i).(j) <- dp.(i - 1).(j - 1) + 1
      else dp.(i).(j) <- max dp.(i - 1).(j) dp.(i).(j - 1)
    done
  done;

  let rec backtrack i j acc =
    if i = 0 || j = 0 then acc
    else if xs.(i - 1) = ys.(j - 1) then
      backtrack (i - 1) (j - 1) (xs.(i - 1) :: acc)
    else if dp.(i - 1).(j) > dp.(i).(j - 1) then backtrack (i - 1) j acc
    else backtrack i (j - 1) acc
  in
  backtrack m n []

let () =
  let xs = [| "A"; "B"; "C"; "D"; "E"; "F" |] in
  let ys = [| "A"; "B"; "C" |] in
  let lcs_result = lcs xs ys in
  List.iter print_endline lcs_result
