module WordDiff = Simple_diff.Make (String)

let string_to_words s = Array.of_list (String.split_on_char ' ' s)
let words_to_string words = String.concat " " (Array.to_list words)

let diff_words s1 s2 =
  let words1 = string_to_words s1 in
  let words2 = string_to_words s2 in
  WordDiff.get_diff words1 words2

type word_diff =
  | WDeleted of string array
  | WAdded of string array
  | WEqual of string array

let apply_word_diff s1 s2 =
  let diff = diff_words s1 s2 in
  List.map
    (function
      | WordDiff.Deleted words -> WDeleted words
      | WordDiff.Added words -> WAdded words
      | WordDiff.Equal words -> WEqual words)
    diff
