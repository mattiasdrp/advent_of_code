open Mdrp_lib

let number_of_digits line =
  let rec find op i line =
    match line.[i] with
    | '0' .. '9' as i -> Char.to_digit i
    | _ -> find op (op i) line
  in
  (10 * find (fun x -> x + 1) 0 line)
  + find (fun x -> x - 1) (String.length line - 1) line

let part_1 file =
  Parse.fold_lines (fun acc line -> acc + number_of_digits line) 0 file

let to_number = function
  | "one" -> 1
  | "two" -> 2
  | "three" -> 3
  | "four" -> 4
  | "five" -> 5
  | "six" -> 6
  | "seven" -> 7
  | "eight" -> 8
  | "nine" -> 9
  | s -> Char.to_digit s.[0]

let number_of_digits_and_words line =
  let r =
    Str.regexp {|[1-9]\|one\|two\|three\|four\|five\|six\|seven\|eight\|nine|}
  in
  let _ = Str.search_forward r line 0 in
  let first = Str.matched_string line in
  let _ = Str.search_backward r line (String.length line - 1) in
  let last = Str.matched_string line in
  (10 * to_number first) + to_number last

let part_2 file =
  Parse.fold_lines
    (fun acc line -> acc + number_of_digits_and_words line)
    0 file

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
