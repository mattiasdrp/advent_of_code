open Mdrp_lib

let part_1 file =
  (* Initialisation *)
  let ci = open_in file in
  let length = String.length (input_line ci) in
  let acc = List.init length (fun _ -> (0, 0)) in
  close_in ci;
  (* Computing *)
  let res =
    Parse.fold_lines
      (fun acc line ->
        List.mapi
          (fun i (zeros, ones) ->
            if line.[i] = '0' then (zeros + 1, ones) else (zeros, ones + 1))
          acc)
      acc file
  in
  let pp_sep _ppf () = () in
  let gamma =
    Format.asprintf "%a"
      Format.(
        pp_print_list ~pp_sep (fun ppf (zeros, ones) ->
            Format.fprintf ppf "%d" (if zeros > ones then 0 else 1)))
      res
    |> Int.Decimal.of_bin
  in
  (* Epsilon is 0xFFF... xor gamma that is also 2^(number of bits in gamma) - 1 - gamma
     2 ^ n is 1 lsl n *)
  let epsilon = (1 lsl length) - 1 - gamma in
  gamma * epsilon

type acc = {
  zeros : int;
  lzeros : string list;
  ones : int;
  lones : string list;
}

let empty = { zeros = 0; lzeros = []; ones = 0; lones = [] }

let rec loop cmp index = function
  | [ e ] -> Int.Decimal.of_bin e
  | l -> aux cmp (index + 1) empty l

and aux cmp index ({ zeros; lzeros; ones; lones } as acc) = function
  | hd :: tl ->
      let acc =
        if hd.[index] = '0' then
          { acc with zeros = zeros + 1; lzeros = hd :: lzeros }
        else { acc with ones = ones + 1; lones = hd :: lones }
      in
      aux cmp index acc tl
  | [] -> if cmp zeros ones then loop cmp index lzeros else loop cmp index lones

let part_2 file =
  let numbers = Parse.lines file in
  let oxygen = aux ( > ) 0 empty numbers in
  let co2 = aux ( <= ) 0 empty numbers in
  oxygen * co2

let run part file = match part with 1 -> part_1 file | _ -> part_2 file
