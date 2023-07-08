open Core

module Freq = struct

  let char_code char =
    match char with
      | 'a' .. 'z' -> Char.to_int char - 97
      | 'A' .. 'Z' -> Char.to_int char - 39
      | _ -> failwith "Day03.char_code: Invalid char given"

  let add_char freq char = freq.(char_code char) <- true

  let string_code str =
    let freq = Array.create ~len:52 false in
    String.iter str ~f:(add_char freq);
    (* Array.iter freq ~f:(fun bool -> printf "%d" (if bool then 1 else 0)); printf "\n"; *)
    (* String.iter str ~f:(fun char -> printf "%c %d\n" char (char_code char)); *)
    freq

  let in_all freqs =
    let combine e1 e2 = Array.init 52 ~f:(fun i -> e1.(i) && e2.(i)) in
    let rec zip = function
      | e1 :: e2 :: tl -> zip ((combine e1 e2) :: tl)
      | [ e ] -> e
      | [] -> failwith "Day03.Freq: zip recieved empty list"
    in
    let rec true_id i arr = if arr.(i) then i + 1 else true_id (i + 1) arr in
    true_id 0 (zip freqs)
    (* let all_true = List.fold_left ~init:true ~f:( && ) in
    let ans = ref 0 in
    for i = 1 to 52 do
      if all_true @@ List.map freqs ~f:(fun freq -> freq.(i - 1))
        then ans := i
    done;
    !ans *)

end

let sum_str list = List.fold_left ~init:0 ~f:( + ) list |> string_of_int

let parse file = In_channel.read_all file |> String.split_lines

let part1 file =
  let ans str =
    let len = String.length str in
    let (left, right) =
      (String.slice str 0 (len / 2), String.slice str (len / 2) len)
    in
    Freq.in_all [Freq.string_code left; Freq.string_code right]
  in
  parse file
    |> List.map ~f:ans
    |> sum_str

let part2 file =
  parse file
    |> List.chunks_of ~length:3
    |> List.map ~f:(List.map ~f:Freq.string_code)
    |> List.map ~f:(Freq.in_all)
    |> sum_str
