let prod list =
  let rec prod_aux ans = function
    | [] -> ans
    | e :: rem -> prod_aux (ans * e) rem
  in
  prod_aux 1 list

let concat list =
  let rec concat_aux str = function
    | [] -> str
    | e :: rem -> concat_aux (str ^ e) rem
  in
  concat_aux "" list

let prod list = List.fold_left ( * ) 1 list

let ( -- ) left right =
  let rec aux left right acc =
    if left > right then acc else aux left (right - 1) (right :: acc)
  in
  aux left right []

let sum_cube_odd n =
  1 -- n
  |> List.filter (fun x -> x mod 2 = 1)
  |> List.map (fun x -> x * x * x)
  |> List.fold_left ( + ) 0

let sum = List.fold_left ( + ) 0
