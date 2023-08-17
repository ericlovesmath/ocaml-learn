type 'a t = { contents : 'a list; len : int }

exception Empty

let init () = { contents = []; len = 0 }
let push e s = { contents = e :: s.contents; len = s.len + 1 }

let pop s =
  match s.contents with
  | [] -> raise Empty
  | _ :: rem -> { contents = rem; len = s.len - 1 }

let peek s =
  match s.contents with
  | [] -> raise Empty
  | e :: _ -> e

let is_empty s = s.len = 0
let length s = s.len

let to_string string_of_e s =
  "[" ^ String.concat " " (List.map string_of_e s.contents) ^ "]"

let pp string_of_e s = print_string (to_string string_of_e s)
