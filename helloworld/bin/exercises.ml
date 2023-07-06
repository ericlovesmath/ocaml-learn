(** Return last element of list **)
let rec last = function
  | [] -> None
  | [ e ] -> Some e
  | _ :: rem -> last rem

(** Last Two Elements of a List *)
let rec last_two = function
  | [] | [ _ ] -> None
  | [ e1; e2 ] -> Some (e1, e2)
  | _ :: rem -> last_two rem

(** Find nth element of list, raise exception if OOB *)
let rec nth lst ind =
  match (lst, ind) with
  | [], _ -> failwith "nth"
  | e :: _, 0 -> e
  | _ :: rem, _ -> nth rem (ind - 1)

(** Length of list, with tail recursion *)
let length lst =
  let rec length_acc len = function
    | [] -> len
    | _ :: rem -> length_acc (len + 1) rem
  in
  length_acc 0 lst

(** Reverse List *)
let rev lst =
  let rec rev_acc acc = function
    | [] -> acc
    | e :: rem -> rev_acc (e :: acc) rem
  in
  rev_acc [] lst

(** Check if list is palindrome *)
let is_palindrome lst = lst = rev lst

(** Flatten a list *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten node =
  let rec flatten_aux acc = function
    | [] -> acc
    | One elem :: rem -> flatten_aux (elem :: acc) rem
    | Many lst :: rem -> flatten_aux (flatten_aux acc lst) rem
  in
  rev (flatten_aux [] node)

(** Remove consecutive duplicates in list *)
let compress lst =
  let rec compress_aux acc prev = function
    | [] -> acc
    | e :: rem -> compress_aux (if prev = e then acc else e :: acc) e rem
  in
  rev (compress_aux [] "" lst)

let compress lst =
  let rec compress_aux acc = function
    | e1 :: e2 :: rem ->
      compress_aux (if e1 = e2 then acc else e1 :: acc) (e2 :: rem)
    | [ e ] -> e :: acc
    | [] -> acc
  in
  rev (compress_aux [] lst)

(** Pack consecutive duplicates of list elements into sublists *)
let pack lst =
  let rec pack_aux acc curr = function
    | e1 :: (e2 :: _ as rem) ->
      if e1 = e2 then pack_aux acc (e1 :: curr) rem
      else pack_aux ((e1 :: curr) :: acc) [] rem
    | [ e ] -> (e :: curr) :: acc
    | [] -> curr :: acc
  in
  rev (pack_aux [] [] lst)

(** Run-Length Encoding *)
let encode lst =
  let rec encode_aux acc count = function
    | e1 :: (e2 :: _ as rem) ->
      if e1 = e2 then encode_aux acc (count + 1) rem
      else encode_aux ((count, e1) :: acc) 1 rem
    | [ e ] -> (count, e) :: acc
    | [] -> []
  in
  rev (encode_aux [] 1 lst)

(** Run-Length Encoding, Modified *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode lst =
  let rle_of_encoding e = function
    | 1 -> One e
    | count -> Many (count, e)
  in
  let rec encode_aux acc count = function
    | e1 :: (e2 :: _ as rem) ->
      if e1 = e2 then encode_aux acc (count + 1) rem
      else encode_aux (rle_of_encoding e1 count :: acc) 1 rem
    | [ e ] -> rle_of_encoding e count :: acc
    | [] -> []
  in
  rev (encode_aux [] 1 lst)

(** Decode a Run-Length Encoded List *)
let decode lst =
  let rec decode_aux acc = function
    | Many (count, e) :: rem ->
      if count = 1 then decode_aux (e :: acc) rem
      else decode_aux (e :: acc) (Many (count - 1, e) :: rem)
    | One e :: rem -> decode_aux (e :: acc) rem
    | [] -> acc
  in
  rev (decode_aux [] lst)

(** Duplicate the Elements of a List *)
let duplicate lst =
  let rec duplicate_aux acc = function
    | [] -> acc
    | e :: rem -> duplicate_aux (e :: e :: acc) rem
  in
  rev (duplicate_aux [] lst)

(** Replicate the Elements of a List a Given Number of Times *)
let replicate lst count =
  let rec add_n_times acc n e =
    if n = 0 then acc else add_n_times (e :: acc) (n - 1) e
  in
  let rec replicate_aux acc = function
    | e :: rem -> replicate_aux (add_n_times acc count e) rem
    | [] -> acc
  in
  replicate_aux [] (rev lst)

(** Drop Every N'th Element From a List *)
let drop lst count =
  let rec drop_aux acc n = function
    | e :: rem -> if n = count 
      then drop_aux acc 1 rem
      else drop_aux (e :: acc) (n + 1) rem
    | [] -> acc
  in
  rev (drop_aux [] 1 lst)

(** Split list into 2 parts *)
let split lst len = 
  let rec split_aux left right n = match (right, n) with
    | (_, 0) | ([], _) -> (rev left, right)
    | (e :: rem, _) -> split_aux (e :: left) (rem) (n - 1)
  in
  split_aux [] lst len

(** Extract a Slice From a List, Inclusve *)
(* let rec slice lst left right = match (lst, left, right) with *)
(*   | (_, 0) -> *)
