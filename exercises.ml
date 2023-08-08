(** Completing OCaml's 99 Exercises [https://ocaml.org/problems] *)

let fails f =
  match f () with
  | exception _ -> true
  | _ -> false

module Problem01 = struct
  (** Return last element of [list] *)
  let rec last = function
    | [] -> None
    | [ e ] -> Some e
    | _ :: rem -> last rem

  let () =
    assert (last [ 1; 2; 3; 4 ] = Some 4);
    assert (last [] = None)
end

module Problem02 = struct
  (** Return last two elements of [list] *)
  let rec last_two = function
    | [] | [ _ ] -> None
    | [ e1; e2 ] -> Some (e1, e2)
    | _ :: tl -> last_two tl

  let () =
    assert (last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d"));
    assert (last_two [ "a" ] = None);
    assert (last_two [] = None)
end

module Problem03 = struct
  (** Reimplement [List.nth] *)
  let rec nth lst i =
    match (lst, i) with
    | hd :: _, 0 -> hd
    | _ :: tl, i -> nth tl (i - 1)
    | _, _ -> failwith "Index out of bounds"

  let () =
    let lst = [ "a"; "b"; "c"; "d"; "e" ] in
    assert (nth lst 0 = "a");
    assert (nth lst 2 = "c");
    assert (nth lst 4 = "e");
    assert (fails (fun () -> nth lst 5));
    assert (fails (fun () -> nth lst (-1)));
    assert (fails (fun () -> nth [] 0))
end

module Problem04 = struct
  (** Reimplement [List.length], tail recursively *)
  let length lst =
    let rec length' lst acc =
      match lst with
      | [] -> acc
      | _ :: rem -> length' rem (acc + 1)
    in
    length' lst 0

  let () =
    assert (length [ 1; 2; 3; 4 ] = 4);
    assert (length [] = 0)
end

module Problem05 = struct
  (** Reimplement [List.rev], tail recursively *)
  let rev lst =
    let rec rev' lst aux =
      match lst with
      | [] -> aux
      | hd :: tl -> rev' tl (hd :: aux)
    in
    rev' lst []

  let () =
    assert (rev [ 1; 2; 3 ] = [ 3; 2; 1 ]);
    assert (rev [ 1; 2; 3; 4 ] = [ 4; 3; 2; 1 ]);
    assert (rev [] = []);
    assert (rev [ 1 ] = [ 1 ])
end

module Problem06 = struct
  (** Determine if [lst] is a palindrome *)
  let is_palindrome lst = lst = Problem05.rev lst

  let () =
    assert (is_palindrome [ 1; 2; 3; 2; 1 ]);
    assert (is_palindrome [ 1; 2; 2; 1 ]);
    assert (is_palindrome [ 1; 2; 1 ]);
    assert (is_palindrome [ 1; 1 ]);
    assert (is_palindrome [ 1 ]);
    assert (is_palindrome []);
    assert (is_palindrome [ 1; 2; 3; 4; 5 ] |> not);
    assert (is_palindrome [ 1; 2 ] |> not)
end

module Problem07 = struct
  (** Defines nested list structure *)
  type 'a node = One of 'a | Many of 'a node list

  (** BAD Flatten a nested list structure *)
  let rec flatten = function
    | [] -> []
    | One e :: rem -> e :: flatten rem
    | Many e :: rem -> flatten e @ flatten rem

  (** Flatten a nested list structure *)
  let flatten lst =
    let rec flatten' aux = function
      | [] -> aux
      | One e :: rem -> flatten' (e :: aux) rem
      | Many es :: rem -> flatten' (flatten' aux es) rem
    in
    flatten' [] lst |> List.rev

  let () =
    assert (
      flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
      = [ "a"; "b"; "c"; "d"; "e" ])
end

module Problem08 = struct
  (** Eliminate consecutive duplicates of list elements *)
  let compress lst =
    let rec compress' acc = function
      | [] -> acc
      | [ e ] -> e :: acc
      | e1 :: (e2 :: _ as rem) ->
        compress' (if e1 = e2 then acc else e1 :: acc) rem
    in
    compress' [] lst |> List.rev

  let () =
    assert (
      compress
        [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
      = [ "a"; "b"; "c"; "a"; "d"; "e" ]);
    assert (compress [] = []);
    assert (compress [ "a" ] = [ "a" ]);
    assert (compress [ "a"; "a" ] = [ "a" ]);
    assert (compress [ "a"; "b" ] = [ "a"; "b" ])
end

module Problem09 = struct
  (** Pack Consecutive Duplicates *)
  let pack lst =
    let rec pack' subacc acc = function
      | [] -> []
      | [ e ] -> (e :: subacc) :: acc
      | e1 :: (e2 :: _ as rem) ->
        let subacc = e1 :: subacc in
        if e1 = e2 then pack' subacc acc rem else pack' [] (subacc :: acc) rem
    in
    pack' [] [] lst |> List.rev

  let () =
    assert (
      pack
        [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"
        ; "e" ]
      = [ [ "a"; "a"; "a"; "a" ]; [ "b" ]; [ "c"; "c" ]; [ "a"; "a" ]
        ; [ "d"; "d" ]; [ "e"; "e"; "e"; "e" ] ]);
    assert (pack [ "a" ] = [ [ "a" ] ]);
    assert (pack [] = [])
end

module Problem10 = struct
  (** Implement Run-length Encoding *)
  let encode lst =
    let rec encode' cnt acc = function
      | [] -> []
      | [ e ] -> (cnt + 1, e) :: acc
      | e1 :: (e2 :: _ as rem) ->
        if e1 = e2
        then encode' (cnt + 1) acc rem
        else encode' 0 ((cnt + 1, e1) :: acc) rem
    in
    encode' 0 [] lst |> List.rev

  let () =
    assert (
      encode
        [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
      = [ (4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e") ]);
    assert (encode [ "a" ] = [ (1, "a") ]);
    assert (encode [] = [])
end

module Problem11 = struct
  (** Duplicate each element of a List *)
  let duplicate lst =
    let rec duplicate' acc = function
      | [] -> acc
      | e :: rem -> duplicate' (e :: e :: acc) rem
    in
    duplicate' [] (List.rev lst)

  let () =
    assert (
      duplicate [ "a"; "b"; "c"; "c"; "d" ]
      = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]);
    assert (duplicate [] = []);
    assert (duplicate [ 1 ] = [ 1; 1 ])
end

module Problem12 = struct
  (** Replicate each element of a List given a number of times *)
  let replicate lst count =
    let rec add_n n lst e = if n = 0 then lst else add_n (n - 1) (e :: lst) e in
    if count < 0
    then failwith "Problem12.replicate received negative count"
    else List.fold_left (add_n count) [] (List.rev lst)

  let () =
    assert (
      replicate [ "a"; "b"; "c"; "c"; "d" ] 2
      = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]);
    assert (
      replicate [ "a"; "b"; "c" ] 3
      = [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]);
    assert (replicate [ "a"; "b"; "c" ] 0 = []);
    assert (replicate [] 5 = []);
    assert (replicate [ "a" ] 1 = [ "a" ]);
    assert (fails (fun () -> replicate [ "a"; "b" ] (-1)))
end

module Problem13 = struct
  (** Drop every nth element of list *)
  let drop lst n =
    let rec drop' acc lst count =
      match (count, lst) with
      | _, [] -> acc
      | 1, _ :: rem -> drop' acc rem n
      | count, e :: rem -> drop' (e :: acc) rem (count - 1)
    in
    if n <= 0
    then failwith "Problem13.drop received non-positive n"
    else List.rev (drop' [] lst n)

  let () =
    assert (
      drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
      = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]);
    assert (drop [ "a"; "b"; "c" ] 3 = [ "a"; "b" ]);
    assert (drop [ "a"; "b"; "c" ] 1 = []);
    assert (drop [] 5 = []);
    assert (fails (fun () -> drop [ "a"; "b" ] 0));
    assert (fails (fun () -> drop [ "a"; "b" ] (-1)))
end
