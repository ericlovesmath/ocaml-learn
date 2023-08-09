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

module Problem14 = struct
  (** Split a list into two parts; the length of the first part is given. If the
      length of the first part is longer than the entire list, then the first
      part is the list and the second part is empty. *)
  let split lst len =
    let rec split' lst acc len =
      match (len, lst) with
      | 0, _ | _, [] -> (List.rev acc, lst)
      | len, e :: rem -> split' rem (e :: acc) (len - 1)
    in
    if len < 0
    then failwith "Problem14.split received negative len"
    else split' lst [] len

  let () =
    assert (
      split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
      = ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ]));
    assert (split [ "a"; "b"; "c"; "d" ] 5 = ([ "a"; "b"; "c"; "d" ], []));
    assert (split [ "a"; "b"; "c" ] 0 = ([], [ "a"; "b"; "c" ]));
    assert (split [] 0 = ([], []));
    assert (split [] 10 = ([], []));
    assert (fails (fun () -> split [ "a"; "b" ] (-1)))
end

module Problem15 = struct
  (** Extract a Slice From a List, inclusive. Fails if not in bounds*)
  let slice lst left right =
    let rec drop lst len =
      match (lst, len) with
      | _, 0 -> lst
      | [], _ -> failwith "Problem15.slice: Left bound longer than list"
      | _ :: rem, _ -> drop rem (len - 1)
    in
    let keep lst len =
      let rec keep' lst acc len =
        match (lst, len) with
        | _, 0 -> List.rev acc
        | [], _ -> failwith "Problem15.slice: Right bound longer than list"
        | e :: rem, _ -> keep' rem (e :: acc) (len - 1)
      in
      keep' lst [] len
    in
    match (left, right) with
    | l, _ when l < 0 -> failwith "Problem15.slice: Negative left bound"
    | l, r when l > r -> failwith "Problem15.slice: Left bound > Right bound"
    | l, r -> keep (drop lst left) (right - left + 1)

  let () =
    assert (
      slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6
      = [ "c"; "d"; "e"; "f"; "g" ]);
    assert (slice [ "a"; "b"; "c"; "d" ] 0 3 = [ "a"; "b"; "c"; "d" ]);
    assert (slice [ "a"; "b"; "c"; "d" ] 1 1 = [ "b" ]);
    assert (fails (fun () -> slice [] 0 0 = []));
    assert (fails (fun () -> slice [ "a"; "b"; "c"; "d" ] 1 6));
    assert (fails (fun () -> slice [ "a"; "b"; "c"; "d" ] (-1) 3));
    assert (fails (fun () -> slice [ "a"; "b"; "c"; "d" ] 2 1));
    assert (fails (fun () -> slice [] (-1) 0));
    assert (fails (fun () -> slice [] 0 1))
end

module Problem16 = struct
  (** Rotate a list N places to the left. *)

  let rotate lst n =
    match lst with
    | [] -> []
    | lst ->
      let left, right = Problem14.split lst (n mod List.length lst) in
      right @ left

  let () =
    assert (
      rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3
      = [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]);
    assert (
      rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 8
      = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]);
    assert (rotate [ "a"; "b"; "c" ] 0 = [ "a"; "b"; "c" ]);
    assert (rotate [ "a"; "b"; "c" ] 5 = [ "c"; "a"; "b" ]);
    assert (rotate [ "a" ] 5 = [ "a" ]);
    assert (rotate [] 5 = [])
end

module Problem17 = struct
  (** Remove the K'th element from a list, error if out of bounds *)
  let remove_at k lst =
    let fail_index_oob () =
      failwith "Problem17.remove_at: Index out of bounds"
    in
    let rec remove_at' acc k lst =
      match (k, lst) with
      | 0, _ :: rem -> List.rev acc @ rem
      | _, [] -> fail_index_oob ()
      | k, e :: rem -> remove_at' (e :: acc) (k - 1) rem
    in
    if k < 0 then fail_index_oob () else remove_at' [] k lst

  let () =
    assert (remove_at 0 [ "a"; "b"; "c"; "d" ] = [ "b"; "c"; "d" ]);
    assert (remove_at 1 [ "a"; "b"; "c"; "d" ] = [ "a"; "c"; "d" ]);
    assert (remove_at 2 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "d" ]);
    assert (remove_at 3 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "c" ]);
    assert (fails (fun () -> remove_at 4 [ "a"; "b"; "c"; "d" ]));
    assert (fails (fun () -> remove_at (-1) [ "a"; "b"; "c"; "d" ]));
    assert (remove_at 0 [ "a" ] = []);
    assert (fails (fun () -> remove_at 0 []))
end

module Problem18 = struct
  (** Insert element [e] at index [k]. If [k > List.length], insert at the end.
      If [k < 0], insert at beginning. *)
  let insert_at e k lst =
    let split k lst =
      let rec split' acc k lst =
        match (k, lst) with
        | _, [] -> (acc, lst)
        | k, _ when k <= 0 -> (acc, lst)
        | k, e :: rem -> split' (e :: acc) (k - 1) rem
      in
      split' [] k lst
    in
    let rec return lst = function
      | [] -> lst
      | e :: rem -> return (e :: lst) rem
    in
    let left, right = split k lst in
    return (e :: right) left

  let () =
    assert (insert_at "x" 0 [ "a"; "b"; "c"; "d" ] = [ "x"; "a"; "b"; "c"; "d" ]);
    assert (insert_at "x" 1 [ "a"; "b"; "c"; "d" ] = [ "a"; "x"; "b"; "c"; "d" ]);
    assert (insert_at "x" 2 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "x"; "c"; "d" ]);
    assert (insert_at "x" 3 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "c"; "x"; "d" ]);
    assert (insert_at "x" 4 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "c"; "d"; "x" ]);
    assert (insert_at "x" 5 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "c"; "d"; "x" ]);
    assert (insert_at "x" 10 [ "a"; "b"; "c" ] = [ "a"; "b"; "c"; "x" ]);
    assert (insert_at "x" (-1) [ "a"; "b"; "c" ] = [ "x"; "a"; "b"; "c" ]);
    assert (insert_at "a" 0 [] = [ "a" ])
end

module Problem19 = struct
  (** Create a List Containing All Integers Within a Given Range, Inclusive *)

  let range l r =
    let rec range' acc curr =
      if curr >= l then range' (curr :: acc) (curr - 1) else acc
    in
    range' [] r

  let () =
    assert (range 4 9 = [ 4; 5; 6; 7; 8; 9 ]);
    assert (range 0 2 = [ 0; 1; 2 ]);
    assert (range (-2) 2 = [ -2; -1; 0; 1; 2 ]);
    assert (range 2 2 = [ 2 ]);
    assert (range 5 2 = [])
end

module Problem20 = struct
  (** Get [n] distinct random elements of list using [Random.int] *)
  let rand_select lst count =
    let rec get_i acc i lst =
      match (i, lst) with
      | _, [] -> failwith "Problem20.rand_select: Unreachable"
      | 0, e :: rem -> (e, acc @ rem)
      | i, e :: rem -> get_i (e :: acc) (i - 1) rem
    in
    let rec rand_select' acc lst len = function
      | 0 -> acc
      | count ->
        let e, rem = get_i [] (Random.int len) lst in
        rand_select' (e :: acc) rem (len - 1) (count - 1)
    in
    let len = List.length lst in
    if count > len
    then failwith "Problem20.rand_select: Count > Length of List"
    else rand_select' [] lst (List.length lst) count

  let () =
    Random.init 5;
    assert (
      rand_select [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3
      = [ "c"; "f"; "h" ]);
    assert (fails (fun () -> rand_select [ "a"; "b"; "c" ] 6))
end

module Problem21 = struct
  (** Lotto: Draw [n] Different Random Numbers From the Set 1..[m] as a list *)
  let lotto_select n m = Problem20.rand_select (Problem19.range 1 m) n

  let () =
    Random.init 5;
    assert (lotto_select 6 49 = [ 35; 41; 32; 17; 2; 6 ]);
    assert (lotto_select 0 49 = []);
    assert (fails (fun () -> lotto_select 5 3));
    assert (fails (fun () -> lotto_select 1 0))
end

module Problem22 = struct
  (** Generate a Random Permutation of the Elements of a List *)
  let permutation lst = Problem20.rand_select lst (List.length lst)

  let () =
    Random.init 14;
    assert (
      permutation [ "a"; "b"; "c"; "d"; "e"; "f" ]
      = [ "d"; "c"; "e"; "f"; "b"; "a" ]);
    assert (permutation [] = []);
    assert (permutation [ "a" ] = [ "a" ])
end

module Problem23 = struct
  (** Generate the Combinations of [k] Distinct Objects Chosen From a List *)
  let extract k lst =
    let combos = ref [] in
    let rec extract' k combo rem =
      match (k, rem) with
      | 0, _ -> combos := combo :: !combos
      | k, _ -> extract_iter k combo rem
    and extract_iter k combo = function
      | [] -> ()
      | e :: rem ->
        extract' (k - 1) (e :: combo) rem;
        extract_iter k combo rem
    in
    extract' k [] (List.rev lst);
    !combos

  let () =
    assert (
      extract 2 [ "a"; "b"; "c"; "d" ]
      = [ [ "a"; "b" ]; [ "a"; "c" ]; [ "b"; "c" ]; [ "a"; "d" ]; [ "b"; "d" ]
        ; [ "c"; "d" ] ]);
    assert (extract 3 [ 1; 2; 3 ] = [ [ 1; 2; 3 ] ]);
    assert (extract 2 [ 1; 2; 3 ] = [ [ 1; 2 ]; [ 1; 3 ]; [ 2; 3 ] ]);
    assert (extract 1 [ 1; 2; 3 ] = [ [ 1 ]; [ 2 ]; [ 3 ] ]);
    assert (extract 0 [ 1; 2; 3 ] = [ [] ])
end
