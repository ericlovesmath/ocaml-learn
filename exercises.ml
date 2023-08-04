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
