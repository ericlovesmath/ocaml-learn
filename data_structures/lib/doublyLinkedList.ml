type 'a t = { hd : 'a node ref; tl : 'a node ref; len : int }

and 'a node =
  | None
  | Some of { prev : 'a node ref; next : 'a node ref; value : 'a option }

let init () =
  let rec hd = Some { prev = ref tl; next = ref None; value = None }
  and tl = Some { prev = ref None; next = ref hd; value = None } in
  { hd = ref hd; tl = ref tl; len = 0 }

let push_hd e l =
  let new_hd = Some { prev = l.hd 
