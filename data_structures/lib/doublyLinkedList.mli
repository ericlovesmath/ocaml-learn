(** Type of [DoublyLinkedList] containing elements of type ['a]. *)
type 'a t

(** Raised when [DoublyLinkedList] is empty *)
exception Empty

val push_hd : 'a -> 'a t -> 'a t
val push_tl : 'a -> 'a t -> 'a t
val pop_hd : 'a t -> 'a t
val pop_tl : 'a t -> 'a t
val peek_hd : 'a t -> 'a
val peek_tl : 'a t -> 'a
val is_empty : 'a t -> bool
val length : 'a t -> int
val to_string : ('a -> string) -> 'a t -> string
val pp : ('a -> string) -> 'a t -> unit

(* Insertion/Deletion of a node at a given position *)
