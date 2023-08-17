(** Type of [Stack] containing elements of type ['a]. *)
type 'a t

(** Raised when [Stack] is empty on [Stack.pop] or [Stack.peek]. *)
exception Empty

(** Return a new [Stack], initially empty. *)
val init : unit -> 'a t

(** [push e s] adds the element [e] at the top of stack [s]. *)
val push : 'a -> 'a t -> 'a t

(** [pop s] removes the topmost element of stack [s] and returns the rest, or
    raises [Empty] if the stack is empty. *)
val pop : 'a t -> 'a t

(** [peek s] returns the topmost element in stack [s], or raises [Empty] if the
    stack is empty. *)
val peek : 'a t -> 'a

(** Return [true] if the given stack is empty, [false] otherwise. *)
val is_empty : 'a t -> bool

(** Return the number of elements in a stack. Time complexity O(1). *)
val length : 'a t -> int

(** [to_string string_of_e s] converts the given stack [s] to a string.
    [string_of_e] is a function that converts each element of [s] to a string. *)
val to_string : ('a -> string) -> 'a t -> string

(** [pp string_of_e s] Pretty prints the given stack [s]. [string_of_e] is a
    function that converts each element of [s] to a string to be printed. *)
val pp : ('a -> string) -> 'a t -> unit
