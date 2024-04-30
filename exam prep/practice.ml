
### Lazy Module and Recursion to Emulate a `while` Loop

let rec while_ (condition : unit -> bool) (body : unit -> unit) : unit =
  if condition () then
    (body (); while_ condition body)
  else ()


### Reimplementation of List Reverse Using the Custom `while_` Function

let rev (xs : 'a list) : 'a list =
  let xs = ref xs in
  let accum = ref [] in
  let condition = fun () -> !xs <> [] in
  let body = fun () -> accum := (List.hd !xs) :: !accum; xs := List.tl !xs in
  while_ condition body;
  !accum


### Working with Arrays and Lists of Mutable References

let arr = [|0; 5; 10|] ;;
arr.(1) <- 42 ;;
arr ;;

let listarr : int ref list = [ref 0; ref 5; ref 10];;

let update (lst : 'a ref list) (index : int) (new_mem : 'a) : unit =
  if index < 0 then
    raise (Invalid_argument "index is negative")
  else if List.length lst <= index then
    raise (Failure "list too small")
  else
    (List.nth lst index) := new_mem;;


### Object-Oriented Programming: Classes and Inheritance

class type library_item_type =
  object
    method get_title : string
    method get_id : string
    method get_details : string
    method print_details : unit
  end ;;

class dvd (title : string) (id : string) (runtime: int) =
  object
    inherit library_item_type as super
    method get_runtime : int = runtime
    method get_details : string = "DVD Title: " ^ title ^ ", ID: " ^ id ^ ", Runtime: " ^ string_of_int runtime ^ " minutes"
  end ;;


### Lazy Evaluation and Streams

type 'a stream_internal = Cons of 'a * 'a stream
and 'a stream = 'a stream_internal Lazy.t ;;

let head (s : 'a stream) : 'a =
  let Cons (hd, _tl) = Lazy.force s in hd ;;

let tail (s : 'a stream) : 'a stream =
  let Cons (_hd, tl) = Lazy.force s in tl ;;

let rec smap (f : 'a -> 'b) (s : 'a stream) : 'b stream =
  lazy (Cons (f (head s), smap f (tail s))) ;;

