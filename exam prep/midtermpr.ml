





let nats_gen () : unit -> int = 
  let counter = ref ~-1 in 
  fun () -> incr counter;
  !counter ;;


No. it can mean different things so not pure programming


let square_gen : unit -> int =
  let counter = ref 0 in
  fun () -> incr counter
  !counter * !counter ;;


let rec stream_of_gen (gen: unit -> 'a) : 'a stream =
  lazy (Cons (gen (), stream_of_gen gen)) ;;


let gen_of_list (lst : 'a list) : unit -> 'a =
  let store = ref lst in
  fun () -> match !store with 
  | [] -> raise NoMore
  | fst :: tail -> 
    store := tail; 
  fst ;; 

let x = ref 3 in x := !x * 1

guh 

