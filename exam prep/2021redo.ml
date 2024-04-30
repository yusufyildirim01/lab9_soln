(* Q2.1 *)


let nats_gen : unit -> int = 
  let x = ref 0 in
  fun () -> 
    let y = !x in 
    incr x; 
    y;;

(* 2.2 *)

(*
it cannot be implemented ina pure subset of OCaml because in pure subsets,
every function should evaluate to the same value when called. since nats_gen 
needs to change every call inherently, it is impossible for it to be pure.   
*)

(* 2.3 *)

let square_gen : unit -> int = 
  let x = ref 1 in 
  fun () -> 
    let y = !x * !x in
    incr x;
    y;;


    
  let rev (xs : 'a list) : 'a list =
    let xs = ref xs in
    let accum = ref [] in
    while !xs <> [] do
      accum := (List.hd !xs) :: !accum;
      xs := List.tl !xs
    done;
    !accum ;;

  let rec while_ (condition : unit -> bool) (body : unit -> unit) : unit = 
    if condition () 
      then (body (); 
          while_ condition body)
      else () 

      let rec while_2 (condition : unit -> bool) (body : unit -> unit) : unit = 
        while condition () do 
          body ()
        done;  

  (* let rev (xs : 'a list) : 'a list =
    let xs = ref xs in
    let accum = ref [] in
    let condition = !xs <> [] in 
    let body = (accum := (List.hd !xs) :: !accum;
                xs := List.tl !xs) in
    while_ (fun () -> condition) (fun () -> body); 
    !accum ;; *)



    type 'a stream_internal = Cons of 'a * 'a stream
    and 'a stream = 'a stream_internal Lazy.t ;;
   
   let head (s : 'a stream) : 'a =
     let Cons (hd, _tl) = Lazy.force s in hd ;;
   
   let tail (s : 'a stream) : 'a stream =
     let Cons (_hd, tl) = Lazy.force s in tl ;;
     
   let rec first (n : int) (s : 'a stream) : 'a list =
     if n = 0 then []
     else head s :: first (n - 1) (tail s) ;;
     
   let rec smap (f : 'a -> 'b)
                (s : 'a stream)
              : ('b stream) = 
     lazy (Cons (f (head s), smap f (tail s))) ;;
     
   let rec smap2 (f : 'a -> 'b -> 'c)
                 (s1 : 'a stream)
                 (s2 : 'b stream)
               : 'c stream = 
     lazy (Cons (f (head s1) (head s2), 
                 smap2 f (tail s1) (tail s2))) ;;
   
   let rec sfilter (pred : 'a -> bool) (s : 'a stream) : 'a stream =
       lazy (if pred (head s) then
               Cons((head s), sfilter pred (tail s))
             else Lazy.force (sfilter pred (tail s))) ;;



let rec stream_of (x : 'a) : 'a stream = 
  lazy (Cons (x, stream_of x))

  (* ! 4.2 *)
let rec every_nth (x : int) : int stream = 
  lazy Cons (x, stream_of idk?? )



(* Define a function update that updates the value stored in an element of an 
'a ref list at a particular index with a new value. Its type should be 
'a ref list -> int -> 'a -> unit. It should raise a Failure exception if the 
list is too short, and Invalid_argument if the index is is negative. 
You may find the function List.nth helpful.

# update listarr 1 43 ;;
- : unit = ()
# listarr ;;
- : int ref list = [{contents = 0}; {contents = 43}; {contents = 10}]
 *)

let update (lst : 'a ref list) (x : int) (v : 'a) : unit = 
  let value = List.nth lst x in 
  value := v ;;


  (* if x < 0 then raise (Invalid_argument "the index cannot be negative") 
  else 
    match lst with 
    | [] -> raise (Failure "the list is too short") 
    | hd :: tl ->  *)




    class type library_item_type = 
    object
      method get_title : string
      method get_id : string
      method get_details : string
      method print_details : unit 
    end ;;

    class type dvd_type = 
      object 
        inherit library_item_type
        method get_runtime : int 
      end ;;


  class dvd (title : string) (id : string) (runtime: int): dvd_type = 
    object
      inherit library_item title id
      method get_runtime : int = runtime 
      method! get_details : string = "DVD Title: " ^ title ^ ", ID: " ^ id ^ ", Runtime: " ^ string_of_int runtime ^ " minutes"
    end ;;

  # let alice = new library_item "Alice in Wonderland" "LC5432" ;;
val alice : library_item = <obj>
# alice#get_details ;;
- : string = "Title: Alice in Wonderland, ID: LC5432"




















(* This is given to you*)
let rev (xs : 'a list) : 'a list =
   let xs = ref xs in
   let accum = ref [] in
   while !xs <> [] do
     accum := (List.hd !xs) :: !accum;
     xs := List.tl !xs
   done;
   !accum ;;
  let list1 = [1;2;3];;

 (* Question 3 *)

let rec while_ (condition : unit -> bool) (body : unit -> unit) : unit =
 if (Lazy.force condition) then
   begin
   Lazy.force body();
   (while_ condition body)
   end
 else ();;

 let rev (xs : 'a list) : 'a list =
   let xs = ref xs in
   let accum = ref [] in
   let condition = ((!xs <> []) Lazy.t) in
   let body = ((accum := (List.hd !xs) :: !accum; xs := List.tl !xs) Lazy.t) in
   while_ condition body;
   !accum ;;

(* You are given this too *)
let arr = [|0; 5; 10|] ;;
 - :val arr : int array = [|0; 5; 10|]
arr.(1) <- 42 ;;
 - : unit = ()
arr ;;
 - : int array = [|0; 42; 10|]

(* My work *)

let listarr : int ref list =
 [ref 0; ref 5; ref 10];;

let rec update (lst : 'a ref list) (index : int) (new_mem : 'a) : unit =
 if index < 0 then raise (Invalid_argument "index is negative")
 else if List.length lst <= index then raise (Failure "list too small")
 else (List.nth lst index) := new_mem;;
