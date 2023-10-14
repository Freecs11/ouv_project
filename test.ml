open Int64

type int64list = Empty | Cons of int64 * int64list

let rec insertInt64List x l = 
  match l with
  | Empty -> Cons(x, Empty)
  | Cons(h, t) -> Cons(h, insertInt64List x t)
;;

let rec removeInt64List l =
  match l with 
  | Empty -> failwith "error on remove"
  | Cons(h, t) -> t
;;

let rec headInt64List l =
  match l with 
  | Empty -> failwith "error on head"
  | Cons(h, t) -> h
;;

let rec printInt64List l =
  match l with
  | Empty -> ()
  | Cons(h, t) -> print_string (Int64.to_string h); print_string " "; printInt64List t
;;

(*construct ne marche pas *)
let rec constructInt64List x =
  let rec construct x acc =
    if x = 0L then acc
    else
      let c = Int64.shift_left 1L 64 in
      let k = Int64.rem x c in
      let l = Int64.div x c in
      construct l (insertInt64List k acc)
  in
  construct x Empty
;;

let x = Int64.shift_left 1L 100 in (* 2^100 *)
let lst = constructInt64List x in
printInt64List lst;
print_string "d\n";
