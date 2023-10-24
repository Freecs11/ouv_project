
open Random;;
(*
1 - Presentation 
Question 1   
*)
type int64list = { mutable l : int64 list; mutable size : int }
;;

(*
  Inserts an element at the end of the given list.
  @param x The element to insert.
  @param l The list to insert into.
*)
let insertEndlist (x : int64) (l : int64list) : unit =
  l.l <- l.l@[x];
  l.size <- l.size + 1
;;

(*
  Inserts an element at the start of the given list.
  @param x The element to insert.
  @param l The list to insert into.
*)
let insertStartlist (x : int64) (l : int64list) : unit =
  l.l <- x:: l.l;
  l.size <- l.size + 1
;;

(*
  Returns the head of the given list.
  @param l The list to get the head of.
  @return The head of the list.
  @raise Failure "Empty list" if the list is empty.
*)
let getHeadlist (l : int64list) : int64 =
  match l.l with
  | [] -> raise (Failure "Empty list")
  | h :: _ -> h
;;

(*
  Removes the head of the given list.
  @param l The list to remove the head from.
  @raise Failure "Empty list" if the list is empty.
*)
let removeHeadlist (l : int64list) : unit =
  match l.l with
  | [] -> raise (Failure "Empty list")
  | _ :: t -> l.l <- t; l.size <- l.size - 1
;;

(*
  Prints the elements of the given list to standard output.
  @param l The list to print.
*)
let rec printList (l : int64list) : unit =
  match l.l with
  | [] -> print_string " } \n"
  | h :: t ->
    print_string "  ";
    print_string (Int64.to_string h );
    printList { l = t; size = l.size - 1 }
  ;;

(*
  Constructs a list of 64-bit integers representing the powers of 2 from 0 to the given power.
  @return The list of 64-bit integers.
*)
let rec constructList (power:int) :int64list =
  let base = Int64.shift_left 1L 64 in
  let rec constructList' (n : int) (acc : int64list) =
    if n <= 64 then (
      insertEndlist (Int64.shift_left 1L n) acc;
      acc
    )
    else (
      let y = if n >= 64 then 0L else Int64.shift_left 1L n in
      let z = n - 64 in
      insertEndlist y acc;
      constructList' z acc
    )
  in
  constructList' power { l = []; size = 0 }
;;


  (* test *)
let l = constructList 100 in
printList l ;; 

let l = constructList 300 in
printList l ;;


(* helper function to convert an int64 to its binary representation *)
let rec int64toBoolList (x : int64) (aux : bool list) : bool list =
  if x = 0L then aux
  else int64toBoolList (Int64.div x 2L) ((Int64.rem x 2L = 1L) :: aux) (* int version :  intToBool (x/2) (x mod 2 = 1 :: aux) *)
;;

(*
Question 2   
approche :  Transform chaque entier en sa représentation binare et on l'inverse pour avoir le résultat à la fin *)
 let decomposition (x : int64list) : bool list =  
  let rec decompose ( x:int64 list ) (aux : bool list) :bool list = 
    match x with
    | [] -> aux
    | h::t -> decompose t (int64toBoolList h aux)
  in
  List.rev (decompose x.l [])
    ;;

(* test *)
let nb : int64list = { l = [38L]; size = 1 };;
let k = decomposition nb;;
print_string "\ndecomposition([38]) = \n";;
for i = 0 to (List.length k) - 1 do
  print_string (string_of_bool (List.nth k i)); print_string " ";
done;
print_string "\n";;


(* Question 3 *)

let completion (x:bool list)  (nb : int)  : bool list = 
  if nb < List.length x then
    let rec aux x nb auxx : bool list = 
      match x with
      | [] -> auxx
      | h::t -> if nb = 0 then auxx
        else aux (t) (nb-1) (h::auxx)
    in
    aux x nb []
  else 
    let  aux x nb  : bool list = 
       (* on ajoute false à la fin de la liste  , bit de poids faible *)
      (*list of nb fasle*)
      let boolj : bool list =  List.init nb (fun i -> false) in
      x@boolj
    in
    aux x (nb - List.length x) 
    ;;

(* test *)
let rec printboolList (l : bool list) : unit =
  match l with
  | [] -> print_string "\n"
  | h :: t ->
    print_string "  ";
    print_string (string_of_bool h);
    printboolList t
  ;;

let k = completion [false; true; true; false; false; true] 4;;
print_string "\ncompletion([false; true; true; false; false; true], 4) = \n";;
printboolList k;;

let k = completion [false; true; true; false; false; true] 8;;
print_string "\ncompletion([false; true; true; false; false; true], 8) = \n";;
printboolList k;;

(* Question 4 *)

(* approche : 1 + 2 * boolListToInt t  puisque on représente les bits de poids fort à gauche *)
let rec boolListToInt64 (x : bool list) : int64 = 
  match x with
  | [] -> Int64.zero
  | h::t -> if h then Int64.add 1L (Int64.mul 2L (boolListToInt64 t))
    else Int64.mul 2L (boolListToInt64 t)
    ;;

(* test *)
let k = boolListToInt64 [false; true; true; false; false; true];;
print_string "\nboolListToInt([false; true; true; false; false; true]) = \n";;
print_string (Int64.to_string k);;
print_string "\n";;

(* une petite fonction pour divisé une liste de 'a type à la case n en 2 listes*)
let rec split_list_at_n (l : 'a list) (n : int) : 'a list * 'a list =
  match l with
  | [] -> ([], [])
  | h :: t ->
    if n = 0 then ([], l)
    else
      let (l1, l2) = split_list_at_n t (n - 1) in
      (h :: l1, l2)
    ;;

let composition (l: bool list) : int64list =
  (* use boolListToInt64 *)
  let listof64 = ref (split_list_at_n l 64 )  in (* (list de 64 elements * reste de la liste donc il faut refaire split sur le reste également) *)
  let rec aux ( le : bool list * bool list) (auxi : int64list) : int64list = 
    match le with
    | ([],[]) -> auxi
    | (h,[]) ->  { l = auxi.l @ [boolListToInt64 h] ; size = auxi.size + 1 } 
    | (h, t) ->  let newlist = ref(split_list_at_n t 64) in (* on split le reste de la liste *)
      aux (!newlist) { l = auxi.l @ [boolListToInt64 h] ; size = auxi.size + 1 }
  in
  aux !listof64 { l = []; size = 0 }
;;

(* test *)
let k = composition [false; true; true; false; false; true];;
print_string "\ncomposition([false; true; true; false; false; true]) = \n";;
printList k;;




(* Question 5 *)

(* approche : on applique decomposition puis completion
    x: entier à transformer en bool list
    n: nombre de bits de la liste de sortie
*)
let table (x : int64list) (n : int) : bool list = 
  completion (decomposition  x) n
    ;;

(* test *)
let k = table { l=[38L] ; size=1}  8;;
print_string "\ntable 38 8 = \n";;
printboolList k;;

(* Question 6 *)
(*
  n = nombre de bits de l'eniter à générer
 *)
let genAlea (n : int) : int64list =
  let rec aux (n : int) (l : int64list) =
    if n <= 0 then l
    else
      let max_64 = Int64.shift_left 1L 64 in (* 2^64 *)
      if l.size * 64 < n then (* si on a pas encore atteint n bits *)
        let rando = Random.int64 max_64 in (* on génère un entier aléatoire sur 64 bits *)
        aux (n - 64) { l =  l.l @ [rando]; size = l.size + 1 } (* on ajoute cet entier à la liste et on continue *)
      else (* on a atteint n bits *)
        let bits_to_generate = n mod 64 in (* on calcule le nombre de bits à générer *)
        let rando = Random.int64 (Int64.shift_left 1L bits_to_generate) in (* on génère un entier aléatoire sur bits_to_generate bits *)
        aux (n - bits_to_generate) { l = l.l @ [rando]; size = l.size + 1 } (* on ajoute cet entier à la liste et on continue *)
  in 
  aux n { l = []; size = 0 }



(* test *)
let k = genAlea 100;;
print_string "\ngenAlea 100 = \n";;
printList k;;



let f2 = constructList 164;;
print_string "constructList 164 = \n";;
printList f2;;
print_string "\n";;



let f2 = constructList 300;;
print_string "\nconstructList 300 = \n";;
printList f2;;
print_string "\n";;


(*new test*)
let k = composition  [false; false; false; false; false; false; false; false; false; false; false;
false; false; false; false; false; false; false; false; false; false; false;
false; false; false; false; false; false; false; false; false; false; false;
false; false; false; false; false; false; false; false; false; false; false;
false; false; false; false; false; false; false; false; false; false; false;
false; false; false; false; false; false; false; false; false; false; false;
false; false; false; false; false; false; false; false; false; false; false;
false; false; false; false; false; false; false; false; false; false; false;
false; false; false; false; false; false; false; false; false; false; false;
false; false; false; false; false; false; false; false; false; false; false;
false; false; false; false; false; false; false; false; false; false; false;
false; false; false; false; false; false; false; true; true; true; true;
true; true; true; true; true; true; true; true; true; true; true; true;
true; true; true; true; true; true; true; true; true; true; true; true;
true; true; true; true; true; true; true; true; true; true; true; true;
true; true; true; true; true; true; true; true; true; true; true; true;
true; true; true; true; true; true; true; true; true; true; true];;
print_string "\ncomposition([false; false; false; false; false; false; false; false; false; false; false;... = \n";;
print_string " { " ;;
printList k;;


let x = decomposition ( constructList (300) );;
let x = composition x;;
print_string "\ncomposition(decomposition(2^300)) = \n";;
print_string " { " ;;
printList x;;
print_string "\n";;
