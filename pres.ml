#use "topfind";;
#require "num";;
open Num;;
open Big_int;;
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
  l.l <- x :: l.l;
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
  | [] -> print_string "\n"
  | h :: t ->
    print_string "  ";
    print_string (Int64.to_string h);
    printList { l = t; size = l.size - 1 }
  ;;

(*
  Constructs a list of 64-bit integers from the given big_int.
  uses the big_int library to do the conversions and calculations ( remainder (mod), division (div)))
  https://v2.ocaml.org/releases/4.04/htmlman/libref/Big_int.html
  @param x The big_int to construct the list from.
  @return The list of 64-bit integers.
*)
let rec constructList (x : big_int) : int64list =
  if eq_big_int x zero_big_int then { l = []; size = 0 }
  else
    (* on récupère les 64 bits de poids faible *)
    let y = int64_of_big_int (mod_big_int x (power_int_positive_int 2 64)) in 
    (* on récupère les 64 bits de poids fort *)
    let z = div_big_int x (power_int_positive_int 2 64) in
    let l = constructList z in
    insertEndlist y l;
    l
  ;;


(* test *)
let test = power_int_positive_int 2 100 in
let l = constructList test in
printList l



(*
Question 2   
approche :  Transform chaque entier en sa représentation binare et on l'inverse pour avoir le résultat à la fin *)
 let decomposition (x : int list) : bool list =  
  let rec decompose ( x:int list ) (aux : bool list) :bool list = 
    match x with
    | [] -> aux
    | h::t -> 
      let rec aux2 (x:int) (aux : bool list) : bool list = 
        if x = 0 then aux 
        else aux2 (x/2) ((x mod 2 = 1)::aux)  (* on ajoute le bit de poids faible à la liste *)
      in
      decompose t (aux2 h aux) (* on ajoute la représentation binaire de h à la liste et appel récurssivement sur le reste  *)
  in
  List.rev (decompose x [])
    ;;

(* test *)
let k = decomposition([38;38]);;
print_string "\ndecomposition([38]) = \n";;
for i = 0 to (List.length k) - 1 do
  print_string (string_of_bool (List.nth k i)); print_string " ";
done;
print_string "\n";;


(* Question 3 *)

let completion x  nb  : bool list = 
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
let rec boolListToInt (x : bool list) : int = 
  match x with
  | [] -> 0
  | h::t -> if h then 1 + 2 * boolListToInt t
    else 0 + 2 * boolListToInt t
    ;;

(* test *)
let k = boolListToInt [false; true; true; false; false; true];;
print_string "\nboolListToInt([false; true; true; false; false; true]) = \n";;
print_int k;;
print_string "\n";;

(* Question 5 *)

(* approche : on applique decomposition puis completion *)
let table (x : int) (n : int) : bool list = 
  completion (decomposition [x]) n
    ;;

(* test *)
let k = table 38 8;;
print_string "\ntable 38 8 = \n";;
printboolList k;;

(* Question 6 *)

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

(* big int from our int64list structure  *)
let bigNumFromList ( l : int64list) : big_int = 
  let rec aux (l : int64list) (auxi : big_int) : big_int = 
    match l.l with
    | [] -> auxi
    | h::t ->  aux { l = t; size = l.size - 1 } (add_big_int (mult_int_big_int (Int64.to_int h) (power_int_positive_int 2 64)) auxi)
  in
  aux l zero_big_int
    ;;

(* test *)
let k = constructList ( power_int_positive_int 2 100 );;
let k = bigNumFromList k;;
print_string "\nbigNumFromList ( constructInt64List ( power_int_positive_int 2 100 ) ) = \n";;
print_string (string_of_big_int k);;
print_string "\n";;

(* Section 2 : Arbre de décision*)

(* Définir une structure de données permettant d’encoder des arbres binaires de décision.
Il s’agit d’une structure de données arborescente dont les nœuds internes contiennent un 
entier égal à sa profondeur;
chaque nœud interne possède deux enfants qui sont des arbres de décision et les feuilles
contiennent un booléen, true ou false. *)

type decisionTree = 
  | Leaf of bool
  | Node of int * decisionTree * decisionTree
;;

(* une petite fonction pour divisé une liste de 'a type à la case n en 2 listes*)
let rec split_list_at_n (l : 'a list) (n : int) : 'a list * 'a list =
  match l with
  | [] -> ([], [])
  | h :: t ->
    if n = 0 then ([], l)
    else
      let (l1, l2) = split_list_at_n t (n - 1) in
      (h :: l1, l2)

(* Étant donné une table de vérité, écrire une fonction cons_arbre qui construit l’arbre
de décision associé à la table de vérité T. Il s’agit d’un arbre binaire totalement équilibré, dont les
nœuds internes ont pour étiquette la valeur de leur profondeur et les feuilles sont étiquetées (via le
parcours préfixe) avec les éléments de T *)

let cons_arbre (t : bool list) : decisionTree =
  match t with
  | [] -> raise (Failure "Empty list")
  | [h] -> Leaf h
  | _ -> 
    let rec aux_cons (t : bool list) (n : int) : decisionTree = 
      match t with
      | [] -> raise (Failure "Empty list")
      | [h] -> Leaf h
      | _ -> 
        let l = List.length t in
        let l1 , l2 = split_list_at_n t (l/2) in
        Node (n, aux_cons l1 (n+1), aux_cons l2 (n+1))
    in
    aux_cons t 1
;;

let rec printTree (t : decisionTree) (indent : string) : unit = 
  match t with
  | Leaf b -> print_string (indent ^ "└─ " ^ string_of_bool b ^ "\n")
  | Node (n, t1, t2) -> 
    print_string (indent ^ "├─ " ^ "Depth " ^ string_of_int n ^ "\n");
    printTree t1 (indent ^ "│  ");
    printTree t2 (indent ^ "│  ")
;;

(* test  with a list representing big integer : 25899 *)
let k = table 25899 16;;
print_string "\ntable 25899 64 = \n";;
printboolList k;;
let k = cons_arbre k;;
print_string "\ncons_arbre ( table 25899 64 ) = \n";;
printTree k "";;


(* Étant donné un nœud N de l’arbre (interne ou feuille), écrire une fonction, nommée
liste_feuilles et qui construit la liste des étiquettes des feuilles du sous-arbre enraciné en N, liste
ordonnée de la feuille la plus à gauche jusqu’à celle la plus à droite. *)
let rec liste_feuilles (t : decisionTree) : bool list = 
  match t with
  | Leaf b -> [b]
  | Node (n, t1, t2) -> liste_feuilles t1 @ liste_feuilles t2
;;
(* test *)
let k = table 25899 16;;
let kd = cons_arbre k;;
let kf = liste_feuilles kd;;
print_string "\nliste_feuilles ( cons_arbre ( table 25899 64 ) ) = \n";;
printboolList kf;;

(* Section 3 : Compression de l’arbre de décision et ZDD *)