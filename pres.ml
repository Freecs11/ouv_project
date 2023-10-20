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

(* big int from our int64list structure 
  Exemple : 2^164 = [0,0,2^36] => droite vers gauche : (2^(64*0))*0 + (2^(64*1))*0 + (2^(64*2))*2^36 = 2^164  
*)
let bigNumFromList ( l : int64list) : big_int = 
  let rec aux ( l: int64 list) (auxi : big_int) (index : int) : big_int =
    match l with
    | [] -> auxi
    | h::t -> aux t (add_big_int (mult_int_big_int (Int64.to_int h) (power_int_positive_int 2 (64*index))) auxi) (index+1)
  in
  aux l.l zero_big_int 0
;;

(* test *)
let k = constructList ( power_int_positive_int 2 100 );;
let k = bigNumFromList k;;
print_string "\nbigNumFromList ( constructInt64List ( power_int_positive_int 2 100 ) ) = \n";;
print_string (string_of_big_int k);;
print_string "\n";;
print_string "power_int_positive_int 2 100 = \n";;
print_string ( string_of_big_int ( power_int_positive_int 2 100 ) );;
print_string "\n";;
let k = power_int_positive_int 2 164;;
let dk = constructList k;;
print_string "\npower_int_positive_int 2 164 = \n";;
print_string (string_of_big_int k);;
print_string " \n number after reconstructing from list  = \n";;
print_string (string_of_big_int (bigNumFromList dk));;
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
let k = table {l=[25899L] ; size=1} 16;;
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
let k = table {l=[25899L] ; size=1} 16;;
let kd = cons_arbre k;;
let kf = liste_feuilles kd;;
print_string "\nliste_feuilles ( cons_arbre ( table 25899 64 ) ) = \n";;
printboolList kf;;

(* Section 3 : Compression de l’arbre de décision et ZDD *)

type listeDejaVus = { mutable l : (int64list * decisionTree) list; mutable size : int }

let insertEndlistDV (x : int64list) (y : decisionTree) (l : listeDejaVus) : unit =
  l.l <- (x, y) :: l.l;
  l.size <- l.size + 1
;;

let getHeadlistDV (l : listeDejaVus) : (int64list * decisionTree) =
  match l.l with
  | [] -> raise (Failure "Empty list")
  | h :: _ -> h
;;

let removeHeadlistDV (l : listeDejaVus) : unit =
  match l.l with
  | [] -> raise (Failure "Empty list")
  | _ :: t -> l.l <- t; l.size <- l.size - 1
;;

let rec printListDV (l : listeDejaVus) : unit =
  match l.l with
  | [] -> print_string "\n"
  | (x, y) :: t ->
    print_string "  ";
    printList x;
    print_string " -> ";
    printTree y "";
    printListDV { l = t; size = l.size - 1 }
;;

let rec boolListEq (l1:bool list) (l2:bool list) : bool =
  match l1, l2 with
  | [], [] -> true
  | [], _ -> false
  | _, [] -> false
  | h1::t1, h2::t2 -> if h1 = h2 then boolListEq t1 t2 else false

(* 
— règle-M : Si deux nœuds M et N sont les racines de sous-arbres ayant le même résultat pour
liste_feuilles, alors les arêtes pointant vers N sont remplacées par des arêtes pointant vers
M dans toute la structure ; puis le nœud N est supprimé de la structure.
— règle-Z : si l’enfant droit de N pointe vers f alse, alors toutes les arêtes pointant vers N sont
remplacées par des arêtes pointant vers l’enfant gauche de N ; puis le nœud N est supprimé de
la structure.
Après avoir utilisé ces règles aussi longtemps que possible l’arbre de décision de départ est compressé
en le graphe ZDD lui correspondant.
Ces règles sont confluentes. On peut les appliquer dans l’ordre que l’on souhaite et sur les nœuds
que l’on souhaite, on obtiendra à terme toujours la même structure compressée : l’unique ZDD.
Pour la suite, les nœuds d’un arbre pourront être utilisés pour être des nœuds d’un graphe, puisque
les ZDD sont des graphes.

3.1 Compression avec historique stocké dans une liste
Question 3.10 Définir une structure de données permettant d’encoder une liste, nommée ListeDejaVus
par la suite, dont les éléments sont des couples avec la première composante étant un grand entier (i.
e. une liste d’entiers), et la seconde composante un pointeur vers un nœud d’un graphe.
Voilà l’algorithme élémentaire de compression d’un arbre de décision.
— Soit G l’arbre de décision qui sera compressé petit à petit. Soit une liste ListeDejaVus vide.
— En parcourant G via un parcours suffixe, étant donné N le nœud en cours de visite :
— Calculer le grand entier n correspondant à la liste des feuilles du sous-arbre enraciné en N ;
— Si n est la première composante d’un couple stocké dans ListeDejaVus, alors remplacer le
pointeur vers N (depuis son parent) par un pointeur vers la seconde composante du couple en
question ;
— Sinon ajouter en tête de ListeDejaVus un couple constitué du grand entier n et d’un pointeur
vers N 

*)

(*
  applique la règle M et Z sur un arbre de décision
  @param dectree l'arbre de décision
  @param l la liste de déjà vus
  @return l'arbre de décision après application de la règle M 
*)
let apply_compression (dectree : decisionTree) :decisionTree = 
  (* parcours suffixe*)
  let rec aux (dectree : decisionTree) (l : listeDejaVus) : decisionTree = 
    match dectree with
    | Leaf b -> dectree
    | Node (n, t1, t2) -> 
      let l1 = liste_feuilles t1 in (* liste feuille  M  *)
      let l2 = liste_feuilles t2 in (* liste feuille  N  *)
      (* TODO *)
      in
      aux2 l
  in
  aux dectree {l=[]; size=0}
