#use "topfind";;
#require "num";;
open Num;;
open Big_int;;
open Random;;
#use "int64list.ml";;

(*Section 1 dans int64list.ml*)
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

let calculateInt64list (l:bool list) : int64list = 
  (* we have a boolListtoInt that converts a bool list to an int64 
   so maybe we can use to convert a bool list of size 64 to an int64 and add it to our int64list *)   
  let rec aux (l:bool list) (auxi:int64list) : int64list = 
    if List.length l <= 64 then 
      let k = boolListToInt64 l in
      insertEndlist k auxi ;
      auxi
    else 
      let l1 , l2 = split_list_at_n l 64 in
      let k = boolListToInt64 l1 in
      let kf = aux l2 auxi in
      insertEndlist k kf ;
      kf
  in
  aux l {l=[];size=0}
;;

(* test *)
let k = calculateInt64list [false; true; true; false; false; true];;
print_string "\ncalculateInt64list([false; true; true; false; false; true]) = \n";;
printList k;;
  
(* liste_feuilles ( cons_arbre ( table 25899 64 ) ) =
  true  true  false  true  false  true  false  false  true  false  true  false  false  true  true  false   
*)
let k = calculateInt64list [true; true; false; true; false; true; false; false; true; false; true; false; false; true; true; false];;
print_string "\ncalculateInt64list(liste_feuilles ( cons_arbre ( table 25899 64 ) )) = \n";;
printList k;;
  
type listeDejaVus = { mutable l : (int64list * decisionTree) list }
;;

let insertEndListeDejaVus (x : int64list) (y : decisionTree) (l : listeDejaVus) : unit =
    l.l <- (x, y) :: l.l
  ;;

(* search the listeDejàvu and return the pointer to the graph (if it exists in the list else returns None) *)
let rec searchListeDejaVus (x : int64list) (l : listeDejaVus) : decisionTree option =
  match l.l with
  | [] -> None
  | (x1, y1) :: t ->
      if x1.l = x.l then Some y1
      else searchListeDejaVus x { l = t }
  ;;

let checkint64listsEq (l1:int64list) (l2 : int64list) :bool =
  if l1.size <> l2.size then false
  else 
    let rec aux (l1:int64 list) (l2:int64 list) : bool = 
      match l1,l2 with
      | [],[] -> true
      | h1::t1, h2::t2 -> if h1 = h2 then aux t1 t2 else false
      | _ -> false
  in
  aux l1.l l2.l
 ;;

(*
Définir une structure de données permettant d’encoder une liste, nommée ListeDejaVus
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
vers N.   
*)

(*— règle-M : Si deux nœuds M et N sont les racines de sous-arbres ayant le même résultat pour
liste_feuilles, alors les arêtes pointant vers N sont remplacées par des arêtes pointant vers
M dans toute la structure ; puis le nœud N est supprimé de la structure.
*)
let rec applyRuleM (tree : decisionTree) (l : listeDejaVus) : decisionTree =
  let rec applyRuleMHelper (t : decisionTree) (l : listeDejaVus) : decisionTree =
    match t with
    | Leaf _ -> t
    | Node (a, t1, t2) ->
      let l1 = liste_feuilles t1 in
      let l2 = liste_feuilles t2 in
      let l1c = calculateInt64list l1 in
      let l2c = calculateInt64list l2 in
      if(checkint64listsEq (l1c) (l2c)) then
        let compressedT1 = applyRuleMHelper t1 l in
        insertEndListeDejaVus l2c t2 l;
        Node (a, compressedT1, t2)
      else
        let compressedT1 = applyRuleMHelper t1 l in
        let compressedT2 = applyRuleMHelper t2 l in
        insertEndListeDejaVus l2c t2 l;
        insertEndListeDejaVus l1c t1 l; 
        Node (a, compressedT1, compressedT2)
  in
  applyRuleMHelper tree l
;;

(*règle-Z : si l’enfant droit de N pointe vers f alse, alors toutes les arêtes pointant vers N sont
remplacées par des arêtes pointant vers l’enfant gauche de N ; puis le nœud N est supprimé de
la structure*)
let rec applyRuleZ (tree : decisionTree) (l : listeDejaVus) (parent : decisionTree) : decisionTree =
  match tree with
  | Leaf _ -> tree
  | Node (a, t1, t2) ->
    match t2 with
    | Leaf false ->
      let compressedT1 = applyRuleZ t1 l parent in
      compressedT1
    | _ ->
      let compressedT1 = applyRuleZ t1 l parent in
      let compressedT2 = applyRuleZ t2 l parent in
      Node (a, compressedT1, compressedT2)
;;  

let compressionParListe (decTree : decisionTree) (l : listeDejaVus) : decisionTree =
  let rec compression (current : decisionTree) : decisionTree =
    match current with
    | Leaf b -> Leaf b
    | Node (a, t1, t2) ->
      (* Apply the "M" and "Z" rules in any order *)
      let compressedT1 = applyRuleM t1 l in
      let compressedT2 = applyRuleM t2 l in
      let compressedT1 = applyRuleZ compressedT1 l current in
      let compressedT2 = applyRuleZ compressedT2 l current in
      Node (a, compressedT1, compressedT2)
  in
  compression decTree
;;

(* test *)
let k = table {l=[25899L] ; size=1} 16;;
let kd = cons_arbre k;;
print_string "\ncons_arbre ( table 25899 64 ) = \n";;
printTree kd "" ;;
let k = compressionParListe kd  {l=[]};;
print_string "\ncompression ( cons_arbre ( table 25899 64 ) ) = \n";;
printTree k "";;
;;


(* Generation du fichier DOT*)
type node_info = {
  label: string;
  shape: string;
  style: string option;
  id: int;
}

let next_id = ref 0

let get_next_id () =
  let current_id = !next_id in 
  next_id := current_id + 1;
  current_id

  let rec dot (t : decisionTree) : string * node_info =
    match t with
    | Leaf b ->
      let node_id = get_next_id () in
      let node = { label = if b then "True" else "False"; shape = "Msquare"; style = None; id = node_id } in
      let dot_string = Printf.sprintf "n%d [label=\"%s\", shape=%s];\n" node_id node.label node.shape in
      (dot_string, node)
    | Node (a, left, right) ->
      let (left_dot, left_node) = dot left in
      let (right_dot, right_node) = dot right in
      let node_id = get_next_id () in
      let node = { label = string_of_int a; shape = "Mdiamond"; style = None; id = node_id } in
      let dot_string = Printf.sprintf "n%d [label=\"%s\", shape=%s];\n" node_id node.label node.shape in
      dot_string ^ left_dot ^ right_dot ^ 
      Printf.sprintf "n%d -> n%d;\n" node_id left_node.id ^ 
      Printf.sprintf "n%d -> n%d [style=dotted];\n" node_id right_node.id, node
  
let generate_dot_file (t : decisionTree) (filename : string) : unit =
  next_id := 0;
  let dot_content, _  = dot t  in
  let dot_string = Printf.sprintf "digraph G {\n%s}\n" dot_content in
  let oc = open_out filename in
  Printf.fprintf oc "%s" dot_string;
  close_out oc

  
let k = table {l=[25899L] ; size=1} 16;;
let kd = cons_arbre k;;
generate_dot_file kd "test.dot";;    
let k = compressionParListe kd {l=[]};;
generate_dot_file k "test2.dot";;