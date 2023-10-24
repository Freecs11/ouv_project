
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
   so we can use it to convert a bool list of size 64 to an int64 and add it to our int64list *)   
  let rec aux (l:bool list) (auxi:int64list) : int64list = 
    if List.length l <= 64 then 
      let k = boolListToInt64 l in
      insertStartlist k auxi ;
      auxi
    else 
      let l1 , l2 = split_list_at_n l 64 in
      let k = boolListToInt64 l1 in
      let kf = aux l2 auxi in
      insertStartlist k kf ;
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
 
(* search the listeDejàvu and return the pointer to the graph (if it exists in the list else returns None) *)
let rec searchListeDejaVus (x : int64list) (l : listeDejaVus) : decisionTree option =
  match l.l with
  | [] -> None
  | (x1, y1) :: t ->
      if checkint64listsEq x x1 then Some y1
      else searchListeDejaVus x { l = t }
  ;;

  (*  test *)
let k = table {l=[25899L] ; size=1} 16;;
let kd = cons_arbre k;;
let kf = liste_feuilles kd;;
let k = calculateInt64list kf;;
let l = {l=[]};;
insertEndListeDejaVus k kd l;;
let k = table {l=[25899L] ; size=1} 16;; 
let kd = cons_arbre k;;
let kf = liste_feuilles kd;;
let k = calculateInt64list kf;;
let k = searchListeDejaVus k l;;
print_string "\nsearchListeDejaVus ( calculateInt64list(liste_feuilles ( cons_arbre ( table 25899 64 ) )) ) = \n";;
printTree (match k with Some k -> k | None -> Leaf false) "";;


(*— règle-M : Si deux nœuds M et N sont les racines de sous-arbres ayant le même résultat pour
liste_feuilles, alors les arêtes pointant vers N sont remplacées par des arêtes pointant vers
M dans toute la structure ; puis le nœud N est supprimé de la structure.
*)
let rec applyRuleM (tree : decisionTree) (l : listeDejaVus) (ltrue:decisionTree) (lfalse:decisionTree): decisionTree =
  let leaf_list = liste_feuilles tree in
  let int64list = calculateInt64list leaf_list in
  match (searchListeDejaVus int64list l), tree with
  | Some t, _ -> t
  | None, Leaf b -> if b then ltrue else lfalse
  | None, Node (a, t1, t2) ->
    let compressedT1 = applyRuleM t1 l ltrue lfalse in
    let compressedT2 = applyRuleM t2 l ltrue lfalse in
      let compressedTree = Node (a, compressedT1, compressedT2) in
      insertEndListeDejaVus int64list compressedTree l; 
      compressedTree


(*règle-Z : si l’enfant droit de N pointe vers f alse, alors toutes les arêtes pointant vers N sont
remplacées par des arêtes pointant vers l’enfant gauche de N ; puis le nœud N est supprimé de
la structure*)
let rec applyRuleZ (tree : decisionTree) (parent : decisionTree) : decisionTree =
  match tree with
  | Leaf b -> tree
  | Node (a, t1, t2) ->
    let compressedT1 = applyRuleZ t1 tree in
    let compressedT2 = applyRuleZ t2 tree in
    match compressedT2 with
    | Leaf false -> compressedT1
    | _ -> Node (a, compressedT1, compressedT2)
  ;;
let compressionParListe (decTree : decisionTree) (l : listeDejaVus) : decisionTree =
  let ltrue = Leaf true in
  let lfalse = Leaf false in
  let compressedT1Z = applyRuleZ decTree decTree in
  let compressedT1M = applyRuleM compressedT1Z l ltrue lfalse in
  compressedT1M
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

type visitedTrees = (decisionTree * int) list;;

let add_to_visited_trees (t: decisionTree) (id) (l: visitedTrees) : visitedTrees =
  (t, id) :: l
;;

let rec visited_trees_contains (t: decisionTree) (l: visitedTrees) : bool =
  match l with
  | [] -> false
  | (t1, _) :: t2 -> if t1 == t then true else visited_trees_contains t t2
;;

let rec get_id_from_visited_trees (t: decisionTree) (l: visitedTrees) : int =
  match l with
  | [] -> raise (Failure "Tree not found")
  | (t1, id) :: t2 -> if t1 == t then id else get_id_from_visited_trees t t2
;;

type node_info = {
  label: string;
  shape: string;
  style: string option;
  id: int;
  nb_child_written: int ref;
}
;;
let next_id = ref 0
;;
let get_next_id () =
  let current_id = !next_id in 
  next_id := current_id + 1;
  current_id
;;

let rec dot (t : decisionTree) (visited : visitedTrees) : string * node_info * visitedTrees =
  (* visit a node , see if it's in the visited , if so then just ignore it *)
  if visited_trees_contains t visited then
    let id = get_id_from_visited_trees t visited in
    let node_info = { label = ""; shape = "cercle"; style = None; id = id; nb_child_written = ref 0 } in
    ("", node_info, visited)
  else
    (* if not visited then add it to the visited list *)
    let id = get_next_id () in
    let visited = add_to_visited_trees t id visited in
    match t with
    | Leaf b ->
      let node_info = { label = string_of_bool b; shape = "cercle"; style = None; id = id; nb_child_written = ref 0 } in
      (Printf.sprintf "%d [label=\"%s\" shape=\"%s\"];\n" id node_info.label node_info.shape, node_info, visited)
    | Node (n, t1, t2) ->
      let node_info = { label = string_of_int n; shape = "cercle"; style = None; id = id; nb_child_written = ref 0 } in
      let dot_t1, node_info_t1, visited = dot t1 visited in
      let dot_t2, node_info_t2, visited = dot t2 visited in
      let dot_string = Printf.sprintf "%d [label=\"%s\" shape=\"%s\"];\n" id node_info.label node_info.shape in
      let dot_string = dot_string ^ dot_t1 ^ dot_t2 in
      let dot_string = dot_string ^ Printf.sprintf "%d -> %d %s;\n" id node_info_t1.id "[style=dotted]" in
      let dot_string = dot_string ^ Printf.sprintf "%d -> %d;\n" id node_info_t2.id in
      (dot_string, node_info, visited)
;;


let generate_dot_file (t : decisionTree) (filename : string) : unit =
  next_id := 0;
  let dot_content, _, _ = dot t [] in
  let dot_string = Printf.sprintf "digraph G {\n%s}\n" dot_content in
  let oc = open_out filename in
  Printf.fprintf oc "%s" dot_string;
  close_out oc
;;

let k = table {l=[25899L] ; size=1} 16;;
let kd = cons_arbre k;;
generate_dot_file kd "test.dot";;    
let k = compressionParListe kd {l=[]};;
generate_dot_file k "testtable16.dot";;