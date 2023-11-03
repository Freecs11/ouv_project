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
  | Empty
  | Leaf of bool
  | Node of int * decisionTree * decisionTree
;;


(* Étant donné une table de vérité, écrire une fonction cons_arbre qui construit l’arbre
de décision associé à la table de vérité T. Il s’agit d’un arbre binaire totalement équilibré, dont les
nœuds internes ont pour étiquette la valeur de leur profondeur et les feuilles sont étiquetées (via le
parcours préfixe) avec les éléments de T *)
let cons_arbre (t : bool list) : decisionTree =
    let rec aux_cons (t : bool list) (n : int) : decisionTree = 
      match t with
      | [] -> Empty
      | [h] -> Leaf h
      | _ -> 
        let l = List.length t in
        let l1 , l2 = split_list_at_n t (l/2) in
        Node (n, aux_cons l1 (n+1), aux_cons l2 (n+1))
    in
    aux_cons t 1
;;

(* fonction pour afficher l'arbre de décision *)
let rec printTree (t : decisionTree) (indent : string) : unit = 
  match t with
  | Empty -> print_string (indent ^ "└─ Empty\n")
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
  | Empty -> []
  | Leaf b -> [b]
  | Node (n, t1, t2) -> liste_feuilles t1 @ liste_feuilles t2
;;
(* test *)
let k = table {l=[25899L] ; size=1} 16;;
let kd = cons_arbre k;;
let kf = liste_feuilles kd;;
print_string "\nliste_feuilles ( cons_arbre ( table 25899 64 ) ) = \n";;
printboolList kf;;

(* une fonction qui calcule le grand entier correspondant à une liste de booléens *)
let calculateInt64list (l:bool list) : int64list = 
  (* On a une fonnction boolListtoInt qui convertit une liste de boolean en Int64
  donc on va l'utiliser pour convertir une liste de booléens de taille 64 en un int64 et le rajouter à notre int64list *)   
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
(* fonction pour insérer un grand entier int64list et sa decisionTree dans la listeDejaVus *)
let insertHeadListeDejaVus (x : int64list) (y : decisionTree) (l : listeDejaVus) : unit =
    l.l <- (x, y) :: l.l
  ;;

  (* Checks if two int64lists have the same elements by comparing one by one*)
let checkint64listsEq (l1:int64list) (l2 : int64list) :bool =
  if l1.size <> l2.size then false
  else 
    let rec aux (l1:int64 list) (l2:int64 list) : bool = 
      match l1,l2 with
      | [],[] -> true
      | h1::t1, h2::t2 -> if Int64.compare h1 h2 = 0 then aux t1 t2 else false
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
insertHeadListeDejaVus k kd l;;
let k = table {l=[25899L] ; size=1} 16;; 
let kd = cons_arbre k;;
let kf = liste_feuilles kd;;
let k = calculateInt64list kf;;
let k = searchListeDejaVus k l;;
print_string "\nsearchListeDejaVus ( calculateInt64list(liste_feuilles ( cons_arbre ( table 25899 64 ) )) ) = \n";;
printTree (match k with Some k -> k | None -> Leaf false) "";;


(* Une fonction qui teste si une liste de booléens ne contient que des valeurs false. *)
let allInListFalse (l : bool list) : bool =
  let rec aux (l : bool list) : bool =
    match l with
    | [] -> true
    | h :: t -> if h then false else aux t
  in
  aux l

(* Définir une structure de données permettant d’encoder une liste, nommée ListeDejaVus
par la suite, dont les éléments sont des couples avec la première composante étant un grand entier (i.
e. une liste d’entiers), et la seconde composante un pointeur vers un nœud d’un graphe.
Voilà l’algorithme élémentaire de compression d’un arbre de décision.
— Soit G l’arbre de décision qui sera compressé petit à petit. Soit une liste ListeDejaVus vide.
— En parcourant G via un parcours suffixe, étant donné N le nœud en cours de visite :
— Calculer la liste_feuilles associées à N (le nombre d’éléments qu’elle contient est une
puissance de 2).
— Si la deuxième moitié de la liste ne contient que des valeurs false alors remplacer le pointeur
vers N (depuis son parent) vers un pointeur vers l’enfant gauche de N
— Sinon, calculer le grand entier n correspondant à liste_feuilles du sous-arbre enraciné
en N ;
— Si n est la première composante d’un couple stocké dans ListeDejaVus, alors remplacer le
pointeur vers N (depuis son parent) par un pointeur vers la seconde composante du couple en
question ;
— Sinon ajouter en tête de ListeDejaVus un couple constitué du grand entier n et d’un pointeur
vers N. *)
let compressionParListe (decTree : decisionTree) (l : listeDejaVus) : decisionTree =
  let rec compressionAux (decTree : decisionTree) (l : listeDejaVus) : decisionTree =
    let calculatedList = liste_feuilles decTree in
    let calculatedInt64list = calculateInt64list calculatedList in
    match decTree with
    | Empty -> Empty
    | Leaf b -> (
      match searchListeDejaVus {l= calculatedInt64list.l; size=calculatedInt64list.size} l with
      | Some t -> t
      | None ->
        let comp = Leaf b in
        insertHeadListeDejaVus {l= calculatedInt64list.l; size=calculatedInt64list.size} comp l;
        comp
      )
    | Node (a, t1, t2) ->
      match searchListeDejaVus calculatedInt64list l with
      | Some t -> t
      | None ->
        let feuillesT2 = liste_feuilles t2 in
        if allInListFalse feuillesT2 then
          let compressedT1 = compressionAux t1 l in
          compressedT1
        else
          let compressedT1 = compressionAux t1 l in
          let compressedT2 = compressionAux t2 l in
          let comp = Node (a, compressedT1, compressedT2) in 
          insertHeadListeDejaVus calculatedInt64list comp l;
          comp
  in
  compressionAux decTree l
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



(*
Définir une structure arborescente de données ArbreDejaVus permettant d’encoder
un arbre binaire tel que l’arête gauche est associé au booléen false et l’arête droite au booléen true.
Les nœuds sont étiquetés avec un pointeur vers un nœud d’un graphe (ou ne sont pas étiquetés suivant
le cas).
L’association d’un booléen à chaque arête permet de se déplacer dans un arbre via une table de
vérité. Ainsi pour atteindre le nœud associé à [f alse;true;true; f alse], on descend à gauche depuis la
racine, puis 2 fois à droite et finalement à gauche.
On va utiliser ArbreDejaVus en tant qu’arbre de recherche pour stocker les pointeurs vers des
sous-arbres déjà vus.   
*)
(* c'est un Trie *)
type arbreDejaVus = 
  | Empty
  | Node2 of ( decisionTree option ) * arbreDejaVus * arbreDejaVus





(* insert an int64 to an ArbreDejaVus from a bool list*)
let insertArbreDejaVus (a: arbreDejaVus) (decTree : decisionTree) : arbreDejaVus =
  let rec aux (a: arbreDejaVus) (path : bool list) : arbreDejaVus =
    match path,a with
      |[],Empty -> Node2(Some(decTree),Empty,Empty)
      |[],Node2(n,g,d) -> a
      |x::xs,Empty -> if x then Node2(None,Empty,aux Empty xs) else Node2(None,aux Empty xs,Empty)
      |x::xs,Node2(n,g,d) -> if x then Node2(n,g,aux d xs) else Node2(n,aux g xs,d)
  in aux a (liste_feuilles decTree)

(* search an int64 in the ArbreDejaVus given*)
let searchArbreDejaVus (x: bool list) (a: arbreDejaVus) : decisionTree option =
  let rec aux (a: arbreDejaVus) (l: bool list) : decisionTree option = 
    match l,a with
    |[],Node2(n,g,d) -> n
    |[],_ -> None
    |x::xs,Empty -> None
    |x::xs,Node2(n,g,d) -> if x then aux d xs else aux g xs
  in aux a x
  
  (*
Q 2 ) 
  Adapter l’algorithme élémentaire pour utiliser l’arbre de recherche ArbreDejaVus
au lieu de la ListeDejaVus.*)
(*
  Algorithme élémentaire compressionParArbre :
  -Soit G l’arbre de décision qui sera compressé petit à petit. Soit un arbreDejaVus vide.
  -En parcourant G via un parcours suffixe, étant donné N le nœud en cours de visite :
  -Calculer la liste_feuilles associées à N (le nombre d’éléments qu’elle contient est une
   puissance de 2).
  -Si la deuxième moitié de la liste ne contient que des valeurs false alors remplacer
   le pointeur vers N (depuis son parent) par un pointeur vers l’enfant gauche de N
  -Sinon, parcourir l’arbreDejaVus en suivant le chemin correspondant à la 
  liste_feuilles du sous-arbre enraciné en N ;
  -Si le chemin existe alors remplacer le pointeur vers N (depuis son parent) par un 
  pointeur vers le nœud correspondant au chemin ;
  -Sinon ajouter en tête de arbreDejaVus un couple constitué du grand entier n et d’un 
  pointeur vers N.
*)
let compressionParArbre (decTree : decisionTree) : decisionTree =
  let a = ref Empty in
  let rec loop (decTree: decisionTree)  : decisionTree =
    let feuilles = liste_feuilles decTree in
    match decTree with
    |Empty -> Empty
    |Leaf b -> ( 
      match searchArbreDejaVus feuilles !a with
        |Some t -> t
        |None -> let compressed = Leaf(b) in
          a := insertArbreDejaVus !a compressed;
          compressed
        )
    |Node(n,g,d) ->( 
      match searchArbreDejaVus feuilles !a with
        |Some t -> t
        |None -> 
          if allInListFalse (liste_feuilles d) then loop g 
          else 
            let compressedG = loop g in
            let compressedD = loop d in
            let compressed = Node(n,compressedG,compressedD ) in
            a := insertArbreDejaVus !a compressed ; 
            compressed
        )
  in loop decTree 
;;
