
open Random;;
(*
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
  let rec constructList' (n : int) (acc : int64list) =
    if n < 64 then (
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


(* fonction qui transform un int64 en bool list *)
let rec int64toBoolList (x : int64) (aux : bool list) : bool list =
  match x with
  | 0L -> aux
  | x -> if (Int64.logand x 1L) = 1L then int64toBoolList (Int64.shift_right x 1) (true :: aux)
    else int64toBoolList (Int64.shift_right x 1) (false :: aux)
;;


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
      (*boolj list of nb fasle*)
      let boolj : bool list =  List.init nb (fun i -> false) in
      x@boolj
    in
    aux x (nb - List.length x) 
    ;;

let rec decomposition (x : int64list) : bool list =
  match x.l with
    | [] -> failwith "Empty List"
    | [t] -> int64toBoolList t []
    | h :: t -> (completion (int64toBoolList h []) 64) @ decomposition { l = t; size = x.size - 1 }
    ;;

(*fonction pour afficher une liste de boolean *)
let rec printboolList (l : bool list) : unit =
  match l with
  | [] -> print_string "\n"
  | h :: t ->
    print_string "  ";
    print_string (string_of_bool h);
    printboolList t
  ;;


(* approche : 1 + 2 * boolListToInt t  puisque on représente cela en little endian, donc le bit poids faible est à la tête de la liste
  boolListToInt : bool list -> int64 *)
let rec boolListToInt64 (x : bool list) : int64 = 
  match x with
  | [] -> Int64.zero 
  | h::t -> if h then Int64.add 1L (Int64.mul 2L (boolListToInt64 t))
    else Int64.mul 2L (boolListToInt64 t) 
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
    ;;

(* Question 4 *)
let composition (l: bool list) : int64list =
  let listof64 = ref (split_list_at_n l 64 )  in (* (list de 64 elements * reste de la liste donc on refait split sur le reste également) *)
  let rec aux ( le : bool list * bool list) (auxi : int64list) : int64list = 
    match le with
    | ([],[]) -> auxi
    | (h,[]) ->  { l = auxi.l @ [boolListToInt64 h] ; size = auxi.size + 1 } 
    | (h, t) ->  let newlist = ref(split_list_at_n t 64) in (* on split le reste de la liste *)
      aux (!newlist) { l = auxi.l @ [boolListToInt64 h] ; size = auxi.size + 1 }
  in
  aux !listof64 { l = []; size = 0 }
;;





(* Question 5 
  approche : on applique decomposition puis completion
    x: entier à transformer en bool list
    n: nombre de bits de la liste de sortie
*)
let table (x : int64list) (n : int) : bool list = 
  completion (decomposition  x) n
    ;;

(* Question 6 
  n = nombre de bits de l'eniter à générer
  Pour constituer un entier aléatoire sur maximum n bits, on construit une liste d’entiers
contenant ℓ entiers avec ℓ = n/64, chaque entier étant sur 64 bits, suivi d’un entier aléatoire inférieur à
n−ℓ×64. Ainsi si on souhaite un entier aléatoire contenant au plus 100 bits, on génère aléatoirement un
entier sur 64 bits, suivi d’un entier strictement plus petit que 2
36. Définir une fonction GenAlea prenant
en entrée une valeur n et générant un grand entier aléatoire de n bits au maximum
 *)
 let genAlea (n : int) : int64list =
  let rec aux (n : int) (l : int64list) =
    if n <= 0 then l
    else (
      let g = Int64.max_int in
      (* on vérifie si n est supérieur à 64, si oui on ajoute un entier aléatoire sur 64 bits à la liste *)
      if n >= 64 then  (
        insertEndlist (Random.int64 g) l;
        aux (n - 64) l 
      )
      else  (
        let g = Int64.shift_right g (64 - n) in   (* g est un entier sur 64 bits donc on shift à droit de 64-n bits pour avoir un entier sur n bits *)
        insertEndlist (Random.int64 g) l;
        aux 0 l
      )
    )
  in 
  aux n { l = []; size = 0 }
;;

(* fonction pour trouvé la puissance de 2 la plus proche de n *)
let rec findClosestUpPowerOf2 (n : int) (acc : int) : int =
  if n <= acc then acc
  else findClosestUpPowerOf2 n (acc * 2)
;;

(* fonction pour transformé une liste de boolean de taille quelconque en une liste de boolean de taille puissance de 2 *)
let transformListBoolEquilibre (l: bool list) : bool list =
  let list_length = List.length l in
  let closestUpPowerOf2 = findClosestUpPowerOf2 list_length 1 in
  completion l closestUpPowerOf2
;;



