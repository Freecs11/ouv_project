#use "int64list.ml";;
#use "decisiontree.ml";;


(* Generation du fichier DOT*)
(* visitedTrees is a list of visited trees , it contains the tree and the id of the node in the dot file *)
type visitedTrees = (decisionTree * int) list;;

(* add a tree to the visited trees *)
let add_to_visited_trees (t: decisionTree) (id) (l: visitedTrees) : visitedTrees =
  (t, id) :: l
;;

(* check if a tree is in the visited trees *)
let rec visited_trees_contains (t: decisionTree) (l: visitedTrees) : bool =
  match l with
  | [] -> false
  | (t1, _) :: t2 -> if t1 == t then true else visited_trees_contains t t2
;;

(* get the id of a tree in the visited trees *)
let rec get_id_from_visited_trees (t: decisionTree) (l: visitedTrees) : int =
  match l with
  | [] -> raise (Failure "Tree not found")
  | (t1, id) :: t2 -> if t1 == t then id else get_id_from_visited_trees t t2
;;

(* node_info is the information about a node in the dot file , it contains the label , the shape , the style and the id *)
type node_info = {
  label: string;
  shape: string;
  style: string option;
  id: int;
}
;;
  
(* counter for the ids n , a ref because we need to increment it *)
let next_id = ref 0
;;
(* get the next id and increment the counter *)
let get_next_id () =
  let current_id = !next_id in 
  next_id := current_id + 1;
  current_id
;;

(* generate the dot file for a tree , it returns the dot string , the node_info of the root and the visited trees *)
let rec dot (t : decisionTree) (visited : visitedTrees) : string * node_info * visitedTrees =
  (* visit a node , see if it's in the visited , if so then just ignore it *)
  if visited_trees_contains t visited then
    let id = get_id_from_visited_trees t visited in
    let node_info = { label = ""; shape = "cercle"; style = None; id = id} in
    ("", node_info, visited)
  else
    (* if not visited then add it to the visited list *)
    let id = get_next_id () in
    let visited = add_to_visited_trees t id visited in
    match t with
    | Empty -> "" , { label = ""; shape = "cercle"; style = None; id = id }, visited
    | Leaf b ->
      let node_info = { label = string_of_bool b; shape = "cercle"; style = None; id = id } in
      (Printf.sprintf "%d [label=\"%s\" shape=\"%s\"];\n" id node_info.label node_info.shape, node_info, visited)
    | Node (n, t1, t2) ->
      let node_info = { label = string_of_int n; shape = "cercle"; style = None; id = id } in
      let dot_t1, node_info_t1, visited = dot t1 visited in
      let dot_t2, node_info_t2, visited = dot t2 visited in
      let dot_string = Printf.sprintf "%d [label=\"%s\" shape=\"%s\"];\n" id node_info.label node_info.shape in
      let dot_string = dot_string ^ dot_t1 ^ dot_t2 in
      let dot_string = dot_string ^ Printf.sprintf "%d -> %d %s;\n" id node_info_t1.id "[style=dotted]" in
      let dot_string = dot_string ^ Printf.sprintf "%d -> %d;\n" id node_info_t2.id in
      (dot_string, node_info, visited)
;;

(* generate the dot file for a tree *)
let generate_dot_file (t : decisionTree) (filename : string) : unit =
  next_id := 0;
  let dot_content, _, _ = dot t [] in
  let dot_string = Printf.sprintf "digraph G {\n%s}\n" dot_content in
  let oc = open_out filename in
  Printf.fprintf oc "%s" dot_string;
  close_out oc
;;

(* tests *)
let k = table {l=[25899L] ; size=1} 16;;
let kd = cons_arbre k;;
generate_dot_file kd "testsimple.dot";;    
let ku = compressionParListe kd {l=[]};;
generate_dot_file ku "testcompressedListe.dot";;
let ks = compressionParArbre kd ;;
generate_dot_file ks "testcompressedArbre.dot";;


let k = {l=[25899L;1L] ; size=2};;
let kd = decomposition k;;
let kd = transformListBoolEquilibre kd;;
let kd = cons_arbre kd;;
let ku = compressionParListe kd {l=[]};;
generate_dot_file ku "testcompressedArbre2.dot";;
let kd = compressionParArbre kd ;;
generate_dot_file kd "testcompressedArbre3.dot";;


let k = genAlea 1000;;
let kd = decomposition k;;
let kd = transformListBoolEquilibre kd;;
let kd = cons_arbre kd;;
let rec sizeOfTree (t : decisionTree) : int =
  match t with
  | Empty -> 0
  | Leaf b -> 1
  | Node (n, t1, t2) -> 1 + sizeOfTree t1 + sizeOfTree t2
;;

let kr = compressionParArbre kd ;;
print_string " length : ";;
print_int (sizeOfTree kr);;
print_string "\n";;
generate_dot_file kr "testcompressedArbre4.dot";;
let ku = compressionParListe kd {l=[]};;
print_string " length : ";;
print_int (sizeOfTree ku);;
print_string "\n";;
generate_dot_file ku "testcompressedListe4.dot";;