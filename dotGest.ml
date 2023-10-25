#use "int64list.ml";;
#use "decisiontree.ml";;


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


