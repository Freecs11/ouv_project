#load "unix.cma" ;;
open Gc ;;
open Unix ;;
#use "decisiontree.ml" ;;
#use "int64list.ml" ;;

let rec sizeOfTree (t : decisionTree) : int =
  match t with
  | Empty -> 0
  | Leaf b -> 1
  | Node (n, t1, t2) -> 1 + sizeOfTree t1 + sizeOfTree t2
;;
(* on va generer des arbres de decision aleatoirement de taille 0 juqu'à 154631 qui est la taille maximale que mon algorithme peut supporter , sinon il y aura un stack overflow *) 
let rec generateRandomDecisionTree (size : int) : decisionTree =
  let max_size = 153000 in
  let size = min size max_size in
  let k = genAlea size in
  let decp = decomposition k in
  let transformed = transformListBoolEquilibre decp in
  let farbre = cons_arbre transformed in
  farbre
;;
(* on va calculer le taux de compression pour un arbre de decision *)
let calculateCompressionRate (t : decisionTree) : float =
  let sizeOfCompressedTree = sizeOfTree (compressionParListe t {l=[]}) in
  let sizeOfTree = sizeOfTree t in
  let rate = (float_of_int sizeOfCompressedTree) /. (float_of_int sizeOfTree) in
  rate
;;

(* Function to measure memory usage in bytes *)
let measure_memory_usage () =
  Gc.full_major (); (* Run a full garbage collection to get accurate memory usage *)
  let stats = Gc.quick_stat () in
  let memory_usage = stats.minor_words *. 8. +. stats.major_words *. 8. in
  memory_usage
;;

let timeit f =
  let t1 = Unix.gettimeofday () in
  let result = f () in
  let t2 = Unix.gettimeofday () in
  (result, t2 -. t1)
;;

let generateExperimentalData () =
  let data = ref [] in
  let max_size = 150000 in
  let step = 200 in  (* Specify your desired step size here *)
  let rec generate_data size =
    if size <= max_size then (
      let before_gen_memory = measure_memory_usage () in
      let (random_tree,gen_time) = timeit (fun () -> generateRandomDecisionTree size) in
      let after_gen_memory = measure_memory_usage () in
      let gen_memory = after_gen_memory -. before_gen_memory in
      let before_comp_memory = measure_memory_usage () in
      let (compression_rate,comp_time) = timeit (fun () -> calculateCompressionRate random_tree) in
      let after_comp_memory = measure_memory_usage () in
      let comp_memory = after_comp_memory -. before_comp_memory in
      let row =  (size, gen_time, gen_memory, comp_time, comp_memory, compression_rate) in
      data :=  row :: !data;
      generate_data (size + step)
    )
  in
  generate_data 0;
  List.rev !data
;;

let saveExperimentalData data filename =
  let oc = open_out filename in
  (*      let row =  (size, gen_time, gen_memory, comp_time, comp_memory, compression_rate) *)
  Printf.fprintf oc "size, gen_time, gen_memory, comp_time, comp_memory, compression_rate\n";
  List.iter (fun (size, gen_time, gen_memory, comp_time, comp_memory, compression_rate) ->
      Printf.fprintf oc "%d, %f, %f, %f, %f, %f\n" size gen_time gen_memory comp_time comp_memory compression_rate
    ) data;
  close_out oc
  ;;

let data = generateExperimentalData () in
saveExperimentalData data "experimental_data.csv";;