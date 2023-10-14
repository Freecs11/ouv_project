let rec for_all c l = match l with 
  | [] -> true 
  | x :: y -> c x && for_all c y ;;

let rec map2  c l t = match (l , t) with 
  | ([] , p) -> []
  | (r , []) -> []
  | (x::y , d::f) -> c x d :: (map2 c y f)

let rec combine c t = 
  match (c ,t) with 
  | (x, [] ) -> [] 
  |( [] , x ) -> []
  | ( x::y , k::l) -> (x,k) :: combine y l;;


(* tests with print  of previous methods *)

let rec map f l = match l with
  | [] -> []
  | x :: xs -> f x :: map f xs ;;

  
let print_int_list l = 
  let rec aux l = match l with 
    | [] -> ()
    | x :: y -> print_int x ; print_string " " ; aux y 
  in 
  print_string "[ " ; aux l ; print_string "]\n" ;;
  

  print_int_list (map (fun x -> x + 1) [1;2;3;4;5;6;7;8;9;10]) ;;

