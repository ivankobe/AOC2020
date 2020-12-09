type tape = (int, int list * int) Hashtbl.t

module IntSet = Set.Make( 
  struct
    let compare = Stdlib.compare
    type t = int
  end )

let set_of_list (list : int list) =
  let rec aux set = function
  | [] -> set
  | x :: xs ->
    aux (IntSet.add x set) xs
  in
  aux IntSet.empty list

let summands_exist list sum =
  let rec aux (set : IntSet.t) sum = function
  | [] -> false
  | x :: xs ->
    if 2 * x = sum then aux set sum xs
    else if IntSet.mem (sum - x) set then true
    else aux set sum xs
  in
  let set = set_of_list list in
  aux set sum list


(* parser for pt. 1 *)
let parse input =

  let rec parser (hashtbl : tape) n = function
    | [] -> hashtbl
    | x :: xs ->
      let list =
        if n = 1
          then []
        else let (list', x') = Hashtbl.find hashtbl (n - 1) in
          if n > 26
            then (List.tl list') @ [x']
        else
          list' @ [x']
        in
        Hashtbl.add hashtbl n (list, x);
        parser hashtbl (n + 1) xs
  in 

input |> String.split_on_char '\n' |> List.map int_of_string |> parser (Hashtbl.create 1000) 1


(* parser for pt. 2 *)
let parse' input = 
  
  let rec parser (hashtbl : (int, int) Hashtbl.t) n = function
    | [] -> hashtbl
    | x :: xs ->
      Hashtbl.add hashtbl n x;
      parser hashtbl (n + 1) xs
    in
  input |> String.split_on_char '\n' |> List.map int_of_string |> parser (Hashtbl.create 1000) 1


let day9pt1 input =

  let rec aux hashtbl n =
    let (list, x) = Hashtbl.find hashtbl n in
    if n <= 25 || summands_exist list x then aux hashtbl (n + 1)
    else x
  in
  aux (parse input) 1 |> string_of_int


let day9pt2 input =

  (* tries to sum consecutive ints into
  a given sum starting from index n *)
  let rec aux hashtbl acc list n sum =
    let x = Hashtbl.find hashtbl n in
    let acc' = acc + x in
    if acc' = sum then Some (x :: list)
    else if acc' > sum then None
    else aux hashtbl acc' (x :: list) (n + 1) sum
  in

  (* applies aux to consecutive tape entries *)
  let rec aux' hashtbl n sum =
    match aux hashtbl 0 [] n sum with
    | None -> aux' hashtbl (n + 1) sum
    | Some list ->
      let sorted = List.sort compare list in
      let a = List.hd sorted in
      let b = List.hd (List.rev sorted) in
      a + b
    in

  aux' (parse' input) 1 (input |> day9pt1 |> int_of_string) |> string_of_int