#load "unix.cma"

let day = "10"

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

let parse input = 
input |> String.split_on_char '\n' |> List.map int_of_string |> List.sort compare

let pt1 input =

  let rec aux curr acc acc' = function
  | [] -> acc * (acc' + 1)
  | x :: xs ->
    if x - curr = 1 then aux x (acc + 1) acc' xs
    else if x - curr = 2 then aux x acc acc' xs
    else if x - curr = 3 then aux x acc (acc' + 1) xs
    else failwith "something is wrong"
  in
  string_of_int (aux 0 0 0 (parse input))

type adapter_tree = (int, int) Hashtbl.t

let maketree input : adapter_tree =

  let my_list = parse input in
  let max = List.hd (List.rev my_list) in
  let my_set = IntSet.add max (set_of_list my_list) in

  let rec aux hashtbl set n = 
  if n = 0 || IntSet.mem n set then (
    (if IntSet.mem (n + 1) set then Hashtbl.add hashtbl n (n + 1));
    (if IntSet.mem (n + 2) set then Hashtbl.add hashtbl n (n + 2));
    (if IntSet.mem (n + 3) set then Hashtbl.add hashtbl n (n + 3));
    aux hashtbl set (n + 1))
  else if n > max then hashtbl
  else aux hashtbl set (n + 1)
  in
  aux (Hashtbl.create 10000) my_set 0

let list_sum list =
  let rec aux sum = function
  | [] -> sum
  | x :: xs -> aux (sum + x) xs in
  aux 0 list

let pt2 input =

  (* pairs (n, count paths n tree) *)
  let paths = Hashtbl.create 1000 in

  let rec count_paths (n : int) (tree : adapter_tree) =
    try Hashtbl.find paths n
    with Not_found ->
      (
      let children = Hashtbl.find_all tree n in
      let value =
        if children = [] then 1
        else list_sum (List.map (fun child -> count_paths child tree) children)
      in
      Hashtbl.add paths n value;
      value
      )
  in
  string_of_int (count_paths 0 (maketree input))

let _ =
  let preberi_datoteko ime_datoteke =
      let chan = open_in ime_datoteke in
      let vsebina = really_input_string chan (in_channel_length chan) in
      close_in chan;
      vsebina
  and izpisi_datoteko ime_datoteke vsebina =
      let chan = open_out ime_datoteke in
      output_string chan vsebina;
      close_out chan
  in
  let vsebina_datoteke = preberi_datoteko ("/home/ivan/AOC2020/in/day_" ^ day ^ ".in") in
  
  let time1 = Unix.gettimeofday () in
  let odgovor1 = pt1 vsebina_datoteke in
  let time_used1 = Unix.gettimeofday () -. time1 in

  let time2 = Unix.gettimeofday () in
  let odgovor2 = pt2 vsebina_datoteke in
  let time_used2 = Unix.gettimeofday () -. time2 in
  
  izpisi_datoteko ("/home/ivan/AOC2020/out/day_" ^ day ^ "_1.out") (odgovor1 ^ " in " ^ (string_of_float time_used1) ^ "s");
  izpisi_datoteko ("/home/ivan/AOC2020/out/day_" ^ day ^ "_2.out") (odgovor2 ^ " in " ^ (string_of_float time_used2) ^ "s")