#load "unix.cma"

let day = "9"

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


let pt1 input =

  let rec aux hashtbl n =
    let (list, x) = Hashtbl.find hashtbl n in
    if n <= 25 || summands_exist list x then aux hashtbl (n + 1)
    else x
  in
  aux (parse input) 1 |> string_of_int


let pt2 input =

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

  aux' (parse' input) 1 (input |> pt1 |> int_of_string) |> string_of_int


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