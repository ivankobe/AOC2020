#load "unix.cma"

let day = "3"

let line_length = String.length ".#......##..#.....#....#.#.#..."

(* Picks the right character from n-th line *)
let pick_from_line line num_steps n =
    let index = (n * num_steps) mod line_length in line.[index]

(* Searches for trees in a list of lines *)
let pick_from_each_line num_steps_x num_steps_y list =
    (* Indexes elements of a given list *)
    let rec index acc n = function
    | [] -> acc
    | x :: xs -> index (acc @ [(x, n)]) (n + 1) xs in
    let rec aux acc count = function
    | [] -> acc
    | (x, n) :: xs ->
        if ((n mod num_steps_y) = 0)
        then aux (acc @ [pick_from_line x num_steps_x count]) (count + 1) xs
        else aux acc count xs
    in
    aux [] 0 (index [] 0 list)

let trees x y v =
    v |> String.split_on_char '\n' |> pick_from_each_line x y
    |> List.tl |> List.filter (Char.equal '#') |> List.length

let pt1 v =
    v |> trees 3 1 |> string_of_int

let pt2 v =
    ((trees 1 1 v) * (trees 3 1 v) * (trees 5 1 v) * (trees 7 1 v) * (trees 1 2 v)) |> string_of_int

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