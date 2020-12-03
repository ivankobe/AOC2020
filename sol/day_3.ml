let dan = "3"

let line_length = String.length ".#......##..#.....#....#.#.#..."

let pick_from_line line num_steps n =
    let index = (n * num_steps) mod line_length in line.[index]

let pick_from_each_line num_steps list =
    let rec aux acc count l = match l with
    | [] -> acc
    | x :: xs -> aux (acc @ [pick_from_line x num_steps count]) (count + 1) xs
    in aux [] 0 list

let rec every_odd = function
    | [] -> []
    | x :: [] -> x :: []
    | x :: y :: z -> x :: (every_odd z)

let trees steps v = 
    v |> String.split_on_char '\n' |> pick_from_each_line steps
    |> List.tl |> List.filter (Char.equal '#') |> List.length

let naloga1 v =
    v |> trees 3 |> string_of_int

let one_right_two_down v =
    v |> String.split_on_char '\n' |> every_odd |> pick_from_each_line 1 
    |> List.tl |> List.filter (Char.equal '#') |> List.length
    
let naloga2 v =
    ((trees 1 v) * (trees 3 v) * (trees 5 v) * (trees 7 v) * (one_right_two_down v)) |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko ("/home/ivan/Faks/Prog1/AOC2020/in/day_" ^ dan ^ ".in") in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko ("/home/ivan/Faks/Prog1/AOC2020/out/day_" ^ dan ^ "_1.out") odgovor1;
    izpisi_datoteko ("/home/ivan/Faks/Prog1/AOC2020/out/day_" ^ dan ^ "_2.out") odgovor2