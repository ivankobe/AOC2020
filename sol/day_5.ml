#use "topfind"
#require "str"
open Str

let day = "5"

(* Given an initial range, it correctly divides
it according to the character passed *)
let divide_range (range : (int * int)) char =
    let min, max = range in
    let middle = (min + max) / 2 in
    if (max - min) > 1 then 
        match char with
        | 'F' | 'L' -> (min, middle)
        | 'B' | 'R' -> (middle + 1, max)
        | _ -> failwith "incorrect character"
    else
        match char with
        | 'F' | 'L' -> (min, min)
        | 'B' | 'R' -> (max, max)
        | _ -> failwith "incorrect character"

(* Loops through a FL- or BR-string and
returns the corresponding row/column *)
let rec loop_chairs range = function
    | [] -> failwith "incorrect argument"
    | x :: xs -> (let new_range = divide_range range x in
        match xs with
        | y :: ys -> (
            match x with
            | 'F' | 'L' | 'B' | 'R' -> loop_chairs new_range xs
            | _ -> failwith "incorrect argument")
        | _ -> (match x with
            | 'F' | 'L' -> fst new_range
            | 'B' | 'R' -> snd new_range
            | _ -> failwith "incorrect argument"))

(* Returns the maximum element of a given int list *)
let list_max int_list =
    let rec aux curr = function
        | [] -> (match curr with
            | None -> failwith "None"
            | Some n -> n)
        | x :: xs -> (match curr with
            | None -> aux (Some x) xs
            | Some n -> if x > n then aux (Some x) xs else aux (Some n) xs)
        in
    aux None int_list

(* https://stackoverflow.com/a/52392884 *)
let explode s = List.init (String.length s) (String.get s)

let calculate_ids input =
input
|> String.split_on_char '\n'
|> List.map (fun seat -> (Str.first_chars seat 7, Str.last_chars seat 3))
|> List.map (fun (x,y) -> (loop_chairs (0,127) (explode x), loop_chairs (0,7) (explode y)))
|> List.map (fun (x,y) -> (x * 8) + y)

(* Starting from 0, we check consecutive ints untill we
find one that satisfies the criteria from part 2 *)
let find_id id_list =
    let rec aux m id_list =
        if m > 922 then failwith "sometnihng's wrong"
        else if not (List.mem m id_list) && List.mem (m - 1) id_list && List.mem (m + 1) id_list
        then m
        else aux (m + 1) id_list in
    aux 0 id_list

let naloga1 input = input |> calculate_ids |> list_max |> string_of_int

let naloga2 input = input |> calculate_ids |> find_id |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko ("/home/ivan/Faks/Prog1/AOC2020/in/day_" ^ day ^ ".in") in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko ("/home/ivan/Faks/Prog1/AOC2020/out/day_" ^ day ^ "_1.out") odgovor1;
    izpisi_datoteko ("/home/ivan/Faks/Prog1/AOC2020/out/day_" ^ day ^ "_2.out") odgovor2