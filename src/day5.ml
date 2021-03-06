#load "unix.cma"
#use "topfind"
#require "str"
open Str

let day = "5"

(* Given an initial range, it correctly divides
it according to the character passed *)
let divide_range range char =
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

(* Loops through a FL- or BR-list and
returns the corresponding row/column *)
let rec loop_chairs range = function
    | [] -> failwith "incorrect argument"
    | x :: xs -> (let new_range = divide_range range x in
        match xs with
        | _ :: _ -> (
            match x with
            | 'F' | 'L' | 'B' | 'R' -> loop_chairs new_range xs
            | _ -> failwith "incorrect argument")
        | _ -> (match x with
            | 'F' | 'L' -> fst new_range
            | 'B' | 'R' -> snd new_range
            | _ -> failwith "incorrect argument"))

let rec find_id = function
    | x :: y :: z ->
        if (y - x) = 2 then (x + 1)
        else find_id (y :: z)
    | _ -> failwith "no id found"

(* https://stackoverflow.com/a/52392884 *)
let explode s = List.init (String.length s) (String.get s)

let calculate_ids input =
input
|> String.split_on_char '\n'
|> List.map (fun seat -> (Str.first_chars seat 7, Str.last_chars seat 3))
|> List.map (fun (x,y) -> (loop_chairs (0,127) (explode x), loop_chairs (0,7) (explode y)))
|> List.map (fun (x,y) -> (x * 8) + y)

let sort = List.sort compare

let pt1 input = 
input |> calculate_ids |> sort |> List.rev |> List.hd |> string_of_int

let pt2 input = input |> calculate_ids |> sort |> find_id |> string_of_int

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