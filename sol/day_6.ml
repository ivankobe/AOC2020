#use "topfind"
#require "str"
#load "unix.cma"
open Str

let day = "6"

let concat_list list =
    let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (acc ^ x) xs in
    aux "" list

let count_chars string =
    let rec aux seen = function
    | "" -> List.length seen
    | s ->
        let first = s.[0] in
        let rest = (String.sub s 1 ((String.length s) - 1)) in
        if List.mem first seen then aux seen rest
        else aux (first :: seen) rest in
    aux [] string

let list_sum list =
    let rec aux sum = function
    | [] -> sum
    | x :: xs -> aux (sum + x) xs in
    aux 0 list

let common_chars list = 
    let rec aux common (hd, tl) = match hd with
        | "" -> common
        | s ->
        let first = s.[0] in
        let rest = (String.sub s 1 ((String.length s) - 1)) in
        if List.mem first common then aux common (rest, tl)
        else if (not (List.mem false (List.map (fun x -> String.contains x first) tl))) then aux (first :: common) (rest, tl)
        else aux common (rest, tl)
    in aux [] (List.hd list, List.tl list)

let naloga1 input = input
|> Str.split (Str.regexp "\n\n")
|> List.map (String.split_on_char '\n')
|> List.map concat_list
|> List.map count_chars
|> list_sum |> string_of_int

let naloga2 input = input
|> Str.split (Str.regexp "\n\n")
|> List.map (String.split_on_char '\n')
|> List.map common_chars
|> List.map List.length
|> list_sum |> string_of_int

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
    
    let time1 = Unix.gettimeofday () in
    let odgovor1 = naloga1 vsebina_datoteke in
    let time_used1 = Unix.gettimeofday () -. time1 in

    let time2 = Unix.gettimeofday () in
    let odgovor2 = naloga2 vsebina_datoteke in
    let time_used2 = Unix.gettimeofday () -. time2 in
    
    izpisi_datoteko ("/home/ivan/Faks/Prog1/AOC2020/out/day_" ^ day ^ "_1.out") (odgovor1 ^ " in " ^ (string_of_float time_used1) ^ "s");
    izpisi_datoteko ("/home/ivan/Faks/Prog1/AOC2020/out/day_" ^ day ^ "_2.out") (odgovor2 ^ " in " ^ (string_of_float time_used2) ^ "s")