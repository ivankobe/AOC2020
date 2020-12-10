#load "unix.cma"
#use "topfind"
#require "str"
open Str

let day = "4"

let rec contains elements list =
    match elements with
    | [] -> true
    | x :: xs -> if List.mem x list then contains xs list else false

let tags = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
let colors = ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]

(* https://stackoverflow.com/questions/39813584/how-to-split-on-whitespaces-in-ocaml *)
let whitespace_regexp = Str.regexp "[ \n\r\x0c\t]+"

let reghgt = regexp "^\\([0-9]+\\)\\(in\\|cm\\)$"

(* It seems that the use of {m,n} pattern requires additional libraries *)
let reghcl = regexp "^#[a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9]$"
let regpid = regexp "^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$"

let cm_conditions num = num >= 150 && num <= 193
let in_conditions num = num >= 59 && num <= 76

let rec validate_passport = function
    | [] -> true
    | x :: xs ->
        let (hd, tl) = x in
        let continue = validate_passport xs in
        match hd with
        | "byr" -> let num = (int_of_string tl) in if (num >= 1920 && num <= 2002) then continue else false
        | "iyr" -> let num = (int_of_string tl) in if (num >= 2010 && num <= 2020) then continue else false
        | "eyr" -> let num = (int_of_string tl) in if (num >= 2020 && num <= 2030) then continue else false
        | "hcl" -> if (Str.string_match reghcl tl 0) then continue else false
        | "pid" -> if (Str.string_match regpid tl 0) then continue else false
        | "ecl" -> if (List.mem tl colors) then continue else false
        | "hgt" ->
        let p = Str.string_match reghgt tl 0 in
            if p = false then false
            else
        let num = Str.matched_group 1 tl in
        let unit = Str.matched_group 2 tl in
            if (unit = "cm" && cm_conditions (int_of_string num)) then continue
            else if (unit = "in" && in_conditions (int_of_string num)) then continue
            else false
        | _ -> continue

let rec list_list_to_tuple_list = function
    | [] -> []
    | [x; y] :: xs -> (x,y) :: (list_list_to_tuple_list xs)
    | _ -> failwith "an incorrect argument was passed to this function"

let pt1 input = 
input
|> Str.split (Str.regexp "\n\n")  |> List.map (Str.split whitespace_regexp)
|> List.map (List.map (fun str -> String.sub str 0 3)) |> List.filter (contains tags)
|> List.length |> string_of_int

let pt2 input =
input
|> Str.split (Str.regexp "\n\n")  |> List.map (Str.split whitespace_regexp)
|> List.map (List.map (String.split_on_char ':')) |> List.map list_list_to_tuple_list
|> List.filter validate_passport |> List.map (List.map fst) |> List.filter (contains tags)
|> List.length |> string_of_int

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