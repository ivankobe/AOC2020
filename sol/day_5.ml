#use "topfind"
#require "str"
open Str

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
        | y :: ys -> (
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

let day5pt1 input = 
input |> calculate_ids |> sort |> List.rev |> List.hd |> string_of_int

let day5pt2 input = input |> calculate_ids |> sort |> find_id |> string_of_int