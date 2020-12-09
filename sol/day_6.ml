#use "topfind"
#require "str"
open Str

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

let rec mem_forall char = function
    | [] -> true
    | x :: xs ->
        if String.contains x char then mem_forall char xs
        else false

let common_chars list = 
    let rec aux common (hd, tl) = match hd with
        | "" -> common
        | s ->
        let first = s.[0] in
        let rest = (String.sub s 1 ((String.length s) - 1)) in
        if List.mem first common then aux common (rest, tl)
        else if mem_forall first tl then aux (first :: common) (rest, tl)
        else aux common (rest, tl)
    in aux [] (List.hd list, List.tl list)

let day6pt1 input = input
|> Str.split (Str.regexp "\n\n")
|> List.map (String.split_on_char '\n')
|> List.map concat_list
|> List.map count_chars
|> list_sum |> string_of_int

let day6pt2 input = input
|> Str.split (Str.regexp "\n\n")
|> List.map (String.split_on_char '\n')
|> List.map common_chars
|> List.map List.length
|> list_sum |> string_of_int