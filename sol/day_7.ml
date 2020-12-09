# use "topfind"
#require "str"
open Str

type bag = string * (string list)

type bag_list = bag list

let rule_regex = Str.regexp "^\([a-z]+ [a-z]+\) [a-z]+ [a-z]+ \(.+\)$"

let rec apply_rule = function
    | [] -> []
    | x :: xs ->
        let _ = Str.string_match rule_regex x 0 in
        let color = Str.matched_group 1 x in
        let colors = Str.matched_group 2 x in
        (color, colors) :: (apply_rule xs)

let list_sum list =
    let rec aux sum = function
    | [] -> sum
    | x :: xs -> aux (sum + x) xs in
    aux 0 list

let format str = str
|> String.split_on_char '\n'
|> apply_rule

let rec retrieve_colors acc list =
    match list with
    | [] -> acc
    | x :: y :: z :: w :: xs
        when Str.string_match (regexp "[0-9]") x 0 ->
        retrieve_colors ((int_of_string x, y ^ " " ^ z) :: acc) xs
    | _ -> acc

let count_containing_bags (bag_list : bag_list) color =

    (* To each bag an additional value "" is assorted, so that
    Hashtbl.find_all hashtbl *empty bag* = [""] *)
    let rec update_bag_hash hashtbl key = function
        | [] -> Hashtbl.add hashtbl key ""
        | x :: xs ->
                Hashtbl.add hashtbl key x;
                update_bag_hash hashtbl key xs
    in

    let rec create_bag_hash hashtbl (bags : bag_list) = match bags with
        | [] -> hashtbl
        | (color, items) :: xs ->
            update_bag_hash hashtbl color items;
            create_bag_hash hashtbl xs
    in

    let my_hash = create_bag_hash (Hashtbl.create 1000) bag_list in

    let rec contains color_big hashtbl color_small =
        let children = Hashtbl.find_all hashtbl color_big in
        if List.length children = 1 then false
        else if List.mem color_small children then true
        else List.mem true (List.map (fun child -> contains child hashtbl color_small) children)
    in

    let rec find acc (list : bag_list) hashtbl color =
        match list with
        | [] -> acc
        | (c, cs) :: xs ->
            let acc' = if contains c hashtbl color then acc + 1 else acc in
            find acc' xs hashtbl color
    in

    find 0 bag_list my_hash color


let rec count_contained_bags list color =

    let rec aux color = function
    | [] -> []
    | (c, cs) :: xs ->
        if c = color
        then retrieve_colors [] (String.split_on_char ' ' cs)
        else aux color xs in

    let initial = aux color list in
    1 + list_sum (List.map (fun (num, col) -> num * (count_contained_bags list col)) initial)


let day7pt1 input =
input
|> format
|> List.map (fun (x,y) -> (x, List.map snd (retrieve_colors [] (String.split_on_char ' ' y))))
|> (fun x -> count_containing_bags x "shiny gold")
|> string_of_int

let day7pt2 input =
input
|> format
|> (fun x -> count_contained_bags x "shiny gold")
|> (fun x -> x - 1)
|> string_of_int