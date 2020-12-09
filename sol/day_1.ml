let rec find_complement n m = function
    | [] -> None
    | x :: xs -> if x = (m - n) then Some x else find_complement n m xs

let rec find_complement_pair n m = function
    | [] -> None
    | _ :: [] -> None
    | x :: xs ->
    (
        if x + n < m then match find_complement (n + x) m xs with
            | None -> find_complement_pair n m xs
            | Some y -> Some (x, y)
            else find_complement_pair n m xs
    )

let rec mul_w_match_num = function
    | [] -> None
    | x :: xs ->
        (
            match find_complement x 2020 xs with
            | None -> mul_w_match_num xs
            | Some y -> Some (x * y)
        )

let rec mul_w_match_pair = function
    | [] -> None
    | x :: xs ->
        (
            match find_complement_pair x 2020 xs with
            | None -> mul_w_match_pair xs
            | Some (y, z) -> Some (x * y * z)
        )

let str_of_int_opt = function
    | None -> ""
    | Some x -> string_of_int x

let day1pt1 vsebina_datoteke =
    vsebina_datoteke |> String.split_on_char '\n' |> List.filter (fun x -> (String.length x) > 0)
    |> List.map int_of_string |> mul_w_match_num |> str_of_int_opt

let day1pt2 vsebina_datoteke =
    vsebina_datoteke |> String.split_on_char '\n' |> List.filter (fun x -> (String.length x) > 0)
    |> List.map int_of_string |> mul_w_match_pair |> str_of_int_opt