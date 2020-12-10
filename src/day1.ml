#load "unix.cma"

let day = "1"

let add x y = x + y

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

let pt1 vsebina_datoteke =
    vsebina_datoteke |> String.split_on_char '\n' |> List.filter (fun x -> (String.length x) > 0)
    |> List.map int_of_string |> mul_w_match_num |> str_of_int_opt

let pt2 vsebina_datoteke =
    vsebina_datoteke |> String.split_on_char '\n' |> List.filter (fun x -> (String.length x) > 0)
    |> List.map int_of_string |> mul_w_match_pair |> str_of_int_opt

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