#use "topfind"
#require "str"


let split_on_newline s = Str.split (Str.regexp "\n") s

(* Adapted from https://stackoverflow.com/questions/39813584/how-to-split-on-whitespaces-in-ocaml *)


let char_list_to_int_list l =
    let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux ((int_of_string x) :: acc) xs
    in
    aux [] l


let rec find_complement n m = function
    | [] -> None
    | x :: xs -> if x = (m - n) then Some x else find_complement n m xs


let rec find_match = function
    | [] -> None
    | x :: xs ->
        (
            match find_complement x 2020 xs with
            | None -> find_match xs
            | Some y -> Some (x * y)
        )


let str_of_int_opt = function
    | None -> "bla"
    | Some x -> string_of_int x


let naloga1 vsebina_datoteke =
    vsebina_datoteke |> split_on_newline |> char_list_to_int_list |> find_match |> str_of_int_opt




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
    let vsebina_datoteke = preberi_datoteko "/home/ivan/Faks/Prog1/AOC2020/day_1_1/day_1_1.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    in
    izpisi_datoteko "/home/ivan/Faks/Prog1/AOC2020/day_1_1/day_1_1.out" odgovor1;





