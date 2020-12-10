#load "unix.cma"

let day = "2"

let check_password_1 char str min max =
    let num = (List.length (String.split_on_char char str)) - 1 in (num >= min && num <= max)

let check_password_2 char str x y =
    let p = str.[x - 1] = char || str.[y - 1] = char in
    let q = str.[x - 1] = char && str.[y - 1] = char in
    p && (not q)

let format str =
    let finalise = function
    | [x;y;z] -> (
        let a = (fun li -> match li with | [w;z] -> (w,z) | _ -> failwith "Incorrect argument")
                (List.map int_of_string (String.split_on_char '-' x)) in
        let b = String.sub y 0 1 in
        (a, b, z))
    | _ -> failwith "Incorrect argument" in
    str |> String.split_on_char '\n' |> List.map (String.split_on_char ' ') |> List.map finalise

let prepare_to_print l =    
    l |> List.filter (fun x -> x) |> List.length |> string_of_int

let pt1 vsebina =
    vsebina |> format |> List.map (fun ((min,max),substr,pass) ->  check_password_1 substr.[0] pass min max) |> prepare_to_print
    
let pt2 vsebina =
    vsebina |> format |> List.map (fun ((x,y),substr,pass) ->  check_password_2 substr.[0] pass x y) |> prepare_to_print

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