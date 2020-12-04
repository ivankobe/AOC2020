let day = "4"

let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let input = preberi_datoteko ("/home/ivan/Faks/Prog1/AOC2020/in/day_" ^ day ^ ".in")




















(* let output1 = naloga1 input *)
(* let output2 = naloga2 input *)