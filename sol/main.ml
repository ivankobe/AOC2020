#use "/home/ivan/AOC2020/sol/day_1.ml"
#use "/home/ivan/AOC2020/sol/day_2.ml"
#use "/home/ivan/AOC2020/sol/day_3.ml"
#use "/home/ivan/AOC2020/sol/day_4.ml"
#use "/home/ivan/AOC2020/sol/day_5.ml"
#use "/home/ivan/AOC2020/sol/day_6.ml"
#use "/home/ivan/AOC2020/sol/day_7.ml"
#use "/home/ivan/AOC2020/sol/day_8.ml"
#use "/home/ivan/AOC2020/sol/day_9.ml"

let print_prompt () = Printf.printf "Day to write >>> %!" 
let rec repl () = 
  print_prompt ();
  let input = Scanf.scanf "%s" (fun x -> x) in
  if input = "" then repl ()
  else input

let day = repl ()

let naloga1 day = match day with
  | "1" -> day1pt1
  | "2" -> day2pt1
  | "3" -> day3pt1
  | "4" -> day4pt1
  | "5" -> day5pt1
  | "6" -> day6pt1
  | "7" -> day7pt1
  | "8" -> day8pt1
  | "9" -> day9pt1
  | _ -> failwith "not solved yet"

  let naloga2 day = match day with
  | "1" -> day1pt2
  | "2" -> day2pt2
  | "3" -> day3pt2
  | "4" -> day4pt2
  | "5" -> day5pt2
  | "6" -> day6pt2
  | "7" -> day7pt2
  | "8" -> day8pt2
  | "9" -> day9pt2
  | _ -> failwith "not solved yet"
 
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
  let odgovor1 = naloga1 day vsebina_datoteke in
  let time_used1 = Unix.gettimeofday () -. time1 in

  let time2 = Unix.gettimeofday () in
  let odgovor2 = naloga2 day vsebina_datoteke in
  let time_used2 = Unix.gettimeofday () -. time2 in
  
  izpisi_datoteko ("/home/ivan/Faks/Prog1/AOC2020/out/day_" ^ day ^ "_1.out") (odgovor1 ^ " in " ^ (string_of_float time_used1) ^ "s");
  izpisi_datoteko ("/home/ivan/Faks/Prog1/AOC2020/out/day_" ^ day ^ "_2.out") (odgovor2 ^ " in " ^ (string_of_float time_used2) ^ "s")