#use "/home/ivan/.ocamlinit"
#use "topfind"
#use "/home/ivan/Faks/Programinanje1/AOC2020/sol/day_1.ml"
let day = "8"

type instr = Nop | Jmp | Acc

let change = function
  | Nop -> Jmp
  | Jmp -> Nop
  | Acc -> failwith "incorrect argument" 

(* keys are line nums (starting wit 1), values are instructions *)
type tape = (int , instr * int) Hashtbl.t

(* the type of modyfied tapes. it contains an additional
(optional) integer specifying the index of the instruction changed *)
type mod_tape = int option * tape

module IntSet = Set.Make( 
  struct
    let compare = Stdlib.compare
    type t = int
  end )

(* pt. 1 *)
let read_tape (tape : tape) =

  let rec read (acc : int) visited tape (start : int) =
    if IntSet.mem start visited then acc
    else
      let visited' = IntSet.add start visited in
      match Hashtbl.find tape start with
      | (Nop, _) ->
        read acc visited' tape (start + 1)
      | (Jmp, num) ->
        read acc visited' tape (start + num)
      | (Acc, num) ->
        read (acc + num) visited' tape (start + 1)
      in
      read 0 IntSet.empty tape 1

(* pt. 2 *)
let read_tape' (tape : tape) : int =

  (* returns index of next jmp or nop *)
  let rec find_next start (tape : tape) =
    let start' = start + 1 in
    match Hashtbl.find_opt tape start' with
    | None -> failwith "no more tapes"
    | Some (instr, num) ->
      if instr = Acc then find_next start' tape else start'
    in
  
  (* returns the next tape *)
  let next_tape (tape : mod_tape) =
    let tape' = snd tape in
    let num = match fst tape with None -> 0 | Some x -> x in
    
      (* change an instruction back to original if necessary*)
      (if num > 0 then
      let (instr, num') = Hashtbl.find tape' num in
      Hashtbl.replace tape' num ((change instr), num'));

      (* change the next instruction *)
      let next = find_next num tape' in
      let (instr', num'') = Hashtbl.find tape' next in
      Hashtbl.replace tape' next ((change instr'), num'');
      (Some next, tape')
    in

  (* checks a single tape *)
  let rec read acc visited tape start =
    (* if we visit the same line twice, this isn't the right tape *)
    if IntSet.mem start visited then None
    (* if we fall of the grid, this is it *)
    else if Hashtbl.find_opt tape start = None then Some acc
    else
      let visited' = IntSet.add start visited in
      match Hashtbl.find tape start with
      | (Nop, _) ->
        read acc visited' tape (start + 1)
      | (Jmp, num) ->
        read acc visited' tape (start + num)
      | (Acc, num) ->
        read (acc + num) visited' tape (start + 1)
      in
      
  (* checks all posible tapes *)
  let rec iterate_on_tapes ((index, tape) : mod_tape) =
    match read 0 IntSet.empty tape 1 with
    | Some x -> x
    | None ->
      let tape' = next_tape (index, tape) in
      iterate_on_tapes tape'
    in

    iterate_on_tapes (None, tape)
  

let parse input : tape =

  let rec parser hashtbl count = function
  | [] -> hashtbl
  | [x;y] :: xs ->
    let num = int_of_string y in
    let new' = if x = "acc" then (Acc, num)
    else if x = "jmp" then (Jmp, num)
    else if x = "nop" then (Nop, num)
    else failwith "incorrect input" in
    let count' = count + 1 in
    Hashtbl.add hashtbl count' new';
    parser hashtbl count' xs
  | _ -> failwith "incorrect input"
  in

  input |> String.split_on_char '\n' |> List.map (String.split_on_char ' ')
  |> parser (Hashtbl.create 1000) 0 

let naloga1 input = input |> parse |> read_tape |> string_of_int

let naloga2 input = input |> parse |> read_tape' |> string_of_int