let line_length = String.length ".#......##..#.....#....#.#.#..."

(* Picks the right character from n-th line *)
let pick_from_line line num_steps n =
    let index = (n * num_steps) mod line_length in line.[index]

(* Searches for trees in a list of lines *)
let pick_from_each_line num_steps_x num_steps_y list =
    (* Indexes elements of a given list *)
    let rec index acc n = function
    | [] -> acc
    | x :: xs -> index (acc @ [(x, n)]) (n + 1) xs in
    let rec aux acc count = function
    | [] -> acc
    | (x, n) :: xs ->
        if ((n mod num_steps_y) = 0)
        then aux (acc @ [pick_from_line x num_steps_x count]) (count + 1) xs
        else aux acc count xs
    in
    aux [] 0 (index [] 0 list)

let trees x y v =
    v |> String.split_on_char '\n' |> pick_from_each_line x y
    |> List.tl |> List.filter (Char.equal '#') |> List.length

let day3pt1 v =
    v |> trees 3 1 |> string_of_int

let day3pt2 v =
    ((trees 1 1 v) * (trees 3 1 v) * (trees 5 1 v) * (trees 7 1 v) * (trees 1 2 v)) |> string_of_int