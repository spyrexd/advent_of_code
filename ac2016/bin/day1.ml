open Base

let input =
  "R4, R4, L1, R3, L5, R2, R5, R1, L4, R3, L5, R2, L3, L4, L3, R1, R5, R1, L3, \
   L1, R3, L1, R2, R2, L2, R5, L3, L4, R4, R4, R2, L4, L1, R5, L1, L4, R4, L1, \
   R1, L2, R5, L2, L3, R2, R1, L194, R2, L4, R49, R1, R3, L5, L4, L1, R4, R2, \
   R1, L5, R3, L5, L4, R4, R4, L2, L3, R78, L5, R4, R191, R4, R3, R1, L2, R1, \
   R3, L1, R3, R4, R2, L2, R1, R4, L5, R2, L2, L4, L2, R1, R2, L3, R5, R2, L3, \
   L3, R3, L1, L1, R5, L4, L4, L2, R5, R1, R4, L3, L5, L4, R5, L4, R5, R4, L3, \
   L2, L5, R4, R3, L3, R1, L5, R5, R1, L3, R2, L5, R5, L3, R1, R4, L5, R4, R2, \
   R3, L4, L5, R3, R4, L5, L5, R4, L4, L4, R1, R5, R3, L1, L4, L3, L4, R1, L5, \
   L1, R2, R2, R4, R4, L5, R4, R1, L1, L1, L3, L5, L2, R4, L3, L5, L4, L1, R3"

type direction = Right of int | Left of int

let string_of_direction = function
  | Right i -> "Right " ^ Int.to_string i
  | Left i -> "Left " ^ Int.to_string i

type orintation = North | East | South | West

let string_of_orintation = function
  | North -> "North"
  | East -> "East"
  | South -> "South"
  | West -> "West"

let orintation_by_direction facing dir =
  match facing with
  | North -> ( match dir with Right _ -> East | Left _ -> West)
  | East -> ( match dir with Right _ -> South | Left _ -> North)
  | South -> ( match dir with Right _ -> West | Left _ -> East)
  | West -> ( match dir with Right _ -> North | Left _ -> South)

let process_move facing (x, y) = function
  | Right i -> (
      match facing with
      | North -> (x + i, y)
      | East -> (x, y - i)
      | South -> (x - i, y)
      | West -> (x, y + i))
  | Left i -> (
      match facing with
      | North -> (x - i, y)
      | East -> (x, y + i)
      | South -> (x + i, y)
      | West -> (x, y - i))

let rec parse_direction = function
  | [] -> []
  | dir :: rest -> (
      let d = dir.[0] in
      let n = List.nth (String.split_on_chars ~on:[ 'R'; 'L' ] dir) 1 in
      match n with
      | None -> assert false
      | Some n -> (
          let i = Int.of_string n in
          match d with
          | 'R' -> Right i :: parse_direction rest
          | 'L' -> Left i :: parse_direction rest
          | _ -> assert false))

let calc_distance (x, y) = Int.abs (0 + x) + Int.abs (0 + y)

let calc_postions a b =
  let rec range i j = function
    | "add" -> if i > j then [] else i :: range (i + 1) j "add"
    | "sub" -> if i < j then [] else i :: range (i - 1) j "sub"
    | _ -> assert false
  in
  if fst a = fst b then
    let rng =
      match snd a < snd b with
      | true -> range (snd a) (snd b) "add"
      | false -> range (snd a) (snd b) "sub"
    in
    let rec pos x = function [] -> [] | hd :: tl -> (x, hd) :: pos x tl in
    pos (fst a) rng
  else
    let rng =
      match fst a < fst b with
      | true -> range (fst a) (fst b) "add"
      | false -> range (fst a) (fst b) "sub"
    in
    let rec pos y = function [] -> [] | hd :: tl -> (hd, y) :: pos y tl in
    pos (snd a) rng

let rec move facing (x, y) visited = function
  | [] ->
      Stdlib.print_endline
        (Printf.sprintf "Pos: (%d, %d) Facing: %s Next Dir: None D: %d" x y
           (string_of_orintation facing)
           (calc_distance (x, y)));
      (x, y) :: visited
  | dir :: rest ->
      Stdlib.print_endline
        (Printf.sprintf "Pos: (%d, %d) Facing: %s Next Dir: %s D: %d" x y
           (string_of_orintation facing)
           (string_of_direction dir)
           (calc_distance (x, y)));
      let v = (x, y) :: visited in
      move
        (orintation_by_direction facing dir)
        (process_move facing (x, y) dir)
        v rest

let dirs =
  parse_direction
    (String.split ~on:' '
       (String.substr_replace_all ~pattern:", " ~with_:" " input))

let moves = move North (0, 0) [] dirs

let rec expanded_moves = function
  | e1 :: e2 :: rest -> calc_postions e1 e2 :: expanded_moves (e2 :: rest)
  | _ -> []

let e_moves = List.rev moves |> expanded_moves

let rec dup = function
  | [] -> (0, 0)
  | pos :: tl -> (
      match Stdlib.List.mem pos tl with true -> pos | false -> dup tl)

let a =
  Stdlib.List.flatten e_moves
  |> List.remove_consecutive_duplicates ~equal:(fun x y ->
         fst x = fst y && snd x = snd y)
  |> dup

let _ = Printf.sprintf "(%d, %d)" (fst a) (snd a) |> Stdlib.print_endline
let () = calc_distance (fst a, snd a) |> Int.to_string |> Stdlib.print_endline
