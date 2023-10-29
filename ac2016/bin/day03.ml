open Base

let input = In_channel.with_open_text "input/day03" In_channel.input_lines

let triangle_of_string tri_line =
  let split =
    String.strip tri_line |> String.split ~on:' '
    |> List.filter ~f:(fun x -> not (equal_string x ""))
  in
  match split with
  | x :: y :: z :: _ -> (Int.of_string x, Int.of_string y, Int.of_string z)
  | _ -> assert false

let rec process_colums col1 col2 col3 = function
  | [] -> col1 @ col2 @ col3
  | hd :: rest -> (
      match hd with
      | x, y, z -> process_colums (x :: col1) (y :: col2) (z :: col3) rest)

let rec valid_triangle valid_tris = function
  | [] -> valid_tris
  | x :: y :: z :: rest ->
      if x + y > z && x + z > y && z + y > x then
        let tris = true :: valid_tris in
        valid_triangle tris rest
      else valid_triangle valid_tris rest
  | _ -> assert false

let () =
  List.map ~f:triangle_of_string input
  |> process_colums [] [] [] |> valid_triangle [] |> List.length
  |> Int.to_string |> Stdio.print_endline
