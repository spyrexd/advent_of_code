open Base

let rec calculate_pass current_num pass input =
  let str = input ^ Int.to_string current_num in
  let digest_hex = Stdlib.Digest.string str |> Stdlib.Digest.to_hex in
  if String.equal (String.sub ~pos:0 ~len:5 digest_hex) "00000" then
    let pass = String.get digest_hex 5 :: pass in
    if List.length pass = 8 then String.of_char_list (List.rev pass)
    else calculate_pass (Int.succ current_num) pass input
  else calculate_pass (Int.succ current_num) pass input

let rec calculate_pass_2 current_num num_found pass input =
  let str = input ^ Int.to_string current_num in
  let digest_hex = Stdlib.Digest.string str |> Stdlib.Digest.to_hex in
  if String.equal (String.sub ~pos:0 ~len:5 digest_hex) "00000" then
    let pos = String.get digest_hex 5 |> Char.get_digit in
    match pos with
    | Some i when i < 8 ->
        if Char.equal ' ' (Array.get pass i) then
          let ch = String.get digest_hex 6 in
          let num_found = Int.succ num_found in
          let () = Array.set pass i ch in
          if num_found = 8 then String.of_array pass
          else calculate_pass_2 (Int.succ current_num) num_found pass input
        else calculate_pass_2 (Int.succ current_num) num_found pass input
    | _ -> calculate_pass_2 (Int.succ current_num) num_found pass input
  else calculate_pass_2 (Int.succ current_num) num_found pass input

let input = "abbhdwsy"
let () = Stdio.print_endline (calculate_pass 0 [] input)
let pass = Array.create ~len:8 ' '
let () = Stdio.print_endline (calculate_pass_2 0 0 pass input)
