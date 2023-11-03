[@@@ocaml.warning "-32"]

open Base

module Processor = struct
  type t = {
    input : string;
    position : int;
    ch : char option;
    map : (char * int) list;
    secid : int;
    checksum : string;
  }
  [@@deriving show]

  let init input =
    if String.is_empty input then
      { input; position = 0; ch = None; map = []; secid = 0; checksum = "" }
    else
      {
        input;
        position = 0;
        ch = Some (String.get input 0);
        map = [];
        secid = 0;
        checksum = "";
      }

  let rec parse_room processor =
    match processor.ch with
    | None -> processor
    | Some ch ->
        let processor =
          match ch with
          | '-' -> advance processor
          | ch when Char.is_alpha ch -> update processor ch
          | ch when Char.is_digit ch -> read_section_id processor
          | '[' -> read_checksum processor
          | ']' -> advance processor
          | _ -> assert false
        in
        processor

  and parse_encrypted processor =
    let processor = { processor with position = 0; ch = Some 'a' } in
    let processor, str =
      read_while processor (fun x -> not (Char.is_digit x))
    in
    (str, processor.secid)

  and advance ?(step = 1) processor =
    if processor.position >= String.length processor.input - 1 then
      { processor with ch = None }
    else
      let position = processor.position + step in
      {
        processor with
        position;
        ch = Some (String.get processor.input position);
      }

  and update processor ch =
    let map = processor.map in
    let count = List.Assoc.find ~equal:(fun x y -> Char.equal x y) map ch in
    let processor =
      match count with
      | None -> { processor with map = (ch, 0) :: map }
      | Some i -> { processor with map = (ch, Int.succ i) :: map }
    in
    advance processor

  and seek processor condition =
    let rec loop processor =
      if condition processor.ch then loop @@ advance processor else processor
    in
    let processor = loop processor in
    (processor, processor.position)

  and read_while processor condition =
    let pos_start = processor.position in
    let processor, pos_end =
      seek processor (fun ch ->
          match ch with Some character -> condition character | None -> false)
    in
    ( processor,
      String.sub processor.input ~pos:pos_start ~len:(pos_end - pos_start) )

  and read_section_id processor =
    let processor, int = read_while processor Char.is_digit in
    { processor with secid = Int.of_string int }

  and read_checksum processor =
    let processor = advance processor in
    let processor, checksum = read_while processor Char.is_alpha in
    { processor with checksum }

  let sort_map processor =
    let freq_compare x y =
      if snd x = snd y then Stdlib.compare (fst x) (fst y)
      else if snd x < snd y then 1
      else -1
    in
    let rec values_by_keys map values = function
      | [] -> values
      | hd :: tl ->
          let v =
            List.Assoc.find_exn ~equal:(fun x y -> Char.equal x y) map hd
          in
          values_by_keys map ((hd, v) :: values) tl
    in
    let keys =
      processor.map |> List.map ~f:fst |> Stdlib.List.sort_uniq Stdlib.compare
    in
    values_by_keys processor.map [] keys |> List.sort ~compare:freq_compare

  let check_checksum processor =
    let sorted_map = sort_map processor in
    let keys = List.map ~f:fst sorted_map in
    let posible_room = String.of_char_list keys |> String.sub ~pos:0 ~len:5 in
    (String.( = ) posible_room processor.checksum, processor)

  let shift_char num ch =
    let ch = Char.to_int ch in
    if ch = 45 then ' '
    else
      let new_ch = ch + num in
      if new_ch <= 122 then Char.of_int_exn new_ch
      else
        let normalized_ch = ch - 97 in
        Char.of_int_exn (((normalized_ch + (num % 26)) % 26) + 97)

  let decrypt_room (encrypted_room, secid) =
    let rec decrypt result = function
      | [] -> String.of_char_list (List.rev result)
      | hd :: tl -> decrypt (shift_char secid hd :: result) tl
    in
    let decrypted_room = decrypt [] (String.to_list encrypted_room) in
    let () =
      Stdio.print_endline (Printf.sprintf "%s %d" decrypted_room secid)
    in
    (decrypted_room, secid)
end

let rec run_processor (processor : Processor.t) =
  match processor.ch with
  | None -> processor
  | Some _ -> run_processor (Processor.parse_room processor)

let input = In_channel.with_open_text "input/day04" In_channel.input_lines
let initialzed = List.map ~f:(fun x -> Processor.init x) input

let valid_room_total =
  List.map ~f:(fun x -> run_processor x) initialzed
  |> List.map ~f:Processor.check_checksum
  |> List.filter ~f:fst
  |> List.fold ~init:0 ~f:(fun acc (x : bool * Processor.t) ->
         acc + (snd x).secid)

let _ =
  List.map ~f:(fun x -> run_processor x) initialzed
  |> List.map ~f:Processor.parse_encrypted
  |> List.map ~f:Processor.decrypt_room

let () = Stdio.print_endline (Int.to_string valid_room_total)
