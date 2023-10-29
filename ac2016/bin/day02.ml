open Base

let input =
  "UULLULLUULLLURDLDUURRDRRLDURDULLRURDUDULLLUULURURLRDRRRRULDRUULLLLUUDURDULDRRDRUDLRRLDLUDLDDRURURUURRRDDDLLRUDURDULUULLRRULLRULDUDRDRLDLURURUDDUDLURUDUDURLURURRURLUDDRURRDLUURLLRURRDUDLULULUDULDLLRRRDLRDLDUDRDDDRRUURRRRRUURRDRRDLURDRRURDLLUULULLRURDLDDDRRLLRRUURULURUUDDLRRUDDRURUUDLRLRDLRURRRDULLDLRUDDUULRDULURUURDULUDLLRRLDDLRDLRUDRLDDRLRRRDURDULLRRRDRRLUURURDRRDRRLDLUDURURLDUURDRUDRDDRLDRRLDLURURULLUURUDUUDLRLL\n\
   LLLULLULDDULRLLURLLLRUUDDLRUULRLULLDLLRRDRLRLRLLDRUUURULDRDDLUDLLDUDULLLRLULLLRULDRDRUDLLRLRLLUDULRRRLDRUULDDULLDULULLUDUDLDRDURDLDLLDUDRRRDLUURRUURULLURLDURLRRLLDDUUULDRLUUDUDLURLULUDURRDRLLDDDDDRRULLRLDULULDDRUURRDLUDDDUDURDDRDRULULLLLUURDURUUUULUDLRURRULRDDRURURLLRLUUDUUURDLLDDLUDRLLLUDLLLLULRLURDRRRDUUDLLDLDDDURRDDRURUURDDRURRLDDDURDLLUURUUULRLUURRUDRLLDLURDUDRLULDLRLULULUDDLRDUDRUDLUULUULDURDRRRRLRULLUDRDDRDLDUDRDRRLDLLLLUDDLRULDLLDDUULDDRRULRRUURUDRDURLLLDDUUDRUUDLULLDR\n\
   UDUUULLDDDDLUDLDULRLRDLULLDDRULDURRLURRUDLRRUDURRDUDRRRUULRLLRLUDLDRRDUURDDRDRDUUUDUDLDLLRRLUURLUUUDDDUURLULURRLURRRDRDURURUDRLRUURUDRUDDDRDRDLDRDURDLDRRDUUDLLURLDDURRRLULDRDRLLRLLLRURLDURDRLDRUURRLDLDRLDDDRLDLRLDURURLLLLDDRDUDLRULULLRDDLLUDRDRRLUUULDRLDURURDUDURLLDRRDUULDUUDLLDDRUUULRRULDDUDRDRLRULUUDUURULLDLLURLRRLDDDLLDRRDDRLDDLURRUDURULUDLLLDUDDLDLDLRUDUDRDUDDLDDLDULURDDUDRRUUURLDUURULLRLULUURLLLLDUUDURUUDUULULDRULRLRDULDLLURDLRUUUDDURLLLLDUDRLUUDUDRRURURRDRDDRULDLRLURDLLRRDRUUUURLDRURDUUDLDURUDDLRDDDDURRLRLUDRRDDURDDRLDDLLRR\n\
   ULDRUDURUDULLUDUDURLDLLRRULRRULRUDLULLLDRULLDURUULDDURDUUDLRDRUDUDDLDRDLUULRRDLRUULULUUUDUUDDRDRLLULLRRDLRRLUDRLULLUUUUURRDURLLRURRULLLRLURRULRDUURRLDDRRDRLULDDRRDRLULLRDLRRURUDURULRLUDRUDLUDDDUDUDDUDLLRDLLDRURULUDRLRRULRDDDDDRLDLRRLUUDLUURRDURRDLDLDUDRLULLULRLDRDUDLRULLULLRLDDRURLLLRLDDDLLLRURDDDLLUDLDLRLUULLLRULDRRDUDLRRDDULRLLDUURLLLLLDRULDRLLLUURDURRULURLDDLRRUDULUURRLULRDRDDLULULRRURLDLRRRUDURURDURDULURULLRLDD\n\
   DURLRRRDRULDLULUDULUURURRLULUDLURURDDURULLRRUUDLRURLDLRUDULDLLRRULLLLRRLRUULDLDLLRDUDLLRLULRLLUUULULRDLDLRRURLUDDRRLUUDDRRUDDRRURLRRULLDDULLLURRULUDLRRRURRULRLLLRULLRRURDRLURULLDULRLLLULLRLRLLLDRRRRDDDDDDULUUDUDULRURDRUDRLUULURDURLURRDRRRRDRRLLLLUDLRRDURURLLULUDDLRLRLRRUURLLURLDUULLRRDURRULRULURLLLRLUURRULLLURDDDRURDUDDULLRULUUUDDRURUUDUURURRDRURDUDRLLRRULURUDLDURLDLRRRRLLUURRLULDDDUUUURUULDLDRLDUDULDRRULDRDULURRUURDU"

type instruction = UP | DOWN | LEFT | RIGHT

let direction_of_char = function
  | 'U' -> UP
  | 'D' -> DOWN
  | 'L' -> LEFT
  | 'R' -> RIGHT
  | _ -> failwith "Invalid instruction"

type key =
  | Key1
  | Key2
  | Key3
  | Key4
  | Key5
  | Key6
  | Key7
  | Key8
  | Key9
  | KeyA
  | KeyB
  | KeyC
  | KeyD

type key_data = {
  num : string;
  up : key option;
  down : key option;
  left : key option;
  right : key option;
}

let key_data_of_key = function
  | Key1 ->
      { num = "1"; up = None; down = Some Key3; left = None; right = None }
  | Key2 ->
      { num = "2"; up = None; down = Some Key6; left = None; right = Some Key3 }
  | Key3 ->
      {
        num = "3";
        up = Some Key1;
        down = Some Key7;
        left = Some Key2;
        right = Some Key4;
      }
  | Key4 ->
      { num = "4"; up = None; down = Some Key8; left = Some Key3; right = None }
  | Key5 ->
      { num = "5"; up = None; down = None; left = None; right = Some Key6 }
  | Key6 ->
      {
        num = "6";
        up = Some Key2;
        down = Some KeyA;
        left = Some Key5;
        right = Some Key7;
      }
  | Key7 ->
      {
        num = "7";
        up = Some Key3;
        down = Some KeyB;
        left = Some Key6;
        right = Some Key8;
      }
  | Key8 ->
      {
        num = "8";
        up = Some Key4;
        down = Some KeyC;
        left = Some Key7;
        right = Some Key9;
      }
  | Key9 ->
      { num = "9"; up = None; down = None; left = Some Key8; right = None }
  | KeyA ->
      { num = "A"; up = Some Key6; down = None; left = None; right = Some KeyB }
  | KeyB ->
      {
        num = "B";
        up = Some Key7;
        down = Some KeyD;
        left = Some KeyA;
        right = Some KeyC;
      }
  | KeyC ->
      { num = "C"; up = Some Key8; down = None; left = Some KeyB; right = None }
  | KeyD ->
      { num = "D"; up = Some KeyB; down = None; left = None; right = None }

let rec key_of_directions key directions =
  let key_data = key_data_of_key key in
  match directions with
  | [] -> key
  | ch :: rest -> (
      let dir = direction_of_char ch in
      let next_key =
        match dir with
        | UP -> key_data.up
        | DOWN -> key_data.down
        | LEFT -> key_data.left
        | RIGHT -> key_data.right
      in
      match next_key with
      | None -> key_of_directions key rest
      | Some k -> key_of_directions k rest)

let directions = String.split_lines input

let rec code_of_directions code key = function
  | [] -> List.rev code
  | dir :: rest ->
      let key = key_of_directions key (String.to_list dir) in
      let key_data = key_data_of_key key in
      let code = key_data.num :: code in
      code_of_directions code key rest

let () =
  code_of_directions [] Key5 directions |> String.concat |> Stdio.print_endline
