open Core

let filename = "input"

let make_int str = 
  String.split ~on:' ' str
  |> List.filter ~f:(String.( <> ) "")
  |> List.map ~f:Int.of_string

let split game = 
  String.split_on_chars ~on:['|'; ':'] game
  |> function
    | _ :: winning :: choosen :: [] -> 
      (make_int winning, make_int choosen)
    | _  -> failwith "not possible"

let list_equality (winning, chosen) =
  List.filter_map 
    ~f:(fun x -> 
      if List.exists ~f:(fun y -> x = y) chosen 
      then Some x else None)
  winning

let length card = 
  split card
  |> list_equality
  |> List.length
  |> function
  | 0 -> 0
  | x -> Int.pow 2 (x - 1)
  
let start game =
  List.map game ~f:(length)
  |> List.fold_left ~init:0 ~f:( + )

let _ = 
  let file = In_channel.create filename in
  let lst  = In_channel.input_lines file in
  let _ = In_channel.close file in
  Printf.printf "Points total: %i\n" (start lst);

