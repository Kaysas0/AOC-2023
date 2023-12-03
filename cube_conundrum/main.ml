open Core

let red_allowed   = 12
let blue_allowed  = 14
let green_allowed = 13
let filename = "input"

let game_id str = 
  String.chop_prefix_exn ~prefix:"Game " str
  |> Int.of_string

let make_key str =
  let lst = String.chop_prefix_exn ~prefix:" " str |> String.split ~on:' ' in
  match Int.of_string (List.hd_exn lst), List.hd_exn (List.tl_exn lst) with
  | x, "green" -> x <= green_allowed
  | x, "red"   -> x <= red_allowed
  | x, "blue"  -> x <= blue_allowed
  | _          -> raise (Invalid_argument "Key unverifiable")

let is_valid str = 
  List.hd_exn str
  |> String.split ~on:';'
  |> List.map ~f:(String.split ~on:',')
  |> List.map ~f:(List.map ~f:(make_key))
  |> List.join
  |> List.fold_left ~init:true ~f:(fun acc x -> acc && x)

let start input = 
  List.map ~f:(String.split ~on:':') input
  |> List.map 
    ~f:(function
      | hd :: tl -> (game_id hd, is_valid tl)
      | [] -> raise (Invalid_argument "List not given"))
  |> List.fold ~init:0 
    ~f:(fun acc -> function
      | x, true  -> acc + x
      | _, false -> acc)

let () = 
  let file = In_channel.create filename in
  let lst  = In_channel.input_lines file in
  Printf.printf "Sum of valid game ids: %i\n" (start lst);
  In_channel.close file
