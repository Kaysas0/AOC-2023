open Core

let red_allowed   = 12
let blue_allowed  = 14
let green_allowed = 13
let filename = "input"

let split_rounds game =
  String.split ~on:';' game
  |> List.map ~f:(String.split ~on:',')

let make_keys str = 
  let lst = 
    String.chop_prefix_exn ~prefix:" " str
    |> String.split ~on:' '
  in
  (Int.of_string (List.hd_exn lst), 
  List.rev lst |> List.hd_exn)

module Part1 = struct
  let game_id str = 
    String.chop_prefix_exn ~prefix:"Game " str
    |> Int.of_string

  let process_keys str =
    match (make_keys str) with
    | x, "green" -> x <= green_allowed
    | x, "red"   -> x <= red_allowed
    | x, "blue"  -> x <= blue_allowed
    | _ -> raise (Invalid_argument "Key unverifiable")

  let is_valid str = 
    split_rounds str
    |> List.map 
      ~f:(List.map ~f:(process_keys))
    |> List.join
    |> List.fold_left ~init:true 
      ~f:(fun acc x -> acc && x)

  let valid_games input = 
    List.map ~f:(String.split ~on:':') input
    |> List.map 
      ~f:(function
        | hd :: tl :: _ -> (game_id hd, is_valid tl)
        | [] | _ :: [] -> raise (Invalid_argument "List not given"))
    |> List.fold ~init:0 
      ~f:(fun acc -> function
        | x, true  -> acc + x
        | _, false -> acc)
end

module Part2 = struct
  let keying str = 
    List.rev str |> List.hd_exn |> split_rounds |>
    List.map ~f:(List.map ~f:make_keys) |> List.join

  let process_keys (green, red, blue) = function
    | x, "green" -> 
      if x > green
      then (x, red, blue)
      else (green, red, blue)
    | x, "red"   -> 
      if x > red 
      then (green, x, blue)
      else (green, red, blue)
    | x, "blue"  -> 
      if x > blue 
      then (green, red, x) 
      else (green, red, blue)
    | _ -> failwith "red"
  ;;

  let mult (green, red, blue) =
    green * red * blue

  let fewest_cubes input =
    List.map ~f:(String.split ~on:':') input
    |> List.map ~f:(keying)
    |> List.map ~f:(List.fold ~init:(0, 0, 0) ~f:(process_keys))
    |> List.fold ~init:0 
      ~f:(fun acc x -> acc + (mult x))
end

let () = 
  let file = In_channel.create filename in
  let lst  = In_channel.input_lines file in
  Printf.printf "Part1: %i\n" (Part1.valid_games lst);
  Printf.printf "Part2: %i\n" (Part2.fewest_cubes lst);
  In_channel.close file
