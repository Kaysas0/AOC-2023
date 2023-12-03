open Core

module Word_num = struct
  let number = function
    | "one"   | "eno"   -> '1'
    | "two"   | "owt"   -> '2'
    | "three" | "eerht" -> '3'
    | "four"  | "ruof"  -> '4'
    | "five"  | "evif"  -> '5'
    | "six"   | "xis"   -> '6'
    | "seven" | "neves" -> '7'
    | "eight" | "thgie" -> '8'
    | "nine"  | "enin"  -> '9'
    | _      -> '0'

  let substr str init len = 
    (try
      Some (Caml.String.sub str init len)  
    with _ -> None)
    |> Option.value ~default:"zero"

  let contains_string s2 s1 =
    let len1 = String.length s1 in
    let len2 = String.length s2 in
    let rec contains_aux s1 s2 i =
      if i > len2 then None else
        if String.( = ) (substr s2 i len1) s1 
        then Some (i, number (substr s2 i len1))
        else contains_aux s1 s2 (i + 1)
    in contains_aux s1 s2 0

  let filter lst str =
    List.map ~f:(contains_string str) lst
    |> List.filter ~f:Option.is_some
end

let num_lst = 
  [  "one"  ;  "two"
  ;  "three";  "four"
  ;  "five" ;  "six"
  ;  "seven";  "eight"
  ;  "nine" ]

let rev_num_lst =
  List.map num_lst ~f:String.rev

let trim lst =
  match 
    List.map lst 
      ~f:(function
          | Some x -> x 
          | None   -> (99, '0')) 
    |> List.sort ~compare:(fun (x, _) (y, _) -> Int.compare x y)
    |> List.hd_exn 
  with
  | (_, x) -> x

let find =
  String.findi ~f:(fun _ x -> Char.is_digit x)

let fst_last str = 
  let rts = String.rev str in
  String.of_char_list [
    trim (find str ::
    Word_num.filter num_lst str);
    trim (find rts ::
    Word_num.filter rev_num_lst rts)
  ]

let sum lst = 
  List.map ~f:fst_last lst
  |> List.fold ~init:0 ~f:(fun x y -> x + Int.of_string y)

(* let filename = "input.txt" *)
let filename1 = "test.txt"

let () =
  let file = In_channel.create filename1 in
  let lst  = In_channel.input_lines file in
  Printf.printf "\nSum: %i\n" (sum lst);
  In_channel.close file
