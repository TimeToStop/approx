open! Approx

let rec truncate k l =
  match (k, l) with
  | _, [] -> []
  | 0, _ -> []
  | k, head :: tail -> head :: truncate (k - 1) tail

let rec read k pList =
  let input = read_line () in
  if input <> "" then
    let data =
      String.split_on_char ';' input |> List.filter (fun x -> x <> "")
    in
    if List.length data >= 2 then
      let x = float_of_string (List.nth data 0) in
      let y = float_of_string (List.nth data 1) in
      match pList with
      | [] -> [(x, y)]
      | _ -> (x, y) :: truncate (k - 1) pList
    else read k pList
  else pList

let rec approximationImpl n k ids points =
  let added = read k points in
  match added with
  | arr when List.length arr = k ->
      let funcsArr =
        Array.map
          (fun id -> Lwt.return (Approx.getFunc added id))
          (Array.of_list ids)
        |> Array.to_list
        |> Lwt_list.map_p (fun p ->
               Lwt.bind p (fun result -> Lwt.return result) )
        |> Lwt_main.run
      in
      let pointGen = Approx.genPoint n added in
      Approx.printData (Array.of_list funcsArr) pointGen n ;
      approximationImpl n k ids added
  | _ -> approximationImpl n k ids added

let approximate n k ids = approximationImpl n k ids []

let parse_ids = function
  | _ :: _ :: _ :: rest -> List.map int_of_string rest
  | _ -> []

let () =
  match Array.length Sys.argv with
  | n when n < 4 ->
      print_string
        "usage: n k <list id of ids>\n\
         ids:\n\
         1 -> segment\n\
         2 -> logarifm\n\
         3 -> linear"
  | _ ->
      approximate
        (int_of_string Sys.argv.(1))
        (int_of_string Sys.argv.(2))
        (parse_ids (Array.to_list Sys.argv))
