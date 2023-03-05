open Printf

let rec generate_list_impl size = function
  | index when index == size -> []
  | i -> i :: generate_list_impl size (i + 1)

let generate_list = function 0 -> [] | n -> generate_list_impl n 0

let linear points =
  let sx = List.fold_left (fun state (x, _) -> state +. x) 0. points in
  let sxx =
    List.fold_left (fun state (x, _) -> state +. (x *. x)) 0. points
  in
  let sy = List.fold_left (fun state (_, y) -> state +. y) 0. points in
  let sxy =
    List.fold_left (fun state (x, y) -> state +. (x *. y)) 0. points
  in
  let n = List.length points in
  let a =
    ((sxy *. float_of_int n) -. (sx *. sy))
    /. ((sxx *. float_of_int n) -. (sx *. sx))
  in
  let b =
    ((sxx *. sy) -. (sx *. sxy)) /. ((sxx *. float_of_int n) -. (sx *. sx))
  in
  let f x = (a *. x) +. b in
  f

let segment points =
  let rec findBottomBorder i v =
    if i < List.length points then
      let x, _ = List.nth points i in
      if x < v then i else findBottomBorder (i + 1) v
    else -1
  in
  let rec findTopBorder i v =
    if i >= 0 then
      let x, _ = List.nth points i in
      if x >= v then i else findTopBorder (i - 1) v
    else -1
  in
  let f x =
    let top = findTopBorder (List.length points - 1) x in
    let bottom = findBottomBorder 0 x in
    if top = -1 then
      let _, yi = List.nth points 0 in
      yi
    else if bottom = -1 then
      let _, yi = List.nth points (List.length points - 1) in
      yi
    else
      let xi, yi = List.nth points top in
      let xiPrev, yiPrev = List.nth points bottom in
      let a = (yi -. yiPrev) /. (xi -. xiPrev) in
      let b = yi -. (a *. xi) in
      (a *. x) +. b
  in
  f

let logarifm points =
  let sx = List.fold_left (fun state (x, _) -> state +. log x) 0. points in
  let sxx =
    List.fold_left (fun state (x, _) -> state +. (log x *. log x)) 0. points
  in
  let sy = List.fold_left (fun state (_, y) -> state +. y) 0. points in
  let sxy =
    List.fold_left (fun state (x, y) -> state +. (log x *. y)) 0. points
  in
  let n = List.length points in
  let delta = (sxx *. float_of_int n) -. (sx *. sx) in
  let delta1 = (sxy *. float_of_int n) -. (sx *. sy) in
  let delta2 = (sxx *. sy) -. (sx *. sxy) in
  let a = delta1 /. delta in
  let b = delta2 /. delta in
  let f x = (a *. log x) +. b in
  f

let getFunc points = function
  | 1 -> segment points
  | 2 -> logarifm points
  | 3 -> linear points
  | _ -> failwith "invalid points"

let genPoint n points =
  match points with
  | (x2, _) :: (x1, _) :: _ ->
      let mult = (x2 -. x1) /. float_of_int n in
      let getPoint i = x1 +. (float_of_int i *. mult) in
      getPoint
  | _ -> fun _ -> 0.

let printData funcs pointGen count =
  generate_list count
  |> List.map (fun i ->
         funcs
         |> Array.map (fun f ->
                print_string "x: " ;
                printf "%.5f" (pointGen i) ;
                print_string ", y: " ;
                printf "%.5f" (f (pointGen i)) ;
                print_string " | " )
         |> ignore ;
         print_newline () )
  |> ignore
