# Функциональное программирование
## Лабораторная работа 3

<b>Выполнил:</b> Ратушняк Евгений Алексеевич \
<b>Группа:</b> p34102 \
<b>Преподаватель:</b> Пенской Александр Владимирович

### Реализация
Функция читает из стандартного потока пока не будет пустая строка. 
В результате получаем список пар (x,y) 
```ocaml
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

```
Полученный список точек обрабатывается, параллельно вычисляются функции, Печатается результат.\ 
Ожидается ввод новой пары чисел и алгоритм повторяется.

`linear` - аппроксимация линейной функцией 
`logarithm` - аппроксимация логарифмической функцией
`segment` - аппроксимация отрезками

```ocaml
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

```

Функция `printData` выводит на экран результат аппроксимации.

```
$ cat test.csv | dune exec approx 5 5 1 2 3
x: 4.00000, y: 4.00000 | x: 4.00000, y: 4.03836 | x: 4.00000, y: 4.00000 |
x: 4.20000, y: 4.20000 | x: 4.20000, y: 4.15651 | x: 4.20000, y: 4.20000 |
x: 4.40000, y: 4.40000 | x: 4.40000, y: 4.26916 | x: 4.40000, y: 4.40000 |
x: 4.60000, y: 4.60000 | x: 4.60000, y: 4.37680 | x: 4.60000, y: 4.60000 |
x: 4.80000, y: 4.80000 | x: 4.80000, y: 4.47987 | x: 4.80000, y: 4.80000 |
x: 5.00000, y: 5.00000 | x: 5.00000, y: 5.23451 | x: 5.00000, y: 5.00000 |
x: 5.40000, y: 5.40000 | x: 5.40000, y: 5.53751 | x: 5.40000, y: 5.40000 |
x: 5.80000, y: 5.80000 | x: 5.80000, y: 5.81886 | x: 5.80000, y: 5.80000 |
x: 6.20000, y: 6.20000 | x: 6.20000, y: 6.08143 | x: 6.20000, y: 6.20000 |
x: 6.60000, y: 6.60000 | x: 6.60000, y: 6.32758 | x: 6.60000, y: 6.60000 |
x: 7.00000, y: 7.00000 | x: 7.00000, y: 7.40947 | x: 7.00000, y: 7.00000 |
x: 7.60000, y: 7.60000 | x: 7.60000, y: 7.88671 | x: 7.60000, y: 7.60000 |
x: 8.20000, y: 8.20000 | x: 8.20000, y: 8.32767 | x: 8.20000, y: 8.20000 |
x: 8.80000, y: 8.80000 | x: 8.80000, y: 8.73747 | x: 8.80000, y: 8.80000 |
x: 9.40000, y: 9.40000 | x: 9.40000, y: 9.12024 | x: 9.40000, y: 9.40000 |

```

### Выводы 
Я вспомнил методы аппроксимации и узнал о работе с потоками на Ocaml при помощи Lwt. Не очень удобно, если программа написана не полностью на данной библиотеке, а лишь небольшая ее часть.