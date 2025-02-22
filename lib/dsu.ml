open! Base
open! Core
open! Stdio

let par = fst
let sz = snd

type t = (int * int) array

let create n : t = Array.init n ~f:(fun id -> id, 1)

let rec leader (dsu : t) x =
  if par dsu.(x) = x
  then x
  else (
    let l = leader dsu @@ par dsu.(x) in
    dsu.(x) <- l, sz dsu.(x);
    l)
;;

let merge (dsu : t) x y =
  let lx = leader dsu x in
  let ly = leader dsu y in
  if lx = ly
  then lx
  else (
    let sx = sz dsu.(lx) in
    let sy = sz dsu.(ly) in
    if sx < sy
    then (
      dsu.(ly) <- ly, sx + sy;
      dsu.(lx) <- ly, sx;
      ly)
    else (
      dsu.(lx) <- lx, sx + sy;
      dsu.(ly) <- lx, sy;
      lx))
;;

let same (dsu : t) x y =
  let lx = leader dsu x in
  let ly = leader dsu y in
  lx = ly
;;

let size (dsu : t) x = sz dsu.(leader dsu x)

let groups (dsu : t) =
  Array.iteri dsu ~f:(fun id (_, sz) -> dsu.(id) <- leader dsu id, sz);
  dsu
  |> Array.to_list
  |> List.mapi ~f:(fun id (le, _) -> id, le)
  |> List.sort_and_group ~compare:(fun (_, l1) (_, l2) -> compare l1 l2)
  |> List.map ~f:(List.map ~f:fst)
;;
