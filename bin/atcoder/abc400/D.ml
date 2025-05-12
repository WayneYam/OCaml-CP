open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let () =
  let h, w = read_int_list () |> to_2ple in
  let board = Array.init h ~f:(fun _ -> read_line ()) in
  let sx, sy, tx, ty = read_int_list () |> List.map ~f:(fun x -> x - 1) |> to_4ple in
  let dist = Array.init h ~f:(fun _ -> Array.init w ~f:(fun _ -> 1000000000)) in
  let process l0 l1 x y depth =
    let is_inbound x y = 0 <= x && x < h && 0 <= y && y < w in
    let is_wall x y = Char.equal board.(x).[y] '#' in
    let dir = [ 1, 0; 0, 1; -1, 0; 0, -1 ] in
    List.fold dir ~init:(l0, l1) ~f:(fun (l0, l1) (dx, dy) ->
      let rec go (l0, l1) d has_wall =
        let x' = x + (d * dx) in
        let y' = y + (d * dy) in
        if d > 2 || not (is_inbound x' y')
        then l0, l1
        else (
          let has_wall = has_wall || is_wall x' y' in
          let depth' = depth + if has_wall then 1 else 0 in
          let l0, l1 =
            if dist.(x').(y') > depth'
            then (
              dist.(x').(y') <- depth';
              if has_wall then l0, (x', y') :: l1 else (x', y') :: l0, l1)
            else l0, l1
          in
          go (l0, l1) (d + 1) has_wall)
      in
      go (l0, l1) 1 false)
  in
  let rec bfs l0 l1 depth =
    match l0 with
    | [] -> bfs l1 l0 (depth + 1)
    | (x, y) :: tl ->
      (* printf "%d %d\n" x y; *)
      (* print_s @@ [%sexp_of: (int * int) list] l0; *)
      (* print_s @@ [%sexp_of: (int * int) list] l1; *)
      if x = tx && y = ty
      then ()
      else (
        let l0, l1 = process tl l1 x y depth in
        bfs l0 l1 depth)
  in
  bfs [ sx, sy ] [] 0;
  printf "%d\n" dist.(tx).(ty)
;;

(** End of file *)
