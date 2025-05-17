open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

(** End of file *)

let solve () =
  let n = read_int () in
  let v = read_int_list () |> List.map ~f:(fun x -> x - 1) |> Array.of_list in
  let pair =
    let pair = Array.init n ~f:(fun _ -> None, None) in
    for i = 0 to (2 * n) - 1 do
      let upd =
        match pair.(v.(i)) with
        | None, None -> Some i, None
        | Some j, None -> Some j, Some i
        | _, Some _ -> assert false
      in
      pair.(v.(i)) <- upd
    done;
    pair |> Array.map ~f:(fun (x, y) -> Option.value_exn x, Option.value_exn y)
  in
  let candidates =
    let get_candidate x =
      let p1, p2 = pair.(x) in
      let get_color pos = if pos < 0 || pos >= 2 * n then None else Some v.(pos) in
      [ p1 - 1; p1 + 1; p2 - 1; p2 + 1 ]
      |> List.filter_map ~f:get_color
      |> List.dedup_and_sort ~compare
      |> List.map ~f:(fun y -> if y > x then x, y else y, x)
    in
    List.init n ~f:Fn.id
    |> List.concat_map ~f:get_candidate
    |> List.dedup_and_sort ~compare:[%compare: int * int]
  in
  let cnt =
    let test (x, y) =
      let x1, x2 = pair.(x) in
      let y1, y2 = pair.(y) in
      if x2 = x1 + 1 || y2 = y1 + 1
      then false
      else (
        let x1, x2, y1, y2 = [ x1; x2; y1; y2 ] |> List.sort ~compare |> to_4ple in
        if x2 = x1 + 1 && y2 = y1 + 1 then true else false)
    in
    List.count ~f:test candidates
  in
  printf "%d\n" cnt
;;

let () =
  let t = read_int () in
  for _ = 1 to t do
    solve ()
  done
;;
