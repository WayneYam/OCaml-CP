open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let s = 26

let () =
  let _ = read_int () in
  let parse_input () =
    read_line ()
    |> String.to_list
    |> List.map ~f:(fun x -> Char.to_int x - Char.to_int 'a')
  in
  let a = parse_input () in
  let b = parse_input () in
  let pairs = List.zip_exn a b |> List.dedup_and_sort ~compare:[%compare: int * int] in
  let edges = Array.init s ~f:(fun _ -> []) in
  List.iter pairs ~f:(fun (a, b) -> edges.(a) <- b :: edges.(a));
  if Array.exists edges ~f:(fun l -> List.length l > 1)
  then printf "-1\n"
  else (
    let edges = Array.map edges ~f:List.hd in
    let self_loops =
      Array.counti edges ~f:(fun i x ->
        match x with
        | Some x when x = i -> true
        | _ -> false)
    in
    ignore self_loops;
    let has_empty = Array.exists edges ~f:Option.is_none in
    let components =
      let dsu = Dsu.create 26 in
      Array.iteri edges ~f:(fun i x ->
        match x with
        | Some j -> Dsu.merge dsu i j |> ignore
        | _ -> ());
      Dsu.groups dsu |> List.filter ~f:(fun lst -> List.length lst > 1)
    in
    let count_edge lst =
      List.count lst ~f:(fun x ->
        match edges.(x) with
        | Some y when x <> y -> true
        | _ -> false)
    in
    let is_loop lst =
      let cnt = Array.init s ~f:(fun _ -> 0) in
      if List.exists lst ~f:(fun x -> Option.is_none edges.(x))
      then false
      else (
        List.iter lst ~f:(fun x ->
          match edges.(x) with
          | Some x -> cnt.(x) <- cnt.(x) + 1
          | None -> assert false);
        List.for_all lst ~f:(fun x -> cnt.(x) = 1))
    in
    let loops = List.map components ~f:is_loop in
    let num_edges = List.map components ~f:count_edge in
    let has_simple = List.exists loops ~f:not in
    let answer =
      if (not @@ List.is_empty components) && (not has_simple) && not has_empty
      then -1
      else
        List.map2_exn num_edges loops ~f:(fun sz loop -> sz + if loop then 1 else 0)
        |> List.sum (module Int) ~f:Fn.id
    in
    printf "%d\n" answer)
;;

(** End of file *)
