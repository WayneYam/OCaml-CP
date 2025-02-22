open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let () =
  let n, m = read_int_list () |> to_2ple in
  let dsu = Dsu.create (n + 1) in
  let edges =
    List.init m ~f:(fun id ->
      let x, y = read_int_list () |> to_2ple in
      x, y, m - id)
  in
  let edges =
    List.filter
      ~f:(fun (x, y, _) ->
        if Dsu.same dsu x y
        then true
        else (
          Dsu.merge dsu x y |> ignore;
          false))
      edges
    |> Fn.flip List.drop (m - n + 1)
  in
  let groups =
    Dsu.groups dsu
    |> List.filter_map ~f:(fun lst ->
      let hd = List.hd_exn lst in
      if hd = 0 then None else Some hd)
  in
  printf "%d\n" @@ List.length edges;
  let process_edge groups (x, y, id) =
    match groups with
    | h1 :: h2 :: tl ->
      if not @@ Dsu.same dsu h1 x
      then (
        Dsu.merge dsu h1 x |> ignore;
        printf "%d %d %d\n" id y h1;
        h2 :: tl)
      else if not @@ Dsu.same dsu h2 x
      then (
        Dsu.merge dsu h2 x |> ignore;
        printf "%d %d %d\n" id y h2;
        h1 :: tl)
      else assert false
    | _ -> assert false
  in
  List.fold ~init:groups ~f:(fun groups edge -> process_edge groups edge) edges |> ignore
;;

(** End of file *)
