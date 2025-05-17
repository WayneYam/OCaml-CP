open! Base
open! Core

let directed_graph n (edges : (int * int) list) =
  let adj = Array.init n ~f:(fun _ -> []) in
  List.iter edges ~f:(fun (x, y) -> adj.(x) <- y :: adj.(x));
  adj
;;

let undirected_graph n (edges : (int * int) list) =
  let adj = Array.init n ~f:(fun _ -> []) in
  List.iter edges ~f:(fun (x, y) ->
    adj.(x) <- y :: adj.(x);
    adj.(y) <- x :: adj.(y));
  adj
;;
