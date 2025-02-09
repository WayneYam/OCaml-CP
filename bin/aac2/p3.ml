open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let reachable prefix_sum n m size =
  let get x y =
    prefix_sum.(x).(y)
    + prefix_sum.(x + size).(y + size)
    - prefix_sum.(x).(y + size)
    - prefix_sum.(x + size).(y)
  in
  let inbound (x, y) = n > x && x >= 0 && m > y && y >= 0 in
  let vst = Array.make_matrix ~dimx:n ~dimy:m false in
  let q = Queue.create () in
  let add (x, y) =
    if inbound (x, y) && (not vst.(x).(y)) && get x y = 0
    then (
      vst.(x).(y) <- true;
      Queue.enqueue q (x, y))
  in
  add (n - 1, 0);
  while not (Queue.is_empty q) do
    let x, y = Queue.dequeue_exn q in
    add (x + 1, y);
    add (x - 1, y);
    add (x, y + 1);
    add (x, y - 1)
  done;
  vst.(0).(m - 1)
;;

let rec binary_search ~f lo hi =
  if lo + 1 = hi
  then lo
  else (
    let mid = (lo + hi) / 2 in
    if f mid then binary_search ~f lo mid else binary_search ~f mid hi)
;;

let () =
  let n, m = read_int_list () |> to_2ple in
  let tbl = List.init n ~f:(fun _ -> read_line ()) in
  let prefix_sum = Array.make_matrix ~dimx:(n + 1) ~dimy:(m + 1) 0 in
  List.iteri tbl ~f:(fun x row ->
    String.iteri row ~f:(fun y c ->
      prefix_sum.(x + 1).(y + 1) <- (if Char.equal c 'X' then 1 else 0);
      prefix_sum.(x + 1).(y + 1) <- prefix_sum.(x + 1).(y + 1) + prefix_sum.(x + 1).(y);
      prefix_sum.(x + 1).(y + 1) <- prefix_sum.(x + 1).(y + 1) + prefix_sum.(x).(y + 1);
      prefix_sum.(x + 1).(y + 1) <- prefix_sum.(x + 1).(y + 1) - prefix_sum.(x).(y)));
  let test size = not (reachable prefix_sum (n - size + 1) (m - size + 1) size) in
  printf "%d\n" @@ binary_search ~f:test 0 (min n m + 1)
;;

(** End of file *)
