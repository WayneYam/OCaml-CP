open! Base
open! Core
open! Stdio

(** Definitions of modules here *)

open! Lib
open! Input

let solve n k =
  if n < k then 1
  else
    let q = Queue.init k ~f:(fun _ -> 1) in
    let sum = ref k in
    let m = 1000000000 in
    for _ = 1 to n - k do
      Queue.enqueue q !sum;
      sum := !sum * 2;
      sum := !sum - Queue.dequeue_exn q;
      sum := ((!sum % m) + m) % m
    done;
    !sum

let () =
  let n, k = read_int_list () |> to_2ple in
  printf "%d" @@ solve n k

(** End of file *)
