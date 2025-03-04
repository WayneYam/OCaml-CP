open! Base
open! Core
open! Stdio

(** Definitions of modules here *)
open! Lib

open! Input

let ten0 = 1 + (1 + (1 + (1 + (1 + (1 + (1 + (1 + (1 + 1))))))))
let x = Array.init 10 ~f:(fun id -> id + 1)
let ten1 = x.(x.(x.(7)))
let ten2 = Int.(5 + 5)

let ten3 =
  let x = [ 10; 9; 8; 7; 6; 5; 4; 3; 2; 1 ] in
  List.hd x
;;

let ten4 =
  let x = [ 10; 9; 8; 7; 6; 5; 4; 3; 2; 1 ] in
  List.hd x
;;

type num2 = { content : int }
type num = { content : num2 }

let ten5 =
  let x : num = { content = { content = 10 } } in
  x.content.content
;;

let ten6 = fst (10, 'a')
let () = printf "%d\n" @@ ten0

(** End of file *)
