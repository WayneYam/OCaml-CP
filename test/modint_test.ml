open! Base
open! Core
open! Stdio
open! Lib
module M = (val Modint.make 998244353 : Modint.Modint)

let%expect_test "transform" =
  for i = -10 to 10 do
    i |> M.of_int |> M.to_int |> printf "%d "
  done;
  printf "\n";
  for i = 123456789 to 123456798 do
    i |> M.of_int |> M.to_int |> printf "%d "
  done;
  printf "\n";
  [%expect
    {|
    998244343 998244344 998244345 998244346 998244347 998244348 998244349 998244350 998244351 998244352 0 1 2 3 4 5 6 7 8 9 10
    123456789 123456790 123456791 123456792 123456793 123456794 123456795 123456796 123456797 123456798 |}]
;;

let%expect_test "addition" =
  let tests = [ -1, -2; -1, 2; 1, -2; 1, 2; 444444444, 555555555 ] in
  let f x y =
    let x = M.of_int x in
    let y = M.of_int y in
    let z' = M.(x + y) in
    let z = M.to_int z' in
    printf "%d\n" z
  in
  List.iter tests ~f:(fun (x, y) -> f x y);
  [%expect
    {|
    998244350
    1
    998244352
    3
    1755646 |}]
;;

let%expect_test "multiplication" =
  let tests = [ -1, -2; -1, 2; 1, -2; 1, 2; 123456789, 98765432 ] in
  let f x y =
    let x = M.of_int x in
    let y = M.of_int y in
    let z' = M.(x * y) in
    let z = M.to_int z' in
    printf "%d\n" z
  in
  List.iter tests ~f:(fun (x, y) -> f x y);
  [%expect
    {|
    2
    998244351
    998244351
    2
    812618277 |}]
;;

let%expect_test "inverse" =
  for i = 1 to 10 do
    i |> M.of_int |> M.( ~/ ) |> M.to_int |> printf "%d "
  done;
  printf "\n";
  for i = 1 to 10 do
    -i |> M.of_int |> M.( ~/ ) |> M.to_int |> printf "%d "
  done;
  printf "\n";
  [%expect
    {|
      1 499122177 332748118 748683265 598946612 166374059 855638017 873463809 443664157 299473306
      998244352 499122176 665496235 249561088 399297741 831870294 142606336 124780544 554580196 698771047 |}]
;;

let%expect_test "pow" =
  for i = 1 to 10 do
    2 |> M.of_int |> Fn.flip M.pow i |> M.to_int |> printf "%d "
  done;
  printf "\n";
  for i = 1 to 10 do
    5 |> M.of_int |> Fn.flip M.pow i |> M.to_int |> printf "%d "
  done;
  printf "\n";
  [%expect
    {|
    2 4 8 16 32 64 128 256 512 1024
    5 25 125 625 3125 15625 78125 390625 1953125 9765625 |}]
;;
