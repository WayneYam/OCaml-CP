open! Base
open! Core

module type O = sig
  val m : int
  val ( +% ) : int -> int -> int
  val ( -% ) : int -> int -> int
  val ( *% ) : int -> int -> int
  val ( /% ) : int -> int -> int
  val ( ^% ) : int -> int -> int
  val ( ~+% ) : int -> int
  val ( ~-% ) : int -> int
  val ( ~/% ) : int -> int
end

let make_o (m : int) =
  (module struct
    let m = m

    let ( +% ) a b =
      let x = a + b in
      if x >= m then x - m else x
    ;;

    let ( -% ) a b =
      let x = a - b in
      if x < 0 then x + m else x
    ;;

    let ( *% ) a b = a * b % m
    let ( ~-% ) a = 0 -% a

    let ( ^% ) a b =
      let rec aux ans a b =
        if b = 0
        then ans
        else if b land 1 > 0
        then aux (ans *% a) (a *% a) (b lsr 1)
        else aux ans (a *% a) (b lsr 1)
      in
      aux 1 a b
    ;;

    let ( ~/% ) a = a ^% (m - 2)
    let ( /% ) a b = a *% ~/%b

    let ( ~+% ) a =
      let a = a % m in
      if a < 0 then a + m else a
    ;;
  end : O)
;;

module O1 = (val make_o 998244353 : O)
module O2 = (val make_o 1000000007 : O)
