open! Base
open! Core
open! Stdio

module Monoid = struct
  type 'a t =
    { id : 'a
    ; plus : 'a -> 'a -> 'a
    ; sexp_of_t : 'a -> Sexp.t
    }

  let int_plus : int t = Int.{ id = zero; plus = ( + ); sexp_of_t }
  let int_mul : int t = Int.{ id = one; plus = ( * ); sexp_of_t }

  (* let xor : int t = Int.{ id = zero; plus = ( lxor ); sexp_of_t } *)

  let float_plus : float t = Float.{ id = one; plus = ( + ); sexp_of_t }
  let float_mul : float t = Float.{ id = one; plus = ( * ); sexp_of_t }
end

module Group = struct
  type 'a t =
    { id : 'a
    ; plus : 'a -> 'a -> 'a
    ; inverse : 'a -> 'a
    ; sexp_of_t : 'a -> Sexp.t
    }

  let int_plus : int t = Int.{ id = zero; plus = ( + ); sexp_of_t; inverse = ( ~- ) }

  (* let xor : int t = Int.{ id = zero; plus = ( lxor ); sexp_of_t; inverse = (fun x -> x) } *)
  let float_plus : float t = Float.{ id = one; plus = ( + ); sexp_of_t; inverse = ( ~- ) }

  let float_mul : float t =
    Float.{ id = one; plus = ( * ); sexp_of_t; inverse = (fun x -> 1. / x) }
  ;;

  let to_monoid (g : 'a t) : 'a Monoid.t =
    { id = g.id; plus = g.plus; sexp_of_t = g.sexp_of_t }
  ;;

  let minus g a b = g.plus a (g.inverse b)
end
