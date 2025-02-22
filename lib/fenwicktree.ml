open! Base
open! Core
open! Stdio

(** This is needed because my expander sucks *)
module Algebra = struct
  module type Monoid = sig
    type t

    include Sexpable with type t := t

    val ( + ) : t -> t -> t
    val zero : t
  end

  module type Group = sig
    type t

    include Monoid with type t := t

    val ( ~- ) : t -> t
  end

  module type CommutativeMonoid = Monoid
  module type CommutativeGroup = Group
end

module type S0 = sig
  type elt
  type t = elt array

  val init : int -> t
  val add : t -> int -> elt -> unit
  val get : t -> int -> elt
  val sum : t -> int -> int -> elt
end

module type S = sig
  type elt

  include S0 with type elt := elt

  val set : t -> int -> elt -> unit
end

module Make0 (M : Algebra.CommutativeMonoid) : S0 with type elt := M.t = struct
  type elt = M.t
  type t = elt array

  let init x = Array.init ~f:(fun _ -> M.zero) (x + 1)
  let add _ _ _ = ()
  let get _ _ = M.zero
  let sum _ _ _ = M.zero
end

module Make (M : Algebra.CommutativeGroup) : S with type elt := M.t = struct
  include Make0 (M)

  let set _ _ _ = ()
end
