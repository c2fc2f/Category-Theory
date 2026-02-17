module type S = sig
  include Semigroup.S

  val empty : t
end

(* ======================================================================== *)

module Unit : S = struct
  include Semigroup.Unit

  let empty = ()
end

(* ======================================================================== *)

module NaturalSum : S with type t = Semigroup.NaturalSum.t = struct
  include Semigroup.NaturalSum

  let empty = 0
end

(* ======================================================================== *)

module NaturalProd : S with type t = Semigroup.NaturalSum.t = struct
  include Semigroup.NaturalProd

  let empty = 1
end

(* ======================================================================== *)

module BoolOr : S with type t = Semigroup.BoolOr.t = struct
  include Semigroup.BoolOr

  let empty = false
end

(* ======================================================================== *)

module BoolAnd : S with type t = Semigroup.BoolAnd.t = struct
  include Semigroup.BoolAnd

  let empty = true
end

(* ======================================================================== *)

module String : S with type t = Semigroup.String.t = struct
  include Semigroup.String

  let empty = String.empty
end

(* ======================================================================== *)

module Free (A : sig
  type t
end) : S with type t = Semigroup.Free(A).t = struct
  include Semigroup.Free (A)

  let empty = []
end

(* ======================================================================== *)

module SetUnion (A : Set.OrderedType) :
  S with type t = Semigroup.SetUnion(A).t = struct
  include Semigroup.SetUnion (A)
  module M = Set.Make (A)

  let empty = M.empty
end

(* ======================================================================== *)

module EndoFunction (A : sig
  type t
end) : S with type t = Semigroup.EndoFunction(A).t = struct
  include Semigroup.EndoFunction (A)

  let empty (a : A.t) = a
end

(* ======================================================================== *)

module EndoFunctionPartial (A : sig
  type t
end) : S with type t = Semigroup.PartialEndoFunction(A).t = struct
  include Semigroup.PartialEndoFunction (A)

  let empty (a : A.t) = Some a
end

(* ======================================================================== *)

module KleeneStarFunction (A : sig
  type t
end) : S with type t = Semigroup.KleeneStarFunction(A).t = struct
  include Semigroup.KleeneStarFunction (A)

  let empty (a : A.t) = [ a ]
end

(* ======================================================================== *)

let sum (type a) (module M : S with type t = a) =
  List.fold_left M.append M.empty

let rec power : type a. (module S with type t = a) -> a -> int -> a =
 fun (module M) e n ->
  match n with 0 -> M.empty | n -> M.append e (power (module M) e (n - 1))

(* ======================================================================== *)

module Product (A : S) (B : S) : S with type t = Semigroup.Product(A)(B).t =
struct
  include Semigroup.Product (A) (B)

  let empty = (A.empty, B.empty)
end

(* ======================================================================== *)

module FromSemigroup (M : Semigroup.S) : S with type t = M.t option = struct
  type t = M.t option

  let append (a : t) (b : t) =
    match (a, b) with
    | None, _ -> b
    | _, None -> a
    | Some a, Some b -> Some (M.append a b)

  let empty = None
end

(* ======================================================================== *)

module FreeCommutative (M : Set.OrderedType) : sig
  type t = int Map.Make(M).t

  val action : t -> int -> t

  include S with type t := t
end = struct
  module N = Map.Make (M)

  type t = int N.t

  let append =
    N.merge (fun (_ : M.t) (a : int option) (b : int option) ->
        match (a, b) with
        | None, None -> None
        | None, Some x | Some x, None -> Some x
        | Some a, Some b when a + b <= 0 -> None
        | Some a, Some b -> Some (a + b))

  let empty = N.empty
  let action (x : t) (n : int) = N.map (fun a -> a * n) x
end

(* ======================================================================== *)

module Kleisli (M : Set.OrderedType) :
  S with type t = M.t -> FreeCommutative(M).t = struct
  module N = Map.Make (M)
  module P = FreeCommutative (M)

  type t = M.t -> P.t

  let empty (_ : M.t) = N.empty
  let append (f : t) (g : t) = fun (a : M.t) -> P.append (f a) (g a)
end
