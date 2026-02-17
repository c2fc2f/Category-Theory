module type S = sig
  type t

  val zero : t
  val one : t
  val add : t -> t -> t
  val mul : t -> t -> t
end

(* ======================================================================== *)

module FromMonoids (M1 : Monoid.S) (M2 : Monoid.S with type t = M1.t) :
  S with type t = M1.t = struct
  type t = M1.t

  let zero = M1.empty
  let one = M2.empty
  let add = M1.append
  let mul = M2.append
end

(* ======================================================================== *)

module Natural : S with type t = int = struct
  include FromMonoids (Monoid.NaturalSum) (Monoid.NaturalProd)
end

(* ======================================================================== *)

module Tropical : S with type t = int option = struct
  type t = int option

  let zero = Some 0
  let one = Some 0

  let add (a : t) (b : t) =
    match (a, b) with
    | None, x | x, None -> x
    | Some a, Some b -> Some (max a b)

  let mul (a : t) (b : t) =
    match (a, b) with
    | None, _ | _, None -> None
    | Some a, Some b -> Some (a + b)
end

(* ======================================================================== *)

module Bool : S with type t = bool = struct
  include FromMonoids (Monoid.BoolOr) (Monoid.BoolAnd)
end

(* ======================================================================== *)

module FreeCommutative (M : sig
  include S
  include Set.OrderedType with type t := t
end) : S with type t = Monoid.FreeCommutative(M).t = struct
  module N = Map.Make (M)
  module F = Monoid.FreeCommutative (M)
  include Monoid.FreeCommutative (M)

  let zero = F.empty
  let one = N.of_list [ (M.one, 1) ]
  let add = F.append

  let mul (a : t) (b : t) =
    if N.is_empty a || N.is_empty b then N.empty
    else
      N.fold
        (fun k_a c_a acc ->
          N.fold
            (fun k_b c_b inner_acc ->
              N.update (M.mul k_a k_b)
                (function
                  | None -> Some (c_a * c_b)
                  | Some exist -> Some (exist + (c_a * c_b)))
                inner_acc)
            b acc)
        a N.empty
end

(* ======================================================================== *)

module Derive (M : sig
  include Monoid.S
  include Set.OrderedType with type t := t
end) : S = struct
  module N = Map.Make (M)
  module F = Monoid.FreeCommutative (M)

  type t = F.t

  let zero = F.empty
  let one = N.of_list [ (M.empty, 1) ]
  let add = F.append

  let mul (a : t) (b : t) =
    if N.is_empty a || N.is_empty b then N.empty
    else
      N.fold
        (fun k_a c_a acc ->
          N.fold
            (fun k_b c_b inner_acc ->
              N.update (M.append k_a k_b)
                (function
                  | None -> Some (c_a * c_b)
                  | Some exist -> Some (exist + (c_a * c_b)))
                inner_acc)
            b acc)
        a N.empty
end
