module type S = sig
  type t

  val append : t -> t -> t
end

(* ======================================================================== *)

module Void : S = struct
  type t = |

  let append (t1 : t) (_ : t) = match t1 with _ -> .
end

(* ======================================================================== *)

module Unit : S with type t = unit = struct
  type t = unit

  let append () () = ()
end

(* ======================================================================== *)

module NaturalSum : S with type t = int = struct
  type t = int

  let append = ( + )
end

(* ======================================================================== *)

module NaturalProd : S with type t = int = struct
  type t = int

  let append = ( * )
end

(* ======================================================================== *)

module BoolOr : S with type t = bool = struct
  type t = bool

  let append = ( || )
end

(* ======================================================================== *)

module BoolAnd : S with type t = bool = struct
  type t = bool

  let append = ( && )
end

(* ======================================================================== *)

module String : S with type t = string = struct
  type t = string

  let append = ( ^ )
end

(* ======================================================================== *)

module Free (A : sig
  type t
end) : S with type t = A.t list = struct
  type t = A.t list

  let append t1 t2 = t1 @ t2
end

(* ======================================================================== *)

module SetUnion (A : Set.OrderedType) : S with type t = Set.Make(A).t = struct
  module S = Set.Make (A)

  type t = S.t

  let append = S.union
end

(* ======================================================================== *)

module SetIntersection (A : Set.OrderedType) : S with type t = Set.Make(A).t =
struct
  module S = Set.Make (A)

  type t = S.t

  let append = S.inter
end

(* ======================================================================== *)

module EndoFunction (A : sig
  type t
end) : S with type t = A.t -> A.t = struct
  type t = A.t -> A.t

  let append (t1 : t) (t2 : t) (a : A.t) = t1 (t2 a)
end

(* ======================================================================== *)

module PartialEndoFunction (A : sig
  type t
end) : S with type t = A.t -> A.t option = struct
  type t = A.t -> A.t option

  let append (t1 : t) (t2 : t) (a : A.t) =
    match t2 a with None -> None | Some t2 -> t1 t2
end

(* ======================================================================== *)

module KleeneStarFunction (A : sig
  type t
end) : S with type t = A.t -> A.t list = struct
  type t = A.t -> A.t list

  let append (t1 : t) (t2 : t) (a : A.t) =
    List.fold_left (fun acc e -> acc @ t1 e) [] (t2 a)
end

(* ======================================================================== *)

module Product (A : S) (B : S) : S with type t = A.t * B.t = struct
  type t = A.t * B.t

  let append (s1, s2) (s'1, s'2) = (A.append s1 s'1, B.append s2 s'2)
end

let lambda (type a) (module Product : S with type t = unit * a) (p : unit * a) :
    a =
  snd p

let rho (type a) (module Product : S with type t = unit * a) (p : a * unit) : a
    =
  fst p

let alpha (type a b c) (module Product : S with type t = (a * b) * c)
    (p : (a * b) * c) : a * (b * c) =
  match p with (a, b), c -> (a, (b, c))

let phi (type a b c d) (module Product : S with type t = a * c)
    (module Product : S with type t = b * d) (p1 : a -> b) (p2 : c -> d) :
    a * c -> b * d =
 fun ((x, y) : a * c) -> (p1 x, p2 y)
