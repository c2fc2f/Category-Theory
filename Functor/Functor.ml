(** Signature pour les modules implantant la notion de foncteur *)
module type S = sig
  module C_in : Category.S
  (** Module représentant la catégorie de départ du foncteur *)

  module C_out : Category.S
  (** Module représentant la catégorie d'arrivée du foncteur *)

  type 'a map_obj
  (** Type (polymorphe) des objets images par le foncteur *)

  val map_obj : 'a C_in.obj -> 'a map_obj C_out.obj
  (** Application du foncteur aux objets *)

  val fmap :
    'a C_in.obj ->
    'b C_in.obj ->
    ('a, 'b) C_in.morph ->
    ('a map_obj, 'b map_obj) C_out.morph
  (** Application du foncteur aux morphismes *)
end

(* ======================================================================== *)

module Diagonal (C : Category.S) : S = struct
  module C_in = C
  module C_out = Category.Product (C) (C)

  type 'a map_obj = 'a * 'a

  let map_obj (x : 'a C_in.obj) = C_out.Product (x, x)

  let fmap (type a b) (_ : a C_in.obj) (_ : b C_in.obj) (f : (a, b) C_in.morph)
      : (a map_obj, b map_obj) C_out.morph =
    C_out.Morph (f, f)
end

(* ======================================================================== *)

module Compose (F : S) (G : S with module C_in = F.C_out) :
  S with module C_in = F.C_in and module C_out = G.C_out = struct
  module C_in = F.C_in
  module C_out = G.C_out

  type 'a map_obj = 'a F.map_obj G.map_obj

  let map_obj (x : 'a C_in.obj) = G.map_obj (F.map_obj x)

  let fmap (type a b) (x : a C_in.obj) (y : b C_in.obj) (f : (a, b) C_in.morph)
      : (a map_obj, b map_obj) C_out.morph =
    G.fmap (F.map_obj x) (F.map_obj y) (F.fmap x y f)
end

(* ======================================================================== *)

module ProductType : sig
  type 'a map_obj = Product : ('b * 'c) -> ('b * 'c) map_obj

  module C_in : module type of Category.Product (Category.Type) (Category.Type)
  module C_out = Category.Type

  include
    S
      with type 'a map_obj := 'a map_obj
       and module C_in := C_in
       and module C_out := C_out
end = struct
  module C_in = Category.Product (Category.Type) (Category.Type)
  module C_out = Category.Type

  type 'a map_obj = Product : ('b * 'c) -> ('b * 'c) map_obj

  let map_obj _ = TypeRep.Type

  let fmap (type a b) (_ : a C_in.obj) (_ : b C_in.obj)
      (Morph (f, f') : (a, b) C_in.morph) : (a map_obj, b map_obj) C_out.morph =
   fun (Product (x, x')) -> Product (f x, f' x')
end

(* ======================================================================== *)

module DualProductType : sig
  type 'a map_obj = Product : ('c * 'b) -> ('b * 'c) map_obj

  module C_in : module type of Category.Product (Category.Type) (Category.Type)
  module C_out = Category.Type

  include
    S
      with type 'a map_obj := 'a map_obj
       and module C_in := C_in
       and module C_out := C_out
end = struct
  module C_in = Category.Product (Category.Type) (Category.Type)
  module C_out = Category.Type

  type 'a map_obj = Product : ('c * 'b) -> ('b * 'c) map_obj

  let map_obj _ = TypeRep.Type

  let fmap (type a b) (_ : a C_in.obj) (_ : b C_in.obj)
      (Morph (f, f') : (a, b) C_in.morph) : (a map_obj, b map_obj) C_out.morph =
   fun (Product (x, x')) -> Product (f' x, f x')
end

(* ======================================================================== *)

module SumType : S = struct
  module C_in = Category.Product (Category.Type) (Category.Type)
  module C_out = Category.Type

  type 'a map_obj = Sum : ('x, 'y) Either.t -> ('x * 'y) map_obj

  let map_obj _ = TypeRep.Type

  let fmap (type a b) (_ : a C_in.obj) (_ : b C_in.obj)
      (Morph (f, f') : (a, b) C_in.morph) : (a map_obj, b map_obj) C_out.morph =
   fun (Sum v) ->
    match v with
    | Either.Left x -> Sum (Either.Left (f x))
    | Either.Right y -> Sum (Either.Right (f' y))
end

(* ======================================================================== *)

module Hom (C : Category.S) = struct
  module C_in = Category.Product (Category.Dual (C)) (C)
  module C_out = Category.Type

  type 'a map_obj = Hom : ('x, 'y) C.morph -> ('x * 'y) map_obj

  let fmap (type a b) (C_in.Product (oa_op, oa) : a C_in.obj)
      (C_in.Product (ob_op, ob) : b C_in.obj)
      (C_in.Morph (f_op, g) : (a, b) C_in.morph) : a map_obj -> b map_obj =
   fun (Hom h) ->
    Hom (C.compose ob_op oa ob g (C.compose ob_op oa_op oa h f_op))
end

(* ======================================================================== *)

module Id (C : Category.S) : S with module C_in = C and module C_out = C =
struct
  module C_in = C
  module C_out = C

  type 'a map_obj = 'a

  let map_obj o = o

  let fmap (type a b) (_ : a C_in.obj) (_ : b C_in.obj) (f : (a, b) C_in.morph)
      : (a map_obj, b map_obj) C_out.morph =
    f
end
