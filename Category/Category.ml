(* Signature pour les modules implantant la notion de catégorie *)
module type S = sig
  (*
    Type (polymorphe) des objets: les objets de la catégorie sont les valeurs
    de ces types
  *)
  type 'a obj

  (*
    Type (polymorphe) des morphismes: les morphismes de valeur ('a,'b) morph
    sont les morphismes de Hom(oa, ob) pour oa un objet de type 'a obj et ob
    un objet de type 'b ob
  *)
  type ('a, 'b) morph

  (* Morphisme identité associé à un objet donné *)
  val id : 'a obj -> ('a, 'a) morph

  (* Composition de morphismes, pouvant dépendre des objets *)
  val compose :
    'a obj ->
    'b obj ->
    'c obj ->
    ('b, 'c) morph ->
    ('a, 'b) morph ->
    ('a, 'c) morph
end

(* ======================================================================== *)

module Unit : S = struct
  type 'a obj = unit
  type ('a, 'b) morph = unit

  let id _ = ()
  let compose _ _ _ _ _ = ()
end

(* ======================================================================== *)

module Void : S = struct
  type 'a obj = |
  type ('a, 'b) morph = |

  let id (x : 'a obj) = match x with _ -> .
  let compose (x : 'a obj) _ _ _ _ = match x with _ -> .
end

(* ======================================================================== *)

module Type :
  S with type 'a obj = 'a TypeRep.type_rep and type ('a, 'b) morph = 'a -> 'b =
struct
  type 'a obj = 'a TypeRep.type_rep
  type ('a, 'b) morph = 'a -> 'b

  let id _ = Fun.id
  let compose _ _ _ = Fun.compose
end

(* ======================================================================== *)

module Dual (C : S) :
  S with type 'a obj = 'a C.obj and type ('a, 'b) morph = ('b, 'a) C.morph =
struct
  type 'a obj = 'a C.obj
  type ('a, 'b) morph = ('b, 'a) C.morph

  let id = C.id
  let compose oa ob oc g f = C.compose oc ob oa f g
end

(* ======================================================================== *)

module Discrete : S = struct
  type 'a obj = 'a TypeRep.type_rep
  type ('a, 'b) morph = Refl : ('a, 'a) morph

  let id _ = Refl

  let compose (type a b c) (_ : a obj) (_ : b obj) (_ : c obj)
      (g : (b, c) morph) (f : (a, b) morph) : (a, c) morph =
    match (f, g) with Refl, Refl -> Refl
end

(* ======================================================================== *)

module PartialFunction : S = struct
  type 'a obj = 'a TypeRep.type_rep
  type ('a, 'b) morph = 'a -> 'b option

  let id _ (a : 'a) = Some a

  let compose _ _ _ (f : ('b, 'c) morph) (g : ('a, 'b) morph) : ('a, 'c) morph =
   fun a -> match g a with None -> None | Some b -> f b
end

(* ======================================================================== *)

module ToMonoid
    (C : S)
    (A : sig
      type a

      val obj : a C.obj
    end) : Monoid.S with type t = (A.a, A.a) C.morph = struct
  type t = (A.a, A.a) C.morph

  let empty = C.id A.obj
  let append = C.compose A.obj A.obj A.obj
end

(* ======================================================================== *)

module FromMonoid (M : Monoid.S) : S = struct
  type 'a obj = M.t
  type ('a, 'b) morph = M.t -> M.t

  let id _ = Fun.id
  let compose _ _ _ = Fun.compose
end

(* ======================================================================== *)

module Product (C : S) (C' : S) : sig
  type 'a obj = Product : 'a C.obj * 'b C'.obj -> ('a * 'b) obj

  type ('a, 'b) morph =
    | Morph :
        ('x1, 'x2) C.morph * ('y1, 'y2) C'.morph
        -> ('x1 * 'y1, 'x2 * 'y2) morph

  include S with type 'a obj := 'a obj and type ('a, 'b) morph := ('a, 'b) morph
end = struct
  type 'a obj = Product : 'a C.obj * 'b C'.obj -> ('a * 'b) obj

  type ('a, 'b) morph =
    | Morph :
        ('x1, 'x2) C.morph * ('y1, 'y2) C'.morph
        -> ('x1 * 'y1, 'x2 * 'y2) morph

  let id (type a) (Product (o1, o2) : a obj) : (a, a) morph =
    Morph (C.id o1, C'.id o2)

  let compose (type a b c) (Product (oa, oa') : a obj)
      (Product (ob, ob') : b obj) (Product (oc, oc') : c obj)
      (Morph (fbc, fbc') : (b, c) morph) (Morph (fab, fab') : (a, b) morph) :
      (a, c) morph =
    Morph (C.compose oa ob oc fbc fab, C'.compose oa' ob' oc' fbc' fab')
end

(* ======================================================================== *)

type left = |
type right = |

module Sum (C : S) (C' : S) : sig
  type 'a obj =
    | Left : 'a C.obj -> ('a * left) obj
    | Right : 'b C'.obj -> ('b * right) obj

  type ('a, 'b) morph =
    | Left : ('a, 'b) C.morph -> ('a * left, 'b * left) morph
    | Right : ('a, 'b) C'.morph -> ('a * right, 'b * right) morph

  include S with type 'a obj := 'a obj and type ('a, 'b) morph := ('a, 'b) morph
end = struct
  type 'a obj =
    | Left : 'a C.obj -> ('a * left) obj
    | Right : 'b C'.obj -> ('b * right) obj

  type ('a, 'b) morph =
    | Left : ('a, 'b) C.morph -> ('a * left, 'b * left) morph
    | Right : ('a, 'b) C'.morph -> ('a * right, 'b * right) morph

  let id (type a) (o : a obj) : (a, a) morph =
    match o with Left o -> Left (C.id o) | Right o -> Right (C'.id o)

  let compose (type a b c) (oa : a obj) (ob : b obj) (oc : c obj)
      (fbc : (b, c) morph) (fab : (a, b) morph) : (a, c) morph =
    match (oa, ob, oc, fbc, fab) with
    | Left oa, Left ob, Left oc, Left fbc, Left fab ->
        Left (C.compose oa ob oc fbc fab)
    | Right oa, Right ob, Right oc, Right fbc, Right fab ->
        Right (C'.compose oa ob oc fbc fab)
    | _ -> .
end

(* ======================================================================== *)

module Relation : sig
  type 'a obj =
    | Obj : (module Set.S with type elt = 'a and type t = 't) -> ('a * 't) obj

  type ('a, 'b) morph =
    | Morph :
        (module Set.S with type t = 't and type elt = 'b) * ('a -> 't)
        -> ('a * 'c, 'b * 't) morph

  include S with type 'a obj := 'a obj and type ('a, 'b) morph := ('a, 'b) morph
end = struct
  type 'a obj =
    | Obj : (module Set.S with type elt = 'a and type t = 't) -> ('a * 't) obj

  type ('a, 'b) morph =
    | Morph :
        (module Set.S with type elt = 'b and type t = 't) * ('a -> 't)
        -> ('a * 'c, 'b * 't) morph

  let id (type a) (o : a obj) : (a, a) morph =
    match o with Obj (module S) -> Morph ((module S), S.singleton)

  let compose (type a b c) _ _ _ (fbc : (b, c) morph) (fab : (a, b) morph) :
      (a, c) morph =
    match (fbc, fab) with
    | Morph ((module Sc), g), Morph ((module Sb), f) ->
        let h x =
          let set_b = f x in
          Sb.fold (fun y acc -> Sc.union (g y) acc) set_b Sc.empty
        in
        Morph ((module Sc), h)
end
