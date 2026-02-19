(*
  Signature pour les modules implantant la notion d'endofoncteur "spécialisé"
  à la catégorie des types. NB: pour que le code soit correct, il faut définir
  la catégorie des types dans un fichier FunCats.ml contenant la définition
  d'un module FunCat de type Category.S
*)
module type S_Base = sig
  (*
    Spécialisation (destructive) de la catégorie C de la signature des
    endofoncteurs 
  *)
  include
    EndoFunctor.S
      with type ('a, 'b) C.morph = 'a -> 'b
       and module C := Category.Type
end

module type S = sig
  include S_Base

  val fmap' : ('a -> 'b) -> 'a map_obj -> 'b map_obj
  val ( <$ ) : 'b -> 'a map_obj -> 'b map_obj
  val ( $> ) : 'a map_obj -> 'b -> 'b map_obj
  val ( <$> ) : ('a -> 'b) -> 'a map_obj -> 'b map_obj
  val ( <&> ) : 'a map_obj -> ('a -> 'b) -> 'b map_obj
  val unzip : ('a * 'b) map_obj -> 'a map_obj * 'b map_obj
  val void : 'a map_obj -> unit map_obj
end

module Helper (F : S_Base) : S with type 'a map_obj = 'a F.map_obj = struct
  include F

  let fmap' f x = F.fmap TypeRep.Type TypeRep.Type f x

  let ( <$ ) (b : 'b) (fa : 'a F.map_obj) : 'b F.map_obj =
    fmap' (Fun.const b) fa

  let ( $> ) (fa : 'a F.map_obj) (b : 'b) : 'b F.map_obj =
    fmap' (Fun.const b) fa

  let ( <$> ) (f : 'a -> 'b) (fa : 'a F.map_obj) : 'b F.map_obj = fmap' f fa
  let ( <&> ) (fa : 'a F.map_obj) (f : 'a -> 'b) : 'b F.map_obj = fmap' f fa

  let unzip (fab : ('a * 'b) F.map_obj) : 'a F.map_obj * 'b F.map_obj =
    (fmap' fst fab, fmap' snd fab)

  let void (fa : 'a F.map_obj) : unit F.map_obj = fmap' (Fun.const ()) fa
end

(* ======================================================================== *)

module Id : S with type 'a map_obj = 'a = Helper (struct
  type 'a map_obj = 'a

  let map_obj _ = TypeRep.Type
  let fmap _ _ = Fun.id
end)

(* ======================================================================== *)

module Const1 : S = Helper (struct
  type 'a map_obj = unit

  let map_obj _ = TypeRep.Type
  let fmap _ _ _ _ = ()
end)

(* ======================================================================== *)

module Option : S with type 'a map_obj = 'a option = Helper (struct
  type 'a map_obj = 'a option

  let map_obj _ = TypeRep.Type
  let fmap _ _ f x = match x with None -> None | Some x -> Some (f x)
end)

(* ======================================================================== *)

module Sum (F1 : S) (F2 : S) :
  S with type 'a map_obj = ('a F1.map_obj, 'a F2.map_obj) Either.t =
Helper (struct
  type 'a map_obj = ('a F1.map_obj, 'a F2.map_obj) Either.t

  let map_obj _ = TypeRep.Type

  let fmap _ _ f x =
    match x with
    | Either.Left x -> Either.Left (F1.fmap TypeRep.Type TypeRep.Type f x)
    | Either.Right x -> Either.Right (F2.fmap TypeRep.Type TypeRep.Type f x)
end)

(* ======================================================================== *)

module Double : S = Helper (struct
  type 'a map_obj = ('a, 'a) Either.t

  let map_obj _ = TypeRep.Type

  let fmap _ _ f x =
    match x with
    | Either.Left x -> Either.Left (f x)
    | Either.Right x -> Either.Right (f x)
end)

(* ======================================================================== *)

module Square : S with type 'a map_obj = 'a * 'a = Helper (struct
  type 'a map_obj = 'a * 'a

  let map_obj _ = TypeRep.Type
  let fmap _ _ f (a, b) = (f a, f b)
end)

(* ======================================================================== *)

module List : S with type 'a map_obj = 'a list = Helper (struct
  type 'a map_obj = 'a list

  let map_obj _ = TypeRep.Type
  let fmap _ _ f x = List.map f x
end)

(* ======================================================================== *)

module Reader (M : sig
  type r
end) : sig
  type ('r, 'a) reader = 'r -> 'a
  type 'a map_obj = (M.r, 'a) reader

  include S with type 'a map_obj := 'a map_obj
end = struct
  type ('r, 'a) reader = 'r -> 'a

  include Helper (struct
    type ('r, 'a) reader = 'r -> 'a
    type 'a map_obj = (M.r, 'a) reader

    let map_obj _ = TypeRep.Type

    let fmap (_ : 'a TypeRep.type_rep) (_ : 'b TypeRep.type_rep)
        (f : ('a, 'b) Category.Type.morph) (x : 'a map_obj) : 'b map_obj =
     fun (r : M.r) -> f (x r)
  end)
end

(* ======================================================================== *)

module Writer (M : sig
  type w
end) : sig
  type ('a, 'w) writer = 'a * 'w
  type 'a map_obj = ('a, M.w) writer

  include S with type 'a map_obj := 'a map_obj
end = struct
  type ('a, 'w) writer = 'a * 'w

  include Helper (struct
    type ('a, 'w) writer = 'a * 'w
    type 'a map_obj = ('a, M.w) writer

    let map_obj _ = TypeRep.Type

    let fmap (_ : 'a TypeRep.type_rep) (_ : 'b TypeRep.type_rep)
        (f : ('a, 'b) Category.Type.morph) ((x, w) : 'a map_obj) : 'b map_obj =
      (f x, w)
  end)
end

(* ======================================================================== *)

module State (M : sig
  type s
end) : S = Helper (struct
  type ('s, 'a) state = 's -> 'a * 's
  type 'a map_obj = (M.s, 'a) state

  let map_obj _ = TypeRep.Type

  let fmap (_ : 'a TypeRep.type_rep) (_ : 'b TypeRep.type_rep)
      (f : ('a, 'b) Category.Type.morph) (x : 'a map_obj) : 'b map_obj =
   fun (s : M.s) ->
    let x', s' = x s in
    (f x', s')
end)

(* ======================================================================== *)

module Continuation : S = Helper (struct
  type 'a cont = { cont : 'r. ('a -> 'r) -> 'r }
  type 'a map_obj = 'a cont

  let map_obj _ = TypeRep.Type

  let fmap (_ : 'a TypeRep.type_rep) (_ : 'b TypeRep.type_rep)
      (f : ('a, 'b) Category.Type.morph) (x : 'a map_obj) : 'b map_obj =
    { cont = (fun k -> x.cont (fun a -> k (f a))) }
end)

(* ======================================================================== *)

module Product (F : S) (G : S) : S = Helper (struct
  type 'a map_obj = 'a F.map_obj * 'a G.map_obj

  let map_obj _ = TypeRep.Type

  let fmap (_ : 'a TypeRep.type_rep) (_ : 'b TypeRep.type_rep)
      (f : ('a, 'b) Category.Type.morph) ((x, x') : 'a map_obj) : 'b map_obj =
    (F.fmap' f x, G.fmap' f x')
end)

(* ======================================================================== *)

module Free (F : S) : S = Helper (struct
  type 'a free = Pure of 'a | Free of 'a free F.map_obj
  type 'a map_obj = 'a free

  let map_obj _ = TypeRep.Type

  let rec fmap _ _ (f : 'a -> 'b) (x : 'a free) : 'b free =
    match x with
    | Pure x -> Pure (f x)
    | Free v ->
        Free
          (F.fmap TypeRep.Type TypeRep.Type
             (fmap TypeRep.Type TypeRep.Type f)
             v)
end)
