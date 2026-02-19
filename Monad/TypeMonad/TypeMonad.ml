module type S_Base = sig
  type 'a map_obj

  val fmap : ('a -> 'b) -> 'a map_obj -> 'b map_obj
  val eta : 'a -> 'a map_obj
  val mu : 'a map_obj map_obj -> 'a map_obj
end

module type S = sig
  include S_Base

  (* *** Fonctions étendues des foncteurs *** *)
  val fmap' : ('a -> 'b) -> 'a map_obj -> 'b map_obj
  val ( <$ ) : 'b -> 'a map_obj -> 'b map_obj
  val ( $> ) : 'a map_obj -> 'b -> 'b map_obj
  val ( <$> ) : ('a -> 'b) -> 'a map_obj -> 'b map_obj
  val ( <&> ) : 'a map_obj -> ('a -> 'b) -> 'b map_obj
  val unzip : ('a * 'b) map_obj -> 'a map_obj * 'b map_obj
  val void : 'a map_obj -> unit map_obj

  (* *** Fonctions étendues des monades *** *)
  val ( <*> ) : ('a -> 'b) map_obj -> 'a map_obj -> 'b map_obj
  val liftA2 : ('a -> 'b -> 'c) -> 'a map_obj -> 'b map_obj -> 'c map_obj
  val ( >>= ) : 'a map_obj -> ('a -> 'b map_obj) -> 'b map_obj
  val ( =<< ) : ('a -> 'b map_obj) -> 'a map_obj -> 'b map_obj
  val ( >=> ) : ('a -> 'b map_obj) -> ('b -> 'c map_obj) -> 'a -> 'c map_obj
  val ( <=< ) : ('b -> 'c map_obj) -> ('a -> 'b map_obj) -> 'a -> 'c map_obj
  val ( >> ) : 'a map_obj -> 'b map_obj -> 'b map_obj
  val ( << ) : 'a map_obj -> 'b map_obj -> 'a map_obj
  val return : 'a -> 'a map_obj
  val join : 'a map_obj map_obj -> 'a map_obj
end

module Helper (M : S_Base) : S with type 'a map_obj = 'a M.map_obj = struct
  include TypeEndoFunctor.Helper (struct
    module C = Category.Type

    type 'a map_obj = 'a M.map_obj

    let map_obj _ = TypeRep.Type
    let fmap _ _ = M.fmap
  end)

  include M

  let ( <*> ) (f : ('a -> 'b) map_obj) (x : 'a map_obj) : 'b map_obj =
    fmap' (fun f' -> fmap' f' x) f |> mu

  let liftA2 (f : 'a -> 'b -> 'c) (x : 'a map_obj) (y : 'b map_obj) : 'c map_obj
      =
    fmap' f x <*> y

  let ( >>= ) (x : 'a map_obj) (f : 'a -> 'b map_obj) : 'b map_obj =
    fmap' f x |> mu

  let ( =<< ) (f : 'a -> 'b map_obj) (x : 'a map_obj) : 'b map_obj = x >>= f

  let ( >=> ) (f : 'a -> 'b map_obj) (g : 'b -> 'c map_obj) (x : 'a) :
      'c map_obj =
    f x >>= g

  let ( <=< ) (g : 'b -> 'c map_obj) (f : 'a -> 'b map_obj) : 'a -> 'c map_obj =
    f >=> g

  let ( >> ) (x : 'a map_obj) (y : 'b map_obj) : 'b map_obj = x >>= Fun.const y
  let ( << ) (x : 'a map_obj) (y : 'b map_obj) : 'a map_obj = y >>= Fun.const x
  let return = eta
  let join = mu
end

(* ======================================================================== *)

module ToMonad (M : S_Base) : Monad.S = struct
  module F :
    EndoFunctor.S
      with module C = Category.Type
       and type 'a map_obj = 'a M.map_obj = struct
    module C = Category.Type

    type 'a map_obj = 'a M.map_obj

    let map_obj _ = TypeRep.Type

    let fmap (type a b) TypeRep.Type TypeRep.Type (f : a -> b) (x : a M.map_obj)
        : b M.map_obj =
      M.fmap f x
  end

  module Eta = struct
    module F1 = Functor.Id (Category.Type)
    module F2 = EndoFunctor.ToFunctor (F)

    type nat_trans = { nat : 'a. 'a TypeRep.type_rep -> 'a -> 'a M.map_obj }

    let nat_trans =
      { nat = (fun (type a) TypeRep.Type (x : a) : a M.map_obj -> M.eta x) }
  end

  module Mu = struct
    module F1 =
      Functor.Compose (EndoFunctor.ToFunctor (F)) (EndoFunctor.ToFunctor (F))

    module F2 = EndoFunctor.ToFunctor (F)

    type nat_trans = {
      nat : 'a. 'a TypeRep.type_rep -> 'a M.map_obj M.map_obj -> 'a M.map_obj;
    }

    let nat_trans =
      {
        nat =
          (fun (type a)
            TypeRep.Type
            (x : a M.map_obj M.map_obj)
            :
            a M.map_obj
          -> M.mu x);
      }
  end
end

(* ======================================================================== *)

module List : S = Helper (struct
  type 'a map_obj = 'a list

  let fmap = List.map
  let eta = List.singleton
  let mu = List.concat
end)

(* ======================================================================== *)

module Option : S = Helper (struct
  type 'a map_obj = 'a option

  let fmap = Option.map
  let eta = Option.some
  let mu = Option.join
end)

(* ======================================================================== *)

module Reader (R : sig
  type t
end) : S = Helper (struct
  type 'a map_obj = R.t -> 'a

  let fmap f x = fun (r : R.t) -> f (x r)
  let eta x = fun (_ : R.t) -> x
  let mu xx = fun (r : R.t) -> (xx r) r
end)

(* ======================================================================== *)

module Writer (W : Monoid.S) : S = Helper (struct
  type 'a map_obj = 'a * W.t

  let fmap f (x, w) = (f x, w)
  let eta x = (x, W.empty)
  let mu ((x, w), w') = (x, W.append w w')
end)

(* ======================================================================== *)

module StateMonad (S : sig
  type t
end) : S = Helper (struct
  type 'a map_obj = S.t -> 'a * S.t

  let fmap f x =
   fun (s : S.t) ->
    let x', s' = x s in
    (f x', s')

  let eta x = fun (s : S.t) -> (x, s)

  let mu xx =
   fun (s : S.t) ->
    let x, s' = xx s in
    x s'
end)

(* ======================================================================== *)

type 'a cont = { cont : 'r. ('a -> 'r) -> 'r }

module Cont : S = Helper (struct
  type 'a map_obj = 'a cont

  let fmap f x = { cont = (fun k -> x.cont (fun a -> k (f a))) }
  let eta x = { cont = (fun k -> k x) }

  let mu (xx : 'a cont cont) : 'a cont =
    { cont = (fun k -> xx.cont (fun x -> x.cont k)) }
end)

(* ======================================================================== *)

module Free (F : TypeEndoFunctor.S) : S = Helper (struct
  type 'a free = Pure of 'a | Free of 'a free F.map_obj
  type 'a map_obj = 'a free

  let rec fmap f x =
    match x with
    | Pure x -> Pure (f x)
    | Free v -> Free (F.fmap TypeRep.Type TypeRep.Type (fmap f) v)

  let eta x = Pure x

  let rec mu xx =
    match xx with
    | Pure x -> x
    | Free v -> Free (F.fmap TypeRep.Type TypeRep.Type mu v)
end)
