module type S = sig
  module F : Functor.S
  module G : Functor.S with module C_in = F.C_out and module C_out = F.C_in

  module Phi :
    NaturalTransformation.S
      with module F1 = Functor.HomFunctorLeft(F)(G)
       and module F2 = Functor.HomFunctorRight(F)(G)

  module PhiInv :
    NaturalTransformation.S
      with module F1 = Functor.HomFunctorRight(F)(G)
       and module F2 = Functor.HomFunctorLeft(F)(G)
end

(* ======================================================================== *)

module ToUnitAdjonction (A : S) :
  UnitAdjonction.S with module F = A.F and module G = A.G = struct
  module F = A.F
  module G = A.G

  module Eta :
    NaturalTransformation.S
      with module F1 = Functor.Id(F.C_in)
       and module F2 = Functor.Compose(F)(G) = struct
    module F1 = Functor.Id (F.C_in)
    module F2 = Functor.Compose (F) (G)

    type nat_trans = {
      nat : 'a. 'a F.C_in.obj -> ('a, 'a F.map_obj G.map_obj) F.C_in.morph;
    }

    let nat_trans =
      {
        nat =
          (fun (type a)
            (x : a F.C_in.obj)
            :
            (a, a F.map_obj G.map_obj) F.C_in.morph
          ->
            let obj_product = A.Phi.F1.C_in.Product (x, F.map_obj x) in
            let hom_id = A.Phi.F1.Hom (F.C_out.id (F.map_obj x)) in
            let (A.Phi.F2.Hom eta_x) = A.Phi.nat_trans.nat obj_product hom_id in
            eta_x);
      }
  end

  module Epsilon :
    NaturalTransformation.S
      with module F1 = Functor.Compose(G)(F)
       and module F2 = Functor.Id(F.C_out) = struct
    module F1 = Functor.Compose (G) (F)
    module F2 = Functor.Id (F.C_out)

    type nat_trans = {
      nat : 'a. 'a F.C_out.obj -> ('a G.map_obj F.map_obj, 'a) F.C_out.morph;
    }

    let nat_trans =
      {
        nat =
          (fun (type a)
            (y : a F.C_out.obj)
            :
            (a G.map_obj F.map_obj, a) F.C_out.morph
          ->
            let obj_product = A.PhiInv.F1.C_in.Product (G.map_obj y, y) in
            let hom_id = A.PhiInv.F1.Hom (F.C_in.id (G.map_obj y)) in
            let (A.PhiInv.F2.Hom eps_y) =
              A.PhiInv.nat_trans.nat obj_product hom_id
            in
            eps_y);
      }
  end
end

(* ======================================================================== *)

module WriterReader (T : sig
  type t
end) :
  S
    with module F.C_in = Category.Type
     and module F.C_out = Category.Type
     and module G.C_in = Category.Type
     and module G.C_out = Category.Type
     and type 'a F.map_obj = 'a * T.t
     and type 'a G.map_obj = T.t -> 'a = struct
  module F = struct
    include TypeEndoFunctor.Writer (struct
      type w = T.t
    end)

    module C_in = Category.Type
    module C_out = Category.Type
  end

  module G = struct
    include TypeEndoFunctor.Reader (struct
      type r = T.t
    end)

    module C_in = Category.Type
    module C_out = Category.Type
  end

  module Phi :
    NaturalTransformation.S
      with module F1 = Functor.HomFunctorLeft(F)(G)
       and module F2 = Functor.HomFunctorRight(F)(G) = struct
    module F1 = Functor.HomFunctorLeft (F) (G)
    module F2 = Functor.HomFunctorRight (F) (G)

    type nat_trans = {
      nat : 'a. 'a F1.C_in.obj -> 'a F1.map_obj -> 'a F2.map_obj;
    }

    let nat_trans =
      {
        nat =
          (fun (type a)
            (Product (TypeRep.Type, TypeRep.Type) : a F1.C_in.obj)
            (Hom (f : _ * T.t -> _) : a F1.map_obj)
            :
            a F2.map_obj
          -> Hom (fun x (t : T.t) -> f (x, t)));
      }
  end

  module PhiInv :
    NaturalTransformation.S
      with module F1 = Functor.HomFunctorRight(F)(G)
       and module F2 = Functor.HomFunctorLeft(F)(G) = struct
    module F1 = Functor.HomFunctorRight (F) (G)
    module F2 = Functor.HomFunctorLeft (F) (G)

    type nat_trans = {
      nat : 'a. 'a F1.C_in.obj -> 'a F1.map_obj -> 'a F2.map_obj;
    }

    let nat_trans =
      {
        nat =
          (fun (type a)
            (Product (TypeRep.Type, TypeRep.Type) : a F1.C_in.obj)
            (Hom (f : _ -> T.t -> _) : a F1.map_obj)
            :
            a F2.map_obj
          -> Hom (fun (x, (t : T.t)) -> f x t));
      }
  end
end
