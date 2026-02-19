module type S = sig
  module F : EndoFunctor.S

  module Eta :
    NaturalTransformation.S
      with module F1 = Functor.Id(F.C)
       and module F2 = EndoFunctor.ToFunctor(F)

  module Mu :
    NaturalTransformation.S
      with module F1 = Functor.Compose(EndoFunctor.ToFunctor(F))
                         (EndoFunctor.ToFunctor(F))
       and module F2 = EndoFunctor.ToFunctor(F)
end

(* ======================================================================== *)

module List : S = struct
  module F = struct
    include TypeEndoFunctor.List
    module C = Category.Type
  end

  module Eta :
    NaturalTransformation.S
      with module F1 = Functor.Id(F.C)
       and module F2 = EndoFunctor.ToFunctor(F) = struct
    module F1 = Functor.Id (F.C)
    module F2 = EndoFunctor.ToFunctor (F)

    type nat_trans = { nat : 'a. 'a TypeRep.type_rep -> 'a -> 'a list }

    let nat_trans =
      { nat = (fun (type a) TypeRep.Type (x : a) : a list -> [ x ]) }
  end

  module Mu :
    NaturalTransformation.S
      with module F1 = Functor.Compose(EndoFunctor.ToFunctor(F))
                         (EndoFunctor.ToFunctor(F))
       and module F2 = EndoFunctor.ToFunctor(F) = struct
    module F1 =
      Functor.Compose (EndoFunctor.ToFunctor (F)) (EndoFunctor.ToFunctor (F))

    module F2 = EndoFunctor.ToFunctor (F)

    type nat_trans = {
      nat : 'a. 'a TypeRep.type_rep -> 'a list list -> 'a list;
    }

    let nat_trans =
      {
        nat =
          (fun (type a) TypeRep.Type (xss : a list list) : a list ->
            List.concat xss);
      }
  end
end

(* ======================================================================== *)

module Option : S = struct
  module F = struct
    include TypeEndoFunctor.Option
    module C = Category.Type
  end

  module Eta :
    NaturalTransformation.S
      with module F1 = Functor.Id(F.C)
       and module F2 = EndoFunctor.ToFunctor(F) = struct
    module F1 = Functor.Id (F.C)
    module F2 = EndoFunctor.ToFunctor (F)

    type nat_trans = { nat : 'a. 'a TypeRep.type_rep -> 'a -> 'a option }

    let nat_trans =
      { nat = (fun (type a) TypeRep.Type (x : a) : a option -> Option.some x) }
  end

  module Mu :
    NaturalTransformation.S
      with module F1 = Functor.Compose(EndoFunctor.ToFunctor(F))
                         (EndoFunctor.ToFunctor(F))
       and module F2 = EndoFunctor.ToFunctor(F) = struct
    module F1 =
      Functor.Compose (EndoFunctor.ToFunctor (F)) (EndoFunctor.ToFunctor (F))

    module F2 = EndoFunctor.ToFunctor (F)

    type nat_trans = {
      nat : 'a. 'a TypeRep.type_rep -> 'a option option -> 'a option;
    }

    let nat_trans =
      {
        nat =
          (fun (type a) TypeRep.Type (x : a option option) : a option ->
            Option.join x);
      }
  end
end

(* ======================================================================== *)

module FromUnitAdjonction (A : UnitAdjonction.S) :
  S
    with module F.C = A.F.C_in
     and type 'a F.map_obj = 'a Functor.Compose(A.F)(A.G).map_obj = struct
  module F :
    EndoFunctor.S
      with module C = A.F.C_in
       and type 'a map_obj = 'a Functor.Compose(A.F)(A.G).map_obj = struct
    include Functor.Compose (A.F) (A.G)
    module C = A.F.C_in
  end

  module Eta :
    NaturalTransformation.S
      with module F1 = Functor.Id(F.C)
       and module F2 = EndoFunctor.ToFunctor(F) = struct
    module F1 = Functor.Id (F.C)
    module F2 = EndoFunctor.ToFunctor (F)

    type nat_trans = {
      nat : 'a. 'a F.C.obj -> ('a F1.map_obj, 'a F2.map_obj) F.C.morph;
    }

    let nat_trans =
      {
        nat =
          (fun (type a)
            (x : a F.C.obj)
            :
            (a F1.map_obj, a F2.map_obj) F.C.morph
          -> A.Eta.nat_trans.nat x);
      }
  end

  module Mu :
    NaturalTransformation.S
      with module F1 = Functor.Compose(EndoFunctor.ToFunctor(F))
                         (EndoFunctor.ToFunctor(F))
       and module F2 = EndoFunctor.ToFunctor(F) = struct
    module F1 =
      Functor.Compose (EndoFunctor.ToFunctor (F)) (EndoFunctor.ToFunctor (F))

    module F2 = EndoFunctor.ToFunctor (F)

    type nat_trans = {
      nat : 'a. 'a F.C.obj -> ('a F1.map_obj, 'a F2.map_obj) F.C.morph;
    }

    let nat_trans =
      {
        nat =
          (fun (type a)
            (x : a F.C.obj)
            :
            (a F1.map_obj, a F2.map_obj) F.C.morph
          ->
            A.G.fmap
              (A.F.map_obj (A.G.map_obj (A.F.map_obj x)))
              (A.F.map_obj x)
              (A.Epsilon.nat_trans.nat (A.F.map_obj x)));
      }
  end
end
