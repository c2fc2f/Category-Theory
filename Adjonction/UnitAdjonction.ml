module type S = sig
  module F : Functor.S
  module G : Functor.S with module C_in = F.C_out and module C_out = F.C_in

  module Eta :
    NaturalTransformation.S
      with module F1 = Functor.Id(F.C_in)
       and module F2 = Functor.Compose(F)(G)

  module Epsilon :
    NaturalTransformation.S
      with module F1 = Functor.Compose(G)(F)
       and module F2 = Functor.Id(F.C_out)
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

  module Eta :
    NaturalTransformation.S
      with module F1 = Functor.Id(Category.Type)
       and module F2 = Functor.Compose(F)(G) = struct
    module F1 = Functor.Id (Category.Type)
    module F2 = Functor.Compose (F) (G)

    type nat_trans = { nat : 'a. 'a TypeRep.type_rep -> 'a -> T.t -> 'a * T.t }

    let nat_trans =
      {
        nat =
          (fun (type a) TypeRep.Type (x : a) (t : T.t) : (a * T.t) -> (x, t));
      }
  end

  module Epsilon :
    NaturalTransformation.S
      with module F1 = Functor.Compose(G)(F)
       and module F2 = Functor.Id(Category.Type) = struct
    module F1 = Functor.Compose (G) (F)
    module F2 = Functor.Id (Category.Type)

    type nat_trans = { nat : 'a. 'a F1.C_in.obj -> (T.t -> 'a) * T.t -> 'a }

    let nat_trans =
      {
        nat = (fun (type a) TypeRep.Type ((f : T.t -> a), (t : T.t)) : a -> f t);
      }
  end
end
