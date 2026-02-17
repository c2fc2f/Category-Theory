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

(*module List : S = struct
  module F = TypeEndoFunctor.List

  module Eta = struct
    let nat_trans = { nat = (fun TypeRep.Type x -> [ x ]) }
  end

  module Mu = struct
    let nat_trans = { nat = (fun TypeRep.Type xss -> List.concat xss) }
  end
end*)
