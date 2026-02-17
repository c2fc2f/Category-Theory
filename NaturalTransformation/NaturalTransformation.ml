module type S = sig
  module F1 : Functor.S
  module F2 : Functor.S with module C_in := F1.C_in and module C_out := F1.C_out

  type nat_trans = {
    nat : 'a. 'a F1.C_in.obj -> ('a F1.map_obj, 'a F2.map_obj) F1.C_out.morph;
  }

  val nat_trans : nat_trans
end

(* ======================================================================== *)

module DuplicationFunction : S = struct
  module F1 = struct
    include TypeEndoFunctor.Id
    module C_in = Category.Type
    module C_out = Category.Type
  end

  module F2 = struct
    include TypeEndoFunctor.Square
    module C_in = Category.Type
    module C_out = Category.Type
  end

  type nat_trans = { nat : 'a. 'a TypeRep.type_rep -> 'a -> 'a * 'a }

  let nat_trans =
    { nat = (fun (type a) (_ : a TypeRep.type_rep) (x : a) -> (x, x)) }
end

(* ======================================================================== *)

module Swap : S = struct
  module F1 = Functor.ProductType
  module F2 = Functor.DualProductType

  type nat_trans = {
    nat : 'a. 'a F1.C_in.obj -> 'a F1.map_obj -> 'a F2.map_obj;
  }

  let nat_trans =
    {
      nat =
        (fun (type a)
          (_ : a F1.C_in.obj)
          (Product (x, y) : a F1.map_obj)
          :
          a F2.map_obj
        -> Product (y, x));
    }
end

(* ======================================================================== *)

module Id (F : Functor.S) : S = struct
  module F1 = F
  module F2 = F

  type nat_trans = {
    nat : 'a. 'a F.C_in.obj -> ('a F.map_obj, 'a F.map_obj) F.C_out.morph;
  }

  let nat_trans =
    {
      nat =
        (fun (type a)
          (o : a F.C_in.obj)
          :
          (a F.map_obj, a F.map_obj) F.C_out.morph
        -> F.fmap o o (F.C_in.id o));
    }
end

(* ======================================================================== *)

module ListHead : S = struct
  module F1 = struct
    include TypeEndoFunctor.List
    module C_in = Category.Type
    module C_out = Category.Type
  end

  module F2 = struct
    include TypeEndoFunctor.Option
    module C_in = Category.Type
    module C_out = Category.Type
  end

  type nat_trans = { nat : 'a. 'a TypeRep.type_rep -> 'a list -> 'a option }

  let nat_trans =
    {
      nat =
        (fun (type a) (_ : a TypeRep.type_rep) (l : a list) : a option ->
          List.nth_opt l 0);
    }
end

(* ======================================================================== *)

module OptionToList : S = struct
  module F1 = struct
    include TypeEndoFunctor.Option
    module C_in = Category.Type
    module C_out = Category.Type
  end

  module F2 = struct
    include TypeEndoFunctor.List
    module C_in = Category.Type
    module C_out = Category.Type
  end

  type nat_trans = { nat : 'a. 'a TypeRep.type_rep -> 'a option -> 'a list }

  let nat_trans =
    {
      nat =
        (fun (type a) (_ : a TypeRep.type_rep) (o : a option) : a list ->
          match o with None -> [] | Some x -> [ x ]);
    }
end
