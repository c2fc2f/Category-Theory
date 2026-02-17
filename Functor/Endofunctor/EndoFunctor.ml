(** Signature pour les modules implantant la notion d'endofoncteur *)
module type S = sig
  module C : Category.S
  (** Définition de la catégorie sur laquelle est définie le foncteur *)

  (* Substitution des catégories par C
NB: le (:=) permet en OCaml des équations "destructives". Les modules C_in et C_out sont
remplacés dans la signature par C ET leur définition disparaît de la signature. *)
  include Functor.S with module C_in := C and module C_out := C
end

(* ======================================================================== *)

module ToFunctor (E : S) :
  Functor.S
    with module C_in = E.C
     and module C_out = E.C
     and type 'a map_obj = 'a E.map_obj = struct
  module C_in = E.C
  module C_out = E.C
  include E
end
