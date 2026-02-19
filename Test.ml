type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree

let node x y = Node (x, y)

module WriterReaderUnit = UnitAdjonction.WriterReader (struct
  type t = int
end)

module IntStateMonad = Monad.FromUnitAdjonction (WriterReaderUnit)

module DerivedIntStateMonad = TypeMonad.Helper (struct
  type 'a map_obj = int -> 'a * int

  let fmap f x = IntStateMonad.F.fmap TypeRep.Type TypeRep.Type f x
  let eta x = IntStateMonad.Eta.nat_trans.nat TypeRep.Type x
  let mu x = IntStateMonad.Mu.nat_trans.nat TypeRep.Type x
end)

let rec num t =
  match t with
  | Leaf _ -> fun s -> (Leaf s, s + 1)
  | Node (l, r) ->
      let open DerivedIntStateMonad in
      node <$> num l <*> num r

let v =
  Node
    ( Leaf '1',
      Node
        (Node (Leaf '2', Node (Leaf '2', Leaf '3')), Node (Leaf '2', Leaf '3'))
    )

let res4 = num v 0

(* ------------------------------------------------------------------------ *)

let s = IntStateMonad.Eta.nat_trans.nat TypeRep.Type 5
let inc = IntStateMonad.F.fmap TypeRep.Type TypeRep.Type (fun x -> x + 1)
let s1 = inc s
