module type ORDERED =
  sig
    type t
    val lt : t -> t -> bool
    val leq : t -> t -> bool
    val eq : t -> t -> bool
    val gt : t -> t -> bool
  end

module type HEAP =
  sig
    type elem
    type heap
    val empty : heap
    val is_empty : heap -> bool
    val insert : elem -> heap -> heap
    val merge : heap -> heap -> heap
    val find_min : heap -> elem
    val delete_min : heap -> heap
    val of_list : elem list -> heap
  end

module type STRATEGY =
  sig
    type elem
    type tree =
      | Empty
      | Tree of int * elem * tree * tree
    val singleton : elem -> tree
    val make : elem -> tree -> tree -> tree
    val merge : tree -> tree -> tree
  end
    
exception Not_found

module Heap
	 (Element : ORDERED)
	 (Strategy : STRATEGY with type elem = Element.t)
       : HEAP with type elem = Element.t and type heap = Strategy.tree =
  struct
    type elem = Element.t
    type heap = Strategy.tree
    let empty = Strategy.Empty
    let is_empty = function
      | Strategy.Empty -> true
      | _ -> false
    let merge a b =
      Strategy.merge a b
    let insert x h =
      merge (Strategy.singleton x) h
    let find_min = function
      | Strategy.Empty -> raise Not_found
      | Strategy.Tree(_, x, a, b) -> x
    let delete_min = function
      | Strategy.Empty -> raise Not_found
      | Strategy.Tree(_, x, a, b) -> merge a b
    let of_list xs =
      BatList.fold_left (fun h x -> insert x h) empty xs
  end

module Int : ORDERED with type t = int =
  struct
    include BatInt
    let counter = ref 0
    let lt x y =
      incr counter;
      x < y
    let leq x y =
      incr counter;
      x <= y
    let eq x y =
      incr counter;
      x == y
    let gt x y =
      incr counter;
      x > y
    let print_stats () =
      BatPrintf.printf "comparisons: %d\n" !counter;
      counter := 0
  end

module LeftistStrategy
	 (Element : ORDERED)
       : STRATEGY with type elem = Element.t =
  struct
    type elem = Element.t
    type tree =
      | Empty
      | Tree of int * elem * tree * tree
    let rank = function
      | Empty -> 0
      | Tree(r, _, _, _) -> r
    let singleton x =
      Tree(1, x, Empty, Empty)
    let make x a b =
      if rank a >= rank b then
	Tree(rank b + 1, x, a, b)
      else
	Tree(rank a + 1, x, b, a)
    let rec merge x y =
      match x, y with
      | h, Empty -> h
      | Empty, h -> h
      | (Tree(_, x, a1, b1) as h1),
	(Tree(_, y, a2, b2) as h2) ->
	 if Element.leq x y then
	   make x a1 (merge b1 h2)
	 else
	   make y a2 (merge h1 b2)
  end
    
module WeightBiasedLeftistStrategy
	 (Element : ORDERED)
       : STRATEGY with type elem = Element.t =
  struct
    type elem = Element.t
    type tree =
      | Empty
      | Tree of int * elem * tree * tree
    let weight = function
      | Empty -> 0
      | Tree(w, _, _, _) -> w
    let singleton x =
      Tree(1, x, Empty, Empty)
    let make x a b =
      if weight a >= weight b then
	Tree(weight a + weight b + 1, x, a, b)
      else
	Tree(weight a + weight b + 1, x, b, a)
    let rec merge x y =
      let rec topdown k = function
	| h, Empty -> k h
	| Empty, h -> k h
	| (Tree(_, x, a1, b1) as h1),
	  (Tree(_, y, a2, b2) as h2) ->
	   if Element.leq x y then
	     topdown (fun z -> make x a1 z |> k) (b1, h2)
	   else
	     topdown (fun z -> make y a2 z |> k) (h1, b2)
      in
      topdown BatPervasives.identity (x, y)
  end

module IntLeftistStrategy = LeftistStrategy (Int)
module IntWeightBiasedLeftistStrategy = WeightBiasedLeftistStrategy (Int)
module IntLeftistHeap = Heap (Int) (IntLeftistStrategy)
module IntWeightBiasedLeftistHeap = Heap (Int) (IntWeightBiasedLeftistStrategy)
		     
let () =
  let open BatPrintf in
  let open IntLeftistStrategy in
  let open IntLeftistHeap in
  let heap = of_list [1; 2; 4; 3] in
  assert(heap = Tree(2, 1,
		     Tree(1, 2, Empty, Empty),
		     Tree(1, 3, Tree(1, 4, Empty, Empty), Empty)));
  let open IntWeightBiasedLeftistStrategy in
  let open IntWeightBiasedLeftistHeap in
  let heap = of_list [1; 2; 4; 3] in
  assert(heap = Tree(4, 1,
		     Tree(2, 3, Tree(1, 4, Empty, Empty), Empty),
		     Tree(1, 2, Empty, Empty)));
  printf "success\n"
