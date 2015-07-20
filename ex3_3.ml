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
    type heap =
      | Empty
      | Tree of int * elem * heap * heap
    val empty : heap
    val is_empty : heap -> bool
    val insert : elem -> heap -> heap
    val merge : heap -> heap -> heap
    val find_min : heap -> elem
    val delete_min : heap -> heap
    val of_list : elem list -> heap
    val rank : heap -> int
    val make_tree : elem -> heap -> heap -> heap
    val from_list : elem list -> heap
  end

exception Not_found

module Heap (Element : ORDERED) : HEAP with type elem = Element.t =
  struct
    type elem = Element.t
    type heap =
      | Empty
      | Tree of int * elem * heap * heap
    let empty = Empty
    let is_empty = function
      | Empty -> true
      | Tree(_, _, _, _) -> false
    let rank = function
      | Empty -> 0
      | Tree(r, _, _, _) -> r
    let make_tree x a b =
      if rank a >= rank b then
	Tree(rank b + 1, x, a, b)
      else
	Tree(rank a + 1, x, b, a)
    let rec merge x y =
      match x, y with
      | h, Empty -> h
      | Empty, h -> h
      | (Tree(_, x, a1, b1) as h1), (Tree(_, y, a2, b2) as h2) ->
	 if Element.leq x y then
	   make_tree x a1 (merge b1 h2)
	 else
	   make_tree y a2 (merge h1 b2)
    let rec insert x = function
      | Empty -> Tree(1, x, Empty, Empty)
      | Tree(_, y, a, b) as h ->
	 if Element.leq x y then
	   make_tree x a h
	 else
	   make_tree y a (insert x b)
    let find_min = function
      | Empty -> raise Not_found
      | Tree(_, x, a, b) -> x
    let delete_min = function
      | Empty -> raise Not_found
      | Tree(_, x, a, b) -> merge a b
    let of_list xs =
      BatList.fold_left (fun h x -> insert x h) empty xs
    let from_list xs =
      let singletons = BatList.map (fun x -> Tree(1, x, Empty, Empty)) xs in
      let rec path = function
	| [] -> []
	| [a] -> [a]
	| a :: b :: rest -> (merge a b) :: path rest
      in
      let rec iter = function
	| [] -> Empty
	| [a] -> a
	| rest -> path rest |> iter
      in
      iter singletons
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

module IntHeap = Heap(Int)
		     
let () =
  let open BatPrintf in
  let open IntHeap in
  assert(from_list [] |> is_empty);
  assert(not (from_list [1] |> is_empty));
  let heap = from_list [1; 2; 4] in
  assert(heap = Tree(2, 1,
		     Tree(1, 2, Empty, Empty),
		     Tree(1, 4, Empty, Empty)));
  let heap = from_list [1; 2; 4; 3] in
  assert(heap = Tree(2, 1,
		     Tree(1, 2, Empty, Empty),
		     Tree(1, 3, Tree(1, 4, Empty, Empty), Empty)));
  printf "success\n"
