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
    module Element : ORDERED with type t = elem
    type heap
    val empty : heap
    val is_empty : heap -> bool
    val insert : elem -> heap -> heap
    val merge : heap -> heap -> heap
    val find_min : heap -> elem
    val delete_min : heap -> heap
    val of_list : elem list -> heap
  end

exception Not_found

module type BINOMINAL_TREE =
  sig
    type elem
    module Element : ORDERED with type t = elem
    type t = Node of elem * t list
    val leq : elem -> elem -> bool
    val singleton : elem -> t
    val link : t -> t -> t
    val rank : t -> int
    val root : t -> elem
    val children : t -> t list
  end

module BinominalTree (Element : ORDERED) : BINOMINAL_TREE with type elem = Element.t =
  struct
    module Element = Element
    type elem = Element.t
    type t = Node of elem * t list
    let leq = Element.leq
    let singleton x =
      Node(x, [])
    let link t1 t2 =
      match (t1, t2) with
      | Node(x1, c1), Node(x2, c2) ->
	 if Element.leq x1 x2 then
	   Node(x1, t2 :: c1)
	 else
	   Node(x2, t1 :: c2)
    let rec rank = function
      | Node(_, []) -> 0
      | Node(_, t :: ts) ->
	 rank t + 1
    let root = function
      | Node(x, _) -> x
    let children = function
      | Node(_, children) -> List.rev children
  end

module BinominalHeap (Tree : BINOMINAL_TREE) : HEAP
       with type elem = Tree.elem
	and type heap = (int * Tree.t) list =
  struct
    module Element = Tree.Element
    type elem = Tree.elem
    type heap = (int * Tree.t) list
    let empty = []
    let is_empty = function
      | [] -> true
      | _ -> false
    let rec insertTree t ts =
      let open Tree in
      match t, ts with
      | (r, t), [] ->
	 [r, t]
      | (r, t), ((r', t') :: ts' as ts) ->
	 if r < r' then
	   (r, t) :: ts
	 else
	   insertTree (r + 1, (link t t')) ts'
    let insert x h =
      insertTree (0, (Tree.singleton x)) h
    let rec merge a b =
      let open Tree in
      match a, b with
      | ts1, [] -> ts1
      | [], ts2 -> ts2
      | ((r1, t1) :: ts1' as ts1), ((r2, t2) :: ts2' as ts2) ->
	 if r1 < r2 then
	   (r1, t1) :: merge ts1' ts2
	 else if r2 < r1 then
	   (r2, t2) :: merge ts1 ts2'
	 else
	   insertTree (r1 + 1, (link t1 t2)) (merge ts1' ts2')
    let rec removeMinTree = function
      | [] -> raise Not_found
      | [r, t] -> (r, t), []
      | (r, t) :: ts ->
	 let open Tree in
	 let (r', t'), ts' = removeMinTree ts in
	 if Tree.leq (root t) (root t') then
	   (r, t), ts
	 else
	   (r', t'), (r, t) :: ts'
    let find_min ts =
      let rec find min = function
	| [] -> min
	| (_, t) :: ts ->
	   let root = Tree.root t in
	   if Tree.leq root min then
	     find root ts
	   else
	     find min ts
      in
      match ts with
      | [] -> raise Not_found
      | (_, t) :: ts ->
	 let root = Tree.root t in
	 find root ts
    let delete_min ts =
      let (r, t), ts2 = removeMinTree ts in
      merge (List.map (fun t -> (r - 1, t)) (Tree.children t)) ts2
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

module ExplicitMin (Heap : HEAP) =
  struct
    module Element = Heap.Element
    type elem = Heap.elem
    type heap =
      | Empty
      | Not_empty of elem * Heap.heap
    let empty = Empty
    let is_empty = function
      | Empty -> true
      | Not_empty(_, _) -> false
    let insert x = function
      | Empty ->
	 Not_empty(x, Heap.insert x (Heap.empty))
      | Not_empty(min, h) ->
	 let heap = Heap.insert x h in
	 let min' = Heap.find_min heap in
	 if Element.leq min min' then
	   Not_empty(min, heap)
	 else
	   Not_empty(min', heap)
    let merge a b =
      match (a, b) with
      | Empty, Empty -> Empty
      | Not_empty(_, _), Empty -> a
      | Empty, Not_empty(_, _) -> b
      | Not_empty(_, h), Not_empty(_, h') ->
	 let merged = Heap.merge h h' in
	 let min = Heap.find_min merged in
	 Not_empty(min, merged)
    let find_min = function
      | Empty -> raise Not_found
      | Not_empty(min, _) -> min
    let delete_min = function
      | Empty -> raise Not_found
      | Not_empty(min, heap) ->
	 let deleted = Heap.delete_min heap in
	 if Heap.is_empty deleted then
	   Empty
	 else
	   let min = Heap.find_min deleted in
	   Not_empty(min, deleted)
    let of_list list =
      let heap = Heap.of_list list in
      if Heap.is_empty heap then
	Empty
      else
	let min = Heap.find_min heap in
	Not_empty(min, heap)
  end

module IntBinominalTree = BinominalTree (Int)
module ExplicitMinIntBinominalHeap = ExplicitMin (BinominalHeap (IntBinominalTree))

let () =
  let open BatPrintf in
  let open ExplicitMinIntBinominalHeap in
  let open IntBinominalTree in
  let heap = of_list [1; 2; 4; 3] in
  assert (heap = Not_empty(1, [2, Node(1, [Node(3, [Node(4, [])]);
					   Node(2, [])])]));
  assert (find_min heap = 1);
  printf "success\n"
