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

exception Not_found

module type BINOMINAL_TREE =
  sig
    type elem
    type t = Node of int * elem * t list
    val leq : elem -> elem -> bool
    val singleton : elem -> t
    val link : t -> t -> t
    val rank : t -> int
    val root : t -> elem
    val children : t -> t list
  end

module BinominalTree (Element : ORDERED) : BINOMINAL_TREE
       with type elem = Element.t =
  struct
    type elem = Element.t
    type t = Node of int * elem * t list
    let leq = Element.leq
    let singleton x =
      Node(0, x, [])
    let link t1 t2 =
      match (t1, t2) with
      | Node(r, x1, c1), Node(_, x2, c2) ->
	 if Element.leq x1 x2 then
	   Node(r + 1, x1, t2 :: c1)
	 else
	   Node(r + 1, x2, t1 :: c2)
    let rank = function
      | Node(r, _, _) -> r
    let root = function
      | Node(_, x, _) -> x
    let children = function
      | Node(_, _, children) -> List.rev children
  end

module BinominalHeap (Tree : BINOMINAL_TREE) : HEAP
       with type elem = Tree.elem
	and type heap = Tree.t list =
  struct
    type elem = Tree.elem
    type heap = Tree.t list
    let empty = []
    let is_empty = function
      | [] -> true
      | _ -> false
    let rec insertTree t ts =
      let open Tree in
      match t, ts with
      | t, [] -> [t]
      | t, (t' :: ts' as ts) ->
	 if rank t < rank t' then
	   t :: ts
	 else
	   insertTree (link t t') ts'
    let insert x h =
      insertTree (Tree.singleton x) h
    let rec merge a b =
      let open Tree in
      match a, b with
      | ts1, [] -> ts1
      | [], ts2 -> ts2
      | (t1 :: ts1' as ts1), (t2 :: ts2' as ts2) ->
	 if rank t1 < rank t2 then
	   t1 :: merge ts1' ts2
	 else if rank t2 < rank t1 then
	   t2 :: merge ts1 ts2'
	 else
	   insertTree (link t1 t2) (merge ts1' ts2')
    let rec removeMinTree = function
      | [] -> raise Not_found
      | [t] -> (t, [])
      | t :: ts ->
	 let open Tree in
	 let t', ts' = removeMinTree ts in
	 if Tree.leq (root t) (root t') then
	   (t, ts)
	 else
	   (t', t :: ts')
    let find_min ts =
      let t, _ = removeMinTree ts in
      Tree.root t
    let delete_min ts =
      let t, ts2 = removeMinTree ts in
      merge (Tree.children t) ts2
    let of_list xs =
      BatList.fold_left (fun h x -> insert x h) empty xs
  end

module BinominalHeapWithDirectFindMin (Tree : BINOMINAL_TREE) : HEAP
       with type elem = Tree.elem
	and type heap = Tree.t list =
  struct
    include BinominalHeap (Tree)
    let find_min ts =
      let rec find min = function
	| [] -> min
	| t :: ts ->
	   let root = Tree.root t in
	   if Tree.leq root min then
	     find root ts
	   else
	     find min ts
      in
      match ts with
      | [] -> raise Not_found
      | t :: ts ->
	 let root = Tree.root t in
	 find root ts
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

module IntBinominalTree = BinominalTree (Int)
module IntBinominalHeap = BinominalHeap (IntBinominalTree)
module IntBinominalHeapWithDirectFindMin = BinominalHeapWithDirectFindMin (IntBinominalTree)

let () =
  let open BatPrintf in
  let open IntBinominalHeap in
  let open IntBinominalTree in
  let heap = of_list [1; 2; 4; 3] in
  assert (heap = [
	     IntBinominalTree.Node(2, 1, [
				       IntBinominalTree.Node(1, 3, [IntBinominalTree.Node(0, 4, [])]);
				       IntBinominalTree.Node(0, 2, [])
				     ])
	   ]);
  assert (find_min heap = 1);
  let open IntBinominalHeapWithDirectFindMin in
  let heap = of_list [1; 2; 4; 3] in
  assert (find_min heap = 1);
  printf "success\n"
