module type ORDERED =
  sig
    type t
    val lt : t -> t -> bool
    val eq : t -> t -> bool
    val gt : t -> t -> bool
  end

module type SET =
  sig
    type elem
    type set
    val empty : set
    val insert : elem -> set -> set
    val insert_all : elem list -> set -> set
    val of_list : elem list -> set
    val member : elem -> set -> bool
    val complete : elem -> int -> set
    val balanced : elem -> int -> set
    val depth : set -> int
    val size : set -> int
  end			       

module UnbalancedSet (Element : ORDERED) =
  struct
    type elem = Element.t
    type tree =
      | Empty
      | Tree of tree * elem * tree
    type set = tree
    let empty = Empty
    let counter = ref 0
    let make_tree a x b =
      incr counter;
      Tree(a, x, b)
    let print_stats () =
      BatPrintf.printf "nodes: %d\n" !counter;
      counter := 0
    let rec member x = function
      | Empty -> false
      | Tree(a, y, b) ->
	 if Element.lt x y then member x a
	 else if Element.lt y x then member x b
	 else true
    let rec insert x = function
      | Empty -> make_tree Empty x Empty
      | Tree(a, y, b) as s ->
	 if Element.lt x y then make_tree (insert x a) y b
	 else if Element.lt y x then make_tree a y (insert x b)
	 else s
    let insert_all xs set =
      BatList.fold_left (fun x y -> insert y x) set xs
    let of_list xs =
      insert_all xs empty
    let rec complete x d =
      assert(d >= 0);
      if d = 0 then Empty
      else
	let child = complete x (d - 1) in
	make_tree child x child
    let rec balanced x n =
      let create2 m =
	(balanced x (m + 1), balanced x m) in
      assert(n >= 0);
      if n = 0 then Empty
      else
	let m = n - 1 in
	let half = m / 2 in
	let even = (m mod 2) = 0 in
	if even then
	  make_tree (balanced x half) x (balanced x half)
	else
	  let left, right = create2 half in
	  make_tree left x right
    let rec depth = function
      | Empty -> 0
      | Tree(left, _, right) ->
	 max (depth left) (depth right) + 1
    let rec size = function
      | Empty -> 0
      | Tree(left, _, right) ->
	 size left + size right + 1
  end

module BetterUnbalancedSet (Element : ORDERED) =
  struct
    include UnbalancedSet (Element)
    exception Existing
    let insert x = function
      | Empty -> make_tree Empty x Empty
      | Tree(_, z, _) as set ->
	 let rec aux x z = function
	   | Empty ->
	      if x == z then raise Existing
	      else make_tree Empty x Empty
	   | Tree(a, y, b) ->
	      if Element.lt x y then make_tree (aux x z a) y b
	      else make_tree a y (aux x y b)
	 in
	 try aux x z set with Existing -> set
    let insert_all xs set =
      BatList.fold_left (fun x y -> insert y x) set xs
    let of_list xs =
      insert_all xs empty
  end

module Int =
  struct
    include BatInt
    let counter = ref 0
    let lt x y =
      incr counter;
      x < y
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

module IntUnbalancedSet = UnbalancedSet(Int)
module BetterIntUnbalancedSet = BetterUnbalancedSet(Int)
    
let () =
  let open BatEnum in
  let open BatPrintf in
  let open BetterIntUnbalancedSet in
  let x = 0 in
  let assert_fail f =
    assert(try ignore (f ()); false with _ -> true)
  in
  printf "complete: ";
  assert_fail(fun () -> complete x (-1));
  assert(complete x 0 = Empty);
  assert(complete x 1 = Tree(Empty, x, Empty));
  assert(complete x 2 = Tree(
			    Tree(Empty, x, Empty),
			    x,
			    Tree(Empty, x, Empty)));
  assert(complete x 3 = Tree(
			    Tree(
				Tree(Empty, x, Empty),
				x,
				Tree(Empty, x, Empty)),
			    x,
			    Tree(
				Tree(Empty, x, Empty),
				x,
				Tree(Empty, x, Empty))));
  iter (fun n ->
	assert(complete x n |> depth = n)
       ) (4 -- 10);
  printf "success\n";
  printf "balanced: ";
  assert_fail(fun () -> balanced x (-1));
  assert(balanced x 0 = Empty);
  assert(balanced x 1 = Tree(Empty, x, Empty));
  assert(balanced x 2 = Tree(Tree(Empty, x, Empty), x, Empty));
  assert(balanced x 3 = Tree(Tree(Empty, x, Empty), x, Tree(Empty, x, Empty)));
  iter (fun n ->
	assert(balanced x n |> size = n)
       ) (4 -- 10);
  printf "success\n";
