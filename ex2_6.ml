module type ORDERED =
  sig
    type t
    val lt : t -> t -> bool
    val eq : t -> t -> bool
    val gt : t -> t -> bool
  end

module type FINITE_MAP =
  sig
    type key
    type 'a map
    val empty : 'a map
    val bind : key -> 'a -> 'a map -> 'a map
    val bind_all : (key * 'a) list -> 'a map -> 'a map
    val of_list : (key * 'a) list -> 'a map
    val lookup : key -> 'a map -> 'a
    val size : 'a map -> int
  end

exception Not_found
    
module FiniteMap (Key : ORDERED) : FINITE_MAP with type key = Key.t =
  struct
    type key = Key.t
    type 'a tree =
      | Empty
      | Tree of 'a tree * key * 'a * 'a tree
    type 'a map = 'a tree
    let empty = Empty
    let counter = ref 0
    let make_tree a k v b =
      incr counter;
      Tree(a, k, v, b)
    let print_stats () =
      BatPrintf.printf "nodes: %d\n" !counter;
      counter := 0
    let lookup key = function
      | Empty -> raise Not_found
      | Tree(a, k, v, b) as map ->
	 let rec aux x k v = function
	   | Empty ->
	      if x == k then v
	      else raise Not_found
	   | Tree(a, k', v', b) ->
	      if Key.lt x k' then aux x k v a
	      else aux x k' v' b
	 in
	 aux key k v map
    exception Existing
    let bind key value = function
      | Empty -> make_tree Empty key value Empty
      | Tree(_, k, _, _) as map ->
	 let rec aux z = function
	   | Empty ->
	      if key == z then raise Existing
	      else make_tree Empty key value Empty
	   | Tree(a, k, v, b) ->
	      if Key.lt key k then make_tree (aux z a) k v b
	      else make_tree a k v (aux k b)
	 in
	 try aux k map with Existing -> map
    let bind_all pairs map =
      BatList.fold_left (fun x (k, v) -> bind k v x) map pairs
    let of_list pairs =
      bind_all pairs empty
    let rec size = function
      | Empty -> 0
      | Tree(left, _, _, right) ->
	 size left + size right + 1
  end

module Int : ORDERED with type t = int =
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

module IntFiniteMap = FiniteMap(Int)
    
let () =
  let open BatEnum in
  let open BatPrintf in
  let open IntFiniteMap in
  let assert_fail f =
    assert(try ignore (f ()); false with _ -> true)
  in
  assert_fail(fun () -> lookup 0 empty);
  let map = of_list [1, 2; 2, 4; 3, 6; 4, 8; 5, 10] in
  assert_fail(fun () -> lookup 0 map);
  assert(lookup 1 map = 2);
  assert(lookup 2 map = 4);
  assert(lookup 3 map = 6);
  assert(lookup 4 map = 8);
  assert(lookup 5 map = 10);
  assert_fail(fun () -> lookup 6 map);
  let map = of_list [2, 4; 1, 2; 4, 8; 3, 6; 5, 10] in
  assert_fail(fun () -> lookup 0 map);
  assert(lookup 1 map = 2);
  assert(lookup 2 map = 4);
  assert(lookup 3 map = 6);
  assert(lookup 4 map = 8);
  assert(lookup 5 map = 10);
  assert_fail(fun () -> lookup 6 map);
  printf "success\n"
