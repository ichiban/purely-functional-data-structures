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
  end

module BetterUnbalancedSet (Element : ORDERED) =
  struct
    include UnbalancedSet (Element)
    exception Existing
    let insert x set =
      let rec aux x = function
	| Empty -> make_tree Empty x Empty
	| Tree(a, y, b) ->
	   if Element.lt x y then make_tree (aux x a) y b
	   else if Element.lt y x then make_tree a y (aux x b)
	   else raise Existing
      in
      try aux x set with Existing -> set
    let insert_all xs set =
      BatList.fold_left (fun x y -> insert y x) set xs
    let of_list xs =
      insert_all xs empty
  end

module Int =
  struct
    open BatPrintf
    include BatInt
    let lt x y =
      x < y
    let eq x y =
      x == y
    let gt x y =
      x > y
  end

module IntUnbalancedSet = UnbalancedSet(Int)
module BetterIntUnbalancedSet = BetterUnbalancedSet(Int)
    
let () =
  let list = [2; 1; 4; 3; 5; 2; 1; 4; 3; 5] in
  print_string "list: ";
  BatList.print BatInt.print BatInnerIO.stdout list;
  print_newline ();
  let open IntUnbalancedSet in
  let set = of_list list in
  print_endline "construction of {1, 2, 3, 4, 5}";
  print_stats ();
  assert (member 1 set);
  assert (member 2 set);
  assert (member 3 set);
  assert (member 4 set);
  assert (member 5 set);
  assert (not (member 6 set));
  counter := 0;
  let open BetterIntUnbalancedSet in
  let set = of_list list in
  print_endline "better construction of {1, 2, 3, 4, 5}";
  print_stats ();
  assert (member 1 set);
  assert (member 2 set);
  assert (member 3 set);
  assert (member 4 set);
  assert (member 5 set);
  assert (not (member 6 set))
