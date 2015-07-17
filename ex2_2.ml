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
    let rec member x = function
      | Empty -> false
      | Tree(a, y, b) ->
	 if Element.lt x y then member x a
	 else if Element.lt y x then member x b
	 else true
    let rec insert x = function
      | Empty -> Tree(Empty, x, Empty)
      | Tree(a, y, b) as s ->
	 if Element.lt x y then Tree(insert x a, y, b)
	 else if Element.lt y x then Tree(a, y, insert x b)
	 else s
    let insert_all xs set =
      BatList.fold_left (fun x y -> insert y x) set xs
    let of_list xs =
      insert_all xs empty
  end

module BetterUnbalancedSet (Element : ORDERED) =
  struct
    include UnbalancedSet (Element)
    let member x = function
      | Empty -> false
      | Tree(a, y, b) as set ->
	 let rec aux x z = function
	   | Empty -> x == z
	   | Tree(a, y, b) ->
	      if Element.lt x y then aux x z a
	      else aux x y b
	 in
	 aux x y set
  end

module Int =
  struct
    open BatPrintf
    include BatInt
    let counter = ref 0
    let lt x y =
      printf "%d < %d\n" x y;
      incr counter;
      x < y
    let eq x y =
      printf "%d = %d\n" x y;
      incr counter;
      x == y
    let gt x y =
      printf "%d > %d\n" x y;
      incr counter;
      x > y
    let print_stats () =
      print_string "count: ";
      print_int !counter;
      print_newline ();
      counter := 0
  end

module IntUnbalancedSet = BetterUnbalancedSet(Int)
    
let () =
  let open Int in
  let open IntUnbalancedSet in
  let set = of_list [2; 1; 4; 3; 5] in
  print_endline "construction of {1, 2, 3, 4, 5}";
  print_stats ();
  assert (member 1 set);
  print_endline "membership of 1";
  print_stats ();
  assert (member 2 set);
  print_endline "membership of 2";
  print_stats ();
  assert (member 3 set);
  print_endline "membership of 3";
  print_stats ();
  assert (member 4 set);
  print_endline "membership of 4";
  print_stats ();
  assert (member 5 set);
  print_endline "membership of 5";
  print_stats ();
  assert (not (member 6 set));
  print_endline "membership of 6 (which is false)";
  print_stats ()
