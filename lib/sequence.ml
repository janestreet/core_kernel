module Step = struct
  (* 'a is an item in the sequence, 's is the state that will produce the remainder of
     the sequence *)
  type ('a,'s) t =
    | Done
    | Skip of 's
    | Yield of 'a * 's
end

open Step

(* 'a is an item in the sequence, 's is the state that will produce the remainder of the
   sequence *)
type +_ t =
  | Sequence : 's * ('s -> ('a,'s) Step.t) -> 'a t

type 'a sequence = 'a t

let unfold_step ~init ~f =
  Sequence (init,f)

let unfold ~init ~f =
  unfold_step ~init
    ~f:(fun s ->
      match f s with
      | None -> Step.Done
      | Some(a,s) -> Step.Yield(a,s))

let unfold_with s ~init ~f =
  match s with
  | Sequence(s, next) ->
    Sequence((init, s) ,
             (fun (seed, s) ->
               match next s with
               | Done -> Done
               | Skip s -> Skip (seed, s)
               | Yield(a,s) ->
                 match f seed a with
                 | Done -> Done
                 | Skip seed -> Skip (seed, s)
                 | Yield(a,seed) -> Yield(a,(seed,s))))

let of_list l =
  unfold_step ~init:l
    ~f:(function
        | [] -> Done
        | x::l -> Yield(x,l))


let fold t ~init ~f =
  let rec loop seed v next f =
    match next seed with
    | Done -> v
    | Skip s -> loop s v next f
    | Yield(a,s) -> loop s (f v a) next f
  in
  match t with
  | Sequence(seed, next) -> loop seed init next f

TEST = fold ~f:(+) ~init:0 (of_list [1; 2; 3; 4; 5]) = 15
TEST = fold ~f:(+) ~init:0 (of_list []) = 0

let to_list_rev t =
      fold t ~init:[] ~f:(fun l x -> x::l)

let to_list (Sequence(s,next)) =
  let safe_to_list t =
    Core_list.rev (to_list_rev t)
  in
  let rec to_list s next i =
    if i = 0 then safe_to_list (Sequence(s,next))
    else
    match next s with
    | Done -> []
    | Skip s -> to_list s next i
    | Yield(a,s) -> a::(to_list s next (i-1))
  in
  to_list s next 500

TEST =
  let test_equal l = to_list (of_list l) = l in
  test_equal [] && test_equal [1; 2; 3; 4; 5]
(* The test for longer list is after range *)

let range ?(stride=1) ?(start=`inclusive) ?(stop=`exclusive) start_v stop_v =
  let step =
    match stop with
    | `inclusive when stride >= 0 ->
      fun i -> if i > stop_v then Done else Yield(i, i + stride)
    | `inclusive ->
      fun i -> if i < stop_v then Done else Yield(i, i + stride)
    | `exclusive when stride >= 0 ->
      fun i -> if i >= stop_v then Done else Yield(i,i + stride)
    | `exclusive ->
      fun i -> if i <= stop_v then Done else Yield(i,i + stride)
  in
  let init =
    match start with
    | `inclusive -> start_v
    | `exclusive -> start_v + stride
  in
  unfold_step ~init ~f:step

TEST = to_list (range 0 5) = [0;1;2;3;4]
TEST = to_list (range ~stop:`inclusive 0 5) = [0;1;2;3;4;5]
TEST = to_list (range ~start:`exclusive 0 5) = [1;2;3;4]
TEST = to_list (range ~stride:(-2) 5 1) = [5;3]

(* Test for to_list *)
TEST = to_list (range 0 5000) = Core_list.range 0 5000

(* Functions used for testing by comparing to List implementation*)
let test_to_list s f g =
  to_list (f s) = g (to_list s)

let map t ~f =
  match t with
  | Sequence(seed, next) ->
    Sequence(seed,
             fun seed ->
               match next seed with
               | Done -> Done
               | Skip s -> Skip s
               | Yield(a,s) -> Yield(f a,s))


let mapi t ~f =
  match t with
  | Sequence(s, next) ->
    Sequence((0,s),
              fun (i,s) ->
               match next s with
               | Done -> Done
               | Skip s -> Skip (i,s)
               | Yield(a,s) -> Yield(f i a,(i+1,s)))

let filter t ~f =
  match t with
  | Sequence(seed, next) ->
    Sequence(seed,
             fun seed ->
               match next seed with
               | Done -> Done
               | Skip s -> Skip s
               | Yield(a,s) when f a -> Yield(a,s)
               | Yield (_,s) -> Skip s)


(* For testing, we create a sequence which is equal to 1;2;3;4;5, but
   with a more interesting structure inside*)

let s12345 = map ~f:(fun x -> x / 2) (filter ~f:(fun x -> x mod 2 = 0)
                                        (of_list [1;2;3;4;5;6;7;8;9;10]))

let sempty = filter ~f:(fun x -> x < 0) (of_list [1;2;3;4])

let test f g = test_to_list s12345 f g && test_to_list sempty f g

TEST =
  to_list s12345 = [1; 2; 3; 4; 5] &&
  to_list sempty = []

TEST =
  to_list (unfold_with s12345 ~init:1
             ~f:(fun s _ ->
               if s mod 2 = 0 then
                 Skip (s+1)
               else if s = 5 then
                 Done
               else
                 Yield(s, s+1)))
  = [1;3]


TEST = to_list s12345 = [1; 2; 3; 4; 5]

TEST = test
         (map ~f:(fun i -> -i))
         (Core_list.map ~f:(fun i -> -i))

TEST = test
         (mapi ~f:(fun i j -> j - 2 *i))
         (Core_list.mapi ~f:(fun i j -> j - 2 *i))

TEST = test
         (filter ~f:(fun i -> i mod 2 = 0))
         (Core_list.filter ~f:(fun i -> i mod 2 = 0))

let filteri t ~f =
  map ~f:snd (
  filter (mapi t ~f:(fun i s -> (i,s)))
    ~f:(fun (i,s) -> f i s))

TEST = test
         (filter ~f:(fun i -> i mod 2 = 0))
         (Core_list.filter ~f:(fun i -> i mod 2 = 0))


let length t =
  let rec loop i s next =
    match next s with
    | Done -> i
    | Skip s -> loop i s next
    | Yield(_,s) -> loop (i+1) s next
  in
  match t with
  | Sequence (seed, next) -> loop 0 seed next

TEST = length s12345 = 5 && length sempty = 0


let to_list_rev_with_length t =
      fold t ~init:([],0) ~f:(fun (l,i) x -> (x::l,i+1))

let to_array t =
  let (l,len) = to_list_rev_with_length t in
  match l with
  | [] -> [||]
  | x::l ->
    let a = Array.make len x in
    let rec loop i l =
      match l with
      | [] -> assert (i = -1)
      | x::l -> a.(i) <- x; loop (i-1) l
    in
    loop (len - 2) l;
    a

let find t ~f =
  let rec loop s next f =
    match next s with
    | Done -> None
    | Yield(a,_) when f a -> Some a
    | Yield(_,s) | Skip s -> loop s next f
  in
  match t with
  | Sequence (seed, next) -> loop seed next f

TEST = find s12345 ~f:(fun x -> x = 3) = Some 3 &&
       find s12345 ~f:(fun x -> x = 7) = None

let find_map t ~f =
  let rec loop s next f =
    match next s with
    | Done -> None
    | Yield(a,s) ->
      (match f a with
       | None -> loop s next f
       | some_b -> some_b)
    | Skip s -> loop s next f
  in
  match t with
  | Sequence (seed, next) -> loop seed next f

TEST = find_map s12345 ~f:(fun x -> if x = 3 then Some "a" else None) = Some "a" &&
       find_map s12345 ~f:(fun x -> if x = 7 then Some "a" else None) = None

let for_all t ~f =
  let rec loop s next f =
    match next s with
    | Done -> true
    | Yield(a,_) when not (f a) -> false
    | Yield (_,s) | Skip s -> loop s next f
  in
  match t with
  | Sequence (seed, next) -> loop seed next f

TEST = for_all sempty ~f:(fun _ -> false)
TEST = for_all s12345 ~f:(fun x -> x > 0)
TEST = not (for_all s12345 ~f:(fun x -> x < 5))

let exists t ~f =
  let rec loop s next f =
    match next s with
    | Done -> false
    | Yield(a,_) when f a -> true
    | Yield(_,s) | Skip s -> loop s next f
  in
  match t with
  | Sequence (seed, next) -> loop seed next f

TEST = not (exists sempty ~f:(fun _ -> assert false))
TEST = exists s12345 ~f:(fun x -> x = 5)
TEST = not (exists s12345 ~f:(fun x -> x = 0))

let iter t ~f =
  let rec loop seed next f =
    match next seed with
    | Done -> ()
    | Skip s -> loop s next f
    | Yield(a,s) ->
      begin
        f a;
        loop s next f
      end
  in
  match t with
  | Sequence(seed, next) -> loop seed next f

TEST =
  let l = ref [] in
  iter s12345 ~f:(fun x -> l := x::!l);
  !l = [5;4;3;2;1]

let is_empty t =
  let rec loop s next =
    match next s with
    | Done -> true
    | Skip s -> loop s next
    | Yield _ -> false
  in
  match t with
  | Sequence(seed, next) -> loop seed next

TEST = is_empty sempty
TEST = not (is_empty (of_list [1]))

let mem ?(equal = (=)) t a =
  let rec loop s next a =
    match next s with
    | Done -> false
    | Yield(b,_) when equal a b -> true
    | Yield(_,s) | Skip s -> loop s next a
  in
  match t with
  | Sequence(seed, next) -> loop seed next a

TEST = mem s12345 1
TEST = not (mem s12345 6)

let empty =
  Sequence((), fun () -> Done)

TEST = to_list empty = []

let bind t f =
  unfold_step
    ~f:(function
      | Sequence(seed,next), rest ->
        match next seed with
        | Done ->
          begin
            match rest with
            | Sequence(seed, next) ->
              match next seed with
              | Done -> Done
              | Skip s -> Skip (empty, Sequence(s, next))
              | Yield(a, s) -> Skip(f a, Sequence(s, next))
          end
        | Skip s -> Skip (Sequence(s,next), rest)
        | Yield(a,s) -> Yield(a, (Sequence(s,next) , rest)))
    ~init:(empty,t)

TEST = to_list (bind sempty (fun _ -> s12345)) = []
TEST = to_list (bind s12345 (fun _ -> sempty)) = []
TEST = to_list (bind s12345 (fun x -> of_list [x;-x])) = [1;-1;2;-2;3;-3;4;-4;5;-5]

let return x =
  unfold_step ~init:(Some x)
    ~f:(function
      | None -> Done
      | Some x -> Yield(x,None))

TEST = to_list (return 1) = [1]

include Monad.Make(struct
  type nonrec 'a t = 'a t
  let map = `Custom map
  let bind = bind
  let return = return
end)

let nth s n =
  if n < 0 then None
  else
    let rec loop i s next =
      match next s with
      | Done -> None
      | Skip s -> loop i s next
      | Yield(a,s) -> if i == 0 then Some a else loop (i-1) s next
    in
    match s with
    | Sequence(s,next) ->
      loop n s next

TEST = nth s12345 3 = Some 4
TEST = nth s12345 5 = None

let nth_exn s n =
  if n < 0 then raise (Invalid_argument "Core.Sequence.nth")
  else
  match nth s n with
  | None -> failwith "Sequence.nth"
  | Some x -> x

let merge (Sequence (s1, next1)) (Sequence (s2, next2)) ~cmp =
  let next = function
    | Skip s1, s2 -> Skip (next1 s1, s2)
    | (s1, Skip s2) -> Skip (s1, next2 s2)
    | (Yield (a, s1') as s1), (Yield (b, s2') as s2) ->
      if cmp a b <= 0
      then Yield (a, (Skip s1', s2))
      else Yield (b, (s1, Skip s2'))
    | Done, Done -> Done
    | Yield (a, s1), Done -> Yield (a, (Skip s1, Done))
    | Done, Yield (b, s2) -> Yield (b, (Done, Skip s2))
  in
  Sequence((Skip s1, Skip s2), next)

TEST = to_list (merge (of_list [1;3;4;6])
                      (of_list [2;5;7]) ~cmp:compare) = [1;2;3;4;5;6;7]

let hd s =
 let rec loop s next =
   match next s with
   | Done -> None
   | Skip s -> loop s next
   | Yield(a,_) -> Some a
 in
 match s with
 | Sequence (s,next) -> loop s next

TEST = hd s12345 = Some 1
TEST = hd sempty = None

let hd_exn s =
  match hd s with
  | None -> failwith "hd_exn"
  | Some a -> a

let tl s =
 let rec loop s next =
   match next s with
   | Done -> None
   | Skip s -> loop s next
   | Yield(_,a) -> Some a
 in
 match s with
 | Sequence (s,next) ->
  match loop s next with
    | None -> None
    | Some s -> Some (Sequence(s,next))

TEST = tl sempty = None
TEST =  match tl s12345 with
         | Some l -> to_list l = [2;3;4;5]
         | None -> false

let tl_eagerly_exn s =
  match tl s with
  | None -> failwith "Sequence.tl_exn"
  | Some s -> s

let lift_identity next s =
  match next s with
  | Done -> Done
  | Skip s -> Skip (`Identity s)
  | Yield(a,s) -> Yield(a, `Identity s)

let next s =
  let rec loop s next =
    match next s with
    | Done -> None
    | Skip s -> loop s next
    | Yield(a,s) -> Some (a, Sequence(s, next))
  in
  match s with
  | Sequence(s, next) -> loop s next

TEST = next sempty = None
TEST = match next s12345 with
       | Some (1,l) -> to_list l = [2;3;4;5]
       | _ -> false

let filter_opt s =
  match s with
  | Sequence(s, next) ->
    Sequence(s,
      fun s ->
      match next s with
      | Done -> Done
      | Skip s -> Skip s
      | Yield(None, s) -> Skip s
      | Yield(Some a, s) -> Yield(a, s))

TEST = to_list (filter_opt (of_list [None; Some 1; None ;Some 2; Some 3])) =
             [1;2;3]

let filter_map s ~f =
  filter_opt (map s ~f)

let filter_mapi s ~f =
  filter_map (mapi s ~f:(fun i s -> (i,s)))
    ~f:(fun (i, s) -> f i s)

let split_n_eagerly s n =
  let rec loop s i accum next =
    if i <= 0 then
      (of_list (List.rev accum), Sequence(s,next))
    else
      match next s with
      | Done -> (of_list (List.rev accum), empty)
      | Skip s -> loop s i accum next
      | Yield(a,s) -> loop s (i-1) (a::accum) next
  in
  match s with
  | Sequence(s, next) -> loop s n [] next

TEST =
  let (l,r) = split_n_eagerly s12345 2 in
  to_list l = [1;2] && to_list r = [3;4;5]

let findi s ~f =
  find (mapi s ~f:(fun i s -> (i,s)))
    ~f:(fun (i,s) -> f i s)

let find_exn s ~f =
  match find s ~f with
  | None -> failwith "Sequence.find_exn"
  | Some x -> x

let append s1 s2 =
  match s1, s2 with
  | Sequence(s1, next1), Sequence(s2, next2) ->
    Sequence(`First_list s1,
             function
             | `First_list s1 ->
                begin
                match next1 s1 with
                  | Done -> Skip (`Second_list s2)
                  | Skip s1 -> Skip (`First_list s1)
                  | Yield(a,s1) -> Yield(a, `First_list s1)
                end
             | `Second_list s2 ->
                begin
                match next2 s2 with
                  | Done -> Done
                  | Skip s2 -> Skip (`Second_list s2)
                  | Yield(a,s2) -> Yield(a, `Second_list s2)
                end)

TEST = to_list (append s12345 s12345) = [1;2;3;4;5;1;2;3;4;5]
TEST = to_list (append sempty s12345) = [1;2;3;4;5]

let concat_map s ~f = bind s f

let concat s = concat_map s ~f:Fn.id

let concat_mapi s ~f =
  concat_map (mapi s  ~f:(fun i s -> (i,s)))
    ~f:(fun (i,s) -> f i s)

let zip (Sequence (s1, next1)) (Sequence (s2, next2)) =
  let next = function
    | Yield (a, s1), Yield (b, s2) -> Yield ((a, b), (Skip s1, Skip s2))
    | Done, _
    | _, Done -> Done
    | Skip s1, s2 -> Skip (next1 s1, s2)
    | s1, Skip s2 -> Skip (s1, next2 s2)
  in
  Sequence ((Skip s1, Skip s2), next)

TEST = to_list (zip s12345 sempty) = []
TEST = to_list (zip s12345 (of_list [6;5;4;3;2;1])) = [1,6;2,5;3,4;4,3;5,2]
TEST = to_list (zip s12345 (of_list ["a"])) = [1,"a"]

let zip_full (Sequence(s1,next1)) (Sequence(s2,next2)) =
  let next = function
    | Yield (a, s1), Yield (b, s2) -> Yield (`Both (a, b), (Skip s1, Skip s2))
    | Done, Done -> Done
    | Skip s1, s2 -> Skip (next1 s1, s2)
    | s1, Skip s2 -> Skip (s1, next2 s2)
    | Done, Yield(b, s2) -> Yield((`Right b), (Done, next2 s2))
    | Yield(a, s1), Done -> Yield((`Left a), (next1 s1, Done))
  in
  Sequence ((Skip s1, Skip s2), next)

let bounded_length (Sequence(seed,next)) ~at_most =
  let rec loop i seed next =
    if i > at_most then `Greater
    else
      match next seed with
      | Done -> `Is i
      | Skip seed -> loop i seed next
      | Yield(_, seed) -> loop (i+1) seed next
  in
  loop 0 seed next

let length_is_bounded_by ?(min=(-1)) ?max t =
  let length_is_at_least (Sequence(s,next)) =
     let rec loop s acc =
       if acc >= min then true else
         match next s with
         | Done -> false
         | Skip s -> loop s acc
         | Yield(_,s) -> loop s (acc + 1)
     in loop s 0
  in
  match max with
    | None -> length_is_at_least t
    | Some max ->
      begin
        match bounded_length t ~at_most:max with
        | `Is len when len >= min -> true
        | _ -> false
      end

let iteri s ~f =
  iter (mapi s ~f:(fun i s -> (i, s)))
    ~f:(fun (i, s) -> f i s)

let foldi s ~f ~init =
  fold ~init (mapi s ~f:(fun i s -> (i,s)))
    ~f:(fun acc (i, s) -> f i acc s)

let reduce s ~f =
  match next s with
  | None -> None
  | Some(a, s) -> Some (fold s ~init:a ~f)

let reduce_exn s ~f =
  match reduce s ~f with
  | None -> failwith "Sequence.reduce_exn"
  | Some res -> res

let find_consecutive_duplicate (Sequence(s, next)) ~equal =
  let rec loop last_elt s =
    match next s with
    | Done -> None
    | Skip s -> loop last_elt s
    | Yield(a,s) ->
      match last_elt with
      | Some b when equal a b -> Some (b, a)
      | None | Some _ -> loop (Some a) s
  in
  loop None s

TEST = find_consecutive_duplicate s12345 ~equal:(=) = None
TEST = find_consecutive_duplicate (of_list [1;2;2;3;4;4;5]) ~equal:(=) = Some (2,2)

let remove_consecutive_duplicates s ~equal =
  unfold_with s ~init:None
    ~f:(fun prev a ->
          match prev with
          | Some b when equal a b -> Skip(Some a)
          | None | Some _ -> Yield(a, Some a))

TEST = to_list
         (remove_consecutive_duplicates ~equal:(=) (of_list [1;2;2;3;3;3;3;4;4;5;6;6;7]))
       = [1;2;3;4;5;6;7]
TEST = to_list
         (remove_consecutive_duplicates ~equal:(=) s12345) = [1;2;3;4;5]

TEST = to_list (remove_consecutive_duplicates ~equal:(fun _ _ -> true) s12345) = [1]

let count s ~f =
  length (filter s ~f)

let init n ~f =
  unfold_step ~init:0
    ~f:(fun i ->
      if i >= n then Done
      else Yield(f i, i + 1))

TEST = to_list (init (-1) ~f:(fun _ -> assert false)) = []
TEST = to_list (init 5 ~f:Fn.id) = [0; 1; 2; 3; 4]


let sub s ~pos ~len =
  if pos < 0 || len < 0 then failwith "Sequence.sub";
  match s with
  | Sequence(s, next) ->
    Sequence((0,s),
              (fun (i, s) ->
                 if i - pos >= len then Done
                 else
                   match next s with
                   | Done -> Done
                   | Skip s -> Skip (i, s)
                   | Yield(a, s) when i >= pos -> Yield (a,(i + 1, s))
                   | Yield(_, s) -> Skip(i + 1, s)))


TEST = to_list (sub s12345 ~pos:4 ~len:10) = [5]
TEST = to_list (sub s12345 ~pos:1 ~len:2) = [2;3]
TEST = to_list (sub s12345 ~pos:0 ~len:0) = []

let take s len =
  if len < 0 then failwith "Sequence.take";
  match s with
  | Sequence(s, next) ->
    Sequence((0,s),
              (fun (i, s) ->
                 if i >= len then Done
                 else
                   match next s with
                   | Done -> Done
                   | Skip s -> Skip (i, s)
                   | Yield(a, s) -> Yield (a,(i + 1, s))))

TEST = to_list (take s12345 2) = [1;2]
TEST = to_list (take s12345 0) = []
TEST = to_list (take s12345 9) = [1;2;3;4;5]

let drop s len =
  if len < 0 then failwith "Sequence.drop";
  match s with
  | Sequence(s, next) ->
    Sequence((0,s),
              (fun (i, s) ->
                   match next s with
                   | Done -> Done
                   | Skip s -> Skip (i, s)
                   | Yield(a, s) when i >= len -> Yield (a,(i + 1, s))
                   | Yield(_, s) -> Skip (i+1, s)))

TEST = to_list (drop s12345 2) = [3;4;5]
TEST = to_list (drop s12345 0) = [1;2;3;4;5]
TEST = to_list (drop s12345 9) = []

let take_while s ~f =
  match s with
  | Sequence(s, next) ->
    Sequence(s,
             fun s ->
              match next s with
              | Done -> Done
              | Skip s -> Skip s
              | Yield (a, s) when f a -> Yield(a,s)
              | Yield (_,_) -> Done)

TEST = to_list (take_while ~f:(fun x -> x < 3) s12345) = [1;2]

let drop_while s ~f =
  match s with
  | Sequence(s, next) ->
    Sequence(`Dropping s,
             function
             |`Dropping s ->
               begin
                match next s with
                | Done -> Done
                | Skip s -> Skip (`Dropping s)
                | Yield(a, s) when f a -> Skip (`Dropping s)
                | Yield(a, s) -> Yield(a, `Identity s)
               end
             | `Identity s -> lift_identity next s)

TEST = to_list (drop_while ~f:(fun x -> x < 3) s12345) = [3;4;5]

let shift_right s x =
  match s with
  | Sequence(seed, next) ->
    Sequence(`Consing (seed, x),
             function
               | `Consing (seed, x) -> Yield(x, `Identity seed)
               | `Identity s -> lift_identity next s)

TEST = to_list (shift_right  (shift_right s12345 0) (-1)) = [-1;0;1;2;3;4;5]

let shift_right_with_list s l =
  append (of_list l) s

let shift_left = drop

module Infix = struct
  let (@) = append
end

let intersperse s ~sep =
  match s with
  | Sequence(s, next) ->
    Sequence(`Init s,
             function
             | `Init s ->
               begin
                 match next s with
                 | Done -> Done
                 | Skip s -> Skip (`Init s)
                 | Yield(a, s) -> Yield(a, `Running s)
               end
             | `Running s ->
               begin
                 match next s with
                 | Done -> Done
                 | Skip s -> Skip (`Running s)
                 | Yield(a, s) -> Yield(sep, `Putting(a,s))
               end
             | `Putting(a,s) -> Yield(a,`Running s))

TEST = to_list (intersperse ~sep:'a' (of_list [])) = []
TEST = to_list (intersperse ~sep:'a' (of_list ['b'])) = ['b']
TEST = to_list (intersperse ~sep:(-1) (take s12345 1)) = [1]
TEST = to_list (intersperse ~sep:0 s12345) = [1;0;2;0;3;0;4;0;5]

let repeat x =
  unfold_step ~init:x ~f:(fun x -> Yield(x, x))

TEST = to_list (take (repeat 1) 3) = [1;1;1]

let cycle s =
  concat_map ~f:(fun () -> s) (repeat ())

TEST = to_list (take (cycle s12345) 7) = [1;2;3;4;5;1;2]

let cartesian_product sa sb =
  concat_map sa
    ~f:(fun a -> zip (repeat a) sb)

TEST = to_list (cartesian_product (of_list ['a';'b']) s12345) =
       ['a',1;'a',2;'a',3;'a',4;'a',5;
        'b',1;'b',2;'b',3;'b',4;'b',5]

let singleton x = return x

let delayed_fold s ~init ~f ~finish =
  let rec loop s next finish f =
    fun acc ->
      match next s with
      | Done   -> finish acc
      | Skip s ->  loop s next finish f acc
      | Yield(a, s) -> f acc a ~k:(loop s next finish f)
  in
  match s with
  | Sequence(s, next) -> loop s next finish f init

TEST =
  delayed_fold s12345 ~init:0.0
    ~f:(fun a i ~k ->
      if a <= 5.0 then
        k (a +. (float_of_int i)) else
        a)
    ~finish:(fun _ -> assert false)
  = 6.0

let force_eagerly t = of_list (to_list t)

let memoize (type a) (Sequence (s, next)) =
  let module M = struct
    type t = T of (a, t) Step.t Lazy.t
  end in
  let rec memoize s = M.T (lazy (find_step s))
  and find_step s =
    match next s with
    | Done -> Done
    | Skip s -> find_step s
    | Yield (a, s) -> Yield (a, memoize s)
  in
  Sequence (memoize s, (fun (M.T l) -> Lazy.force l))

TEST =
  let num_computations = ref 0 in
  let t = memoize (unfold ~init:() ~f:(fun () -> incr num_computations; None)) in
  iter t ~f:Fn.id;
  iter t ~f:Fn.id;
  !num_computations = 1

let drop_eagerly s len =
  let rec loop i ~len s next =
    if i >= len then Sequence(s, next)
    else
      match next s with
      | Done -> empty
      | Skip s -> loop i ~len s next
      | Yield(_,s) -> loop (i+1) ~len s  next
  in
  match s with
  | Sequence(s, next) -> loop 0 ~len s next

let drop_while_option (Sequence (s, next)) ~f =
  let rec loop s =
    match next s with
    | Done -> None
    | Skip s -> loop s
    | Yield (x, s) -> if f x then loop s else Some (x, Sequence (s, next))
  in
  loop s

TEST = to_list (drop_eagerly s12345 0) = [1;2;3;4;5]
TEST = to_list (drop_eagerly s12345 2) = [3;4;5]
TEST = to_list (drop_eagerly s12345 5) = []
TEST = to_list (drop_eagerly s12345 8) = []

module Generator = struct

  type 'elt steps = Wrap of ('elt, unit -> 'elt steps) Step.t

  let unwrap (Wrap step) = step

  module T = struct
    type ('a, 'elt) t = ('a -> 'elt steps) -> 'elt steps
    let return x = (); fun k -> k x
    let bind m f = (); fun k -> m (fun a -> let m' = f a in m' k)
    let map m ~f = (); fun k -> m (fun a -> k (f a))
    let map = `Custom map
  end
  include T
  include Monad.Make2 (T)

  let yield e = (); fun k -> Wrap (Yield (e, k))

  let to_steps t = t (fun () -> Wrap Done)

  let run t =
    let init () = to_steps t in
    let f thunk = unwrap (thunk ()) in
    unfold_step ~init ~f

end

BENCH_FUN "Sequence.map_sum_list" =
   let l = Core_list.range 0 100_000 in
   (fun () ->
       let _ =
         Core_list.fold ~f:(+) ~init:0
           (Core_list.map ~f:(fun x -> x * 2) l) in ()

   )

BENCH_FUN "Sequence.map_sum_list_sequence" =
   let l = Core_list.range 0 100_000 in
   (fun () ->
       let _ =
       fold ~f:(+) ~init:0
         (map ~f:(fun x -> x * 2) (of_list l)) in ())

BENCH_FUN "Sequence.map_sum_sequence" =
   let s = range 0 100_000 in
   (fun () ->
       let _ =
       fold ~f:(+) ~init:0
         (map ~f:(fun x -> x * 2) s) in ())

BENCH_FUN "Sequence.map_sum_baseline" =
  let rec loop acc n =
    if n = 0 then acc else
      loop (acc + 2 * n) (n - 1)
  in
  fun () -> let _ = loop 0 100_000 in ()


BENCH_FUN "Sequence.map_filter_sum_list" =
   let l = Core_list.range 0 100_000 in
   (fun () ->
       let _ =
         Core_list.fold ~f:(+) ~init:0
           (Core_list.filter ~f:(fun x -> x mod 2 = 0)
           (Core_list.map ~f:(fun x -> x * 2) l)) in ()

   )

BENCH_FUN "Sequence.map_filter_sum_list_sequence" =
   let l = Core_list.range 0 100_000 in
   (fun () ->
       let _ =
         fold ~f:(+) ~init:0
          (filter ~f:(fun x -> x mod 2 = 0)
            (map ~f:(fun x -> x * 2) (of_list l)))in ())

BENCH_FUN "Sequence.map_filter_sum_sequence" =
   let s = range 0 100_000 in
   (fun () ->
       let _ =
       fold ~f:(+) ~init:0
          (filter ~f:(fun x -> x mod 2 = 0)
            (map ~f:(fun x -> x * 2) s)) in ())

BENCH_FUN "Sequence.map_filter_sum_baseline" =
  let rec loop acc n =
    if n = 0 then acc else
      loop (if n mod 2 = 0 then (acc + 2 * n) else acc) (n - 1)
  in
  fun () -> let _ = loop 0 100_000 in ()
