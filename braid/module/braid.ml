(* Notations *)
type sigma = int;;
type word = sigma list;;

(* Translation *)
let string_of_sigma = string_of_int;;

let string_of_word w =
    let rec aux w = match w with
        [] -> "]"
        |t::[] -> (string_of_sigma t)^"]"
        |t::q -> (string_of_sigma t)^";"^(aux q)
    in "["^(aux w);;


(* Basics of word algewra *)
let inverse w =
    let rec aux w acc = match w with
        [] -> acc
        |t::q -> aux q (-t::acc)
    in aux w [];;

(* TODO obvious simplifications *)
let rec simplify (w:word) :word = match w with
    [] -> []
    |0::q -> simplify q
    |t::0::q -> simplify (t::q)
    |t1::t2::q when t1 = -t2 -> simplify q
    |t::q -> t::(simplify q);;

let is_simplified (w:word) :bool = (w = simplify w);;



(* Testing if a word is pure *)
let max l = match l with
    [] -> 0
    |t::q -> let rec aux l m = match l with
                [] -> m
                |t::q when t>m -> aux q t
                |_::q -> aux q m
             in aux q t;;

let is_pure w =
    let rec aux top bot n read_w w  = match read_w with
        [] when bot = top -> if top < n then aux (top+1) (top+1) n w w else true
        |[] -> false
        |t::q -> let x=abs t in if x=bot-1 then aux top (bot-1) n q w
                                else if x=bot then aux top (bot+1) n q w
                                else aux top bot n q w
    in aux 1 1 (max w) w w;;

(* Rewriting algorithm *)
let rec rewrite w =
    let rec step w = match w with
        []
            -> [], false
        |0::q
            -> let s = step q in fst s, true
        |t1::t2::q when t1 = -t2
            -> let s = step q in fst s, true
        |t1::t2::q when t1 < 0 && t2 > 0 && abs(t1+t2) = 1
            ->  let s = step q in t2::(-t1)::(-t2)::t1::(fst s), true
        |t1::t2::q when t1 < 0 && t2 > 0 && abs(t1+t2) > 1
            -> let s = step q in t2::t1::(fst s), true
        |t::q
            -> let s = step q in t::(fst s), snd s
    in let w, c = step w
    in if c then rewrite w else w;;

let break_down w =
    let rec aux w u v = match w with
        [] -> (inverse u), v
        |t::q when t>0 -> aux q ((-t)::u) v
        |t::q -> aux q u ((-t)::v)
    in aux w [] [];;

let is_empty w =
    let u,v = break_down (rewrite w) in rewrite ((inverse u)@v) = [];;

let is_equiv w1 w2 = is_empty (w1@(inverse w2));;
