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
        |0::q -> aux top bot n q w
        |t::q -> let x=abs t in if x=bot-1 then aux top (bot-1) n q w
                                else if x=bot then aux top (bot+1) n q w
                                else aux top bot n q w
    in aux 1 1 (max w) w w;;

(* Rewriting algorithm *)
let rewrite w =
    let rec mirror w acc = match w with
        [] -> acc
        |t::q -> mirror q (t::acc)
    in
    let rec aux w1 w2 = match w1 with
        [] -> w2
        |0::q1 -> (match w2 with
            [] -> aux q1 []
            |h::q2 -> aux (h::q1) q2)
        |i::j::q1 when i<0 && j>0 && abs(i+j)=0 -> (match w2 with
            [] -> aux q1 []
            |h::q2 -> aux (h::q1) q2)
        |i::j::q1 when i<0 && j>0 && abs(i+j)=1 -> (match w2 with
            [] -> aux (j::(-i)::(-j)::i::q1) []
            |h::q2 -> aux (h::j::(-i)::(-j)::i::q1) q2)
        |i::j::q1 when i<0 && j>0 && abs(i+j)>1 -> (match w2 with
            [] -> aux (j::i::q1) []
            |h::q2 -> aux (h::j::i::q1) q2)
        |h::q -> aux q (h::w2)
    in mirror (aux w []) [];;

let break_down w =
    let rec aux w u v = match w with
        [] -> (inverse u), v
        |t::q when t>0 -> aux q ((-t)::u) v
        |t::q -> aux q u ((-t)::v)
    in aux w [] [];;

let is_empty w =
    let u,v = break_down (rewrite w) in rewrite ((inverse u)@v) = [];;

let is_equiv w1 w2 = is_empty (w1@(inverse w2));;
