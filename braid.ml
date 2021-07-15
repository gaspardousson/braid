module Braid :
sig

    val string_of_sigma : int -> string
    val string_of_braid : int list -> string
    
    val inverse : int list -> int list
    val is_pure: int list -> bool

    val rewrite: int list -> int list
    val decompose: int list -> int list * int list

    val is_empty: int list -> bool
    val is_equiv: int list -> int list -> bool

end =
struct

let string_of_sigma s = match s with
    |0 -> "\\epsilon"
    |_ when s > 0 -> "\\sigma_{"^(string_of_int s)^"}"
    |_ -> "\\sigma_{"^(string_of_int (-s))^"}^{-1}"

let string_of_braid b =
    let rec aux b = match b with
        |[] -> ""
        |t::q -> (string_of_sigma t)^(aux q)
    in (aux b)

let inverse b =
    let rec aux b acc = match b with
        |[] -> acc
        |t::q -> aux q (-t::acc)
    in aux b []

let is_pure b =
    let max b = match b with
        |[] -> 0
        |t::q -> let rec aux l m = match l with
                    [] -> m
                    |t::q when t>m -> aux q t
                    |_::q -> aux q m
                 in aux q t
    in
    let rec aux top bot n read_w b  = match read_w with
        |[] when bot = top -> if top < n then aux (top+1) (top+1) n b b else true
        |[] -> false
        |0::q -> aux top bot n q b
        |t::q -> let x=abs t in if x=bot-1 then aux top (bot-1) n q b
                                else if x=bot then aux top (bot+1) n q b
                                else aux top bot n q b
    in aux 1 1 (max b) b b

let rewrite b =
    let rec mirror b acc = match b with
        |[] -> acc
        |t::q -> mirror q (t::acc)
    in
    let rec aux b1 b2 = match b1 with
        |[] -> b2
        |0::q1 -> (match b2 with
            [] -> aux q1 []
            |h::q2 -> aux (h::q1) q2)
        |i::j::q1 when i<0 && j>0 && abs(i+j)=0 -> (match b2 with
            [] -> aux q1 []
            |h::q2 -> aux (h::q1) q2)
        |i::j::q1 when i<0 && j>0 && abs(i+j)=1 -> (match b2 with
            [] -> aux (j::(-i)::(-j)::i::q1) []
            |h::q2 -> aux (h::j::(-i)::(-j)::i::q1) q2)
        |i::j::q1 when i<0 && j>0 && abs(i+j)>1 -> (match b2 with
            [] -> aux (j::i::q1) []
            |h::q2 -> aux (h::j::i::q1) q2)
        |h::q -> aux q (h::b2)
    in mirror (aux b []) []

let decompose b =
    let rec aux b = match b with
        |h::q when h>0 -> let b1,b2 = aux q in h::b1,b2
        |b2 -> [],b2
    in let b1,b2 = aux b in b1,inverse b2


let is_empty b = (is_pure b) && (let b1,b2 = decompose (rewrite b) in rewrite ((inverse b1)@b2) = [])
let is_equiv b1 b2 = is_empty (b1@(inverse b2))

end;;