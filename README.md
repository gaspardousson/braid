# Braid

Braid is an OCaml module implementing the word reversing algorithm on braids groups to solve the word problem.


### Content

* `braid.ml` : The complete module.

* `braid.pdf` : A brief secondary document in French explaining math behind braids and studying the algorithm and its complexity.


### Import

The module can be called using `#use braid.ml` and `open Braid;;`.


### Documentation

* `string_of_sigma : int -> string` converts a sigma (numbered crossing) into a string, compilable in LaTeX
* `string_of_braid : int -> string` converts a braid (list of sigmas) into a string, compilable in LaTeX
* `inverse : int list -> int list` returns the input's inverse braid
* `is_pure: int list -> bool` returns if the input is a pure braid
* `rewrite: int list -> int list` returns the input's Ore decomposition
* `decompose: int list -> (int list)*(int list)` returns positive braids of an Ore decomposition input
* `is_empty: int list -> bool` returns if the input is equivalent to the empty braid
* `is_equiv: int list -> bool` returns if the first input is equivalent to the second