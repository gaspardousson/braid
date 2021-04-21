type sigma
type word

val string_of_sigma: sigma -> string
val string_of_word: word -> string

val inverse: word -> word

val is_pure: word -> bool

val rewrite: word -> word
val break_down: word -> word*word
val is_empty: word -> bool
val is_equiv: word -> word -> bool