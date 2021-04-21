# Braid

Braid est un module OCaml implémentant l'algorithme de retournement qui résout le problème du mot dans le cadre du groupe des tresses.

### Contenu

* `braid.pdf` : Document résumant les bases de la théorie mathématique des tresses et explicitant le principe de l'algorithme de retournement, accompagné d'une étude expérimentale de sa complexité.

* `braid.ml` : Ensemble du code du module.

* `is_equal` : Programme exécutable prenant comme argument deux mots de tresse et répondant à la question de leur équivalence.

### Utilisation

Le module peut être utilisé en étant appelé avec `#use braid.ml`. Son installation via `opam` n'a pas encore été configurée.

Le programme exécutable doit être appelé via un terminal et répond à la question de l'équivalence de deux mots de tresse donnés en argument sous la forme de listes d'entiers entre guillemets.
