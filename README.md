PF_Projet — Mini langage WHILE en OCaml et Coq
================================================

Ce dépôt contient une implémentation d'un mini langage impératif de type WHILE, son analyseur via combinateurs, un interprète OCaml avec jeux de tests, ainsi que deux TD Coq (sémantique naturelle et petit-pas).

Contenu des fichiers
--------------------
- `anacomb.ml` : bibliothèque de combinateurs d'analyse récursive descendante (terminaux, choix, répétition) et utilitaires `list_of_string`.
- `partie_1.ml` : définitions des AST (`aexp`, `instr`, `prog`, `state`) et rappels de grammaires (exercices 1.1.x) + schéma de sémantique (ex. 1.2.1).
- `2_1_2.ml` : premier parseur du langage sans opérateurs booléens complexes (if/while/assign/skip, variables `a`..`d` et valeurs 0/1) avec fonctions de rendu d'AST et jeux de tests basiques.
- `2_1_3.ml` : parseur enrichi gérant disjonction/conjonction/négation booléenne via `+`, `.`, `!` et parenthèses ; mêmes tests adaptés.
- `2_1_4.ml` : parseur tolérant les espaces/retours ligne/tabulations autour des lexèmes, avec tests incluant du code formaté sur plusieurs lignes.
- `2_2.ml` : sémantique fonctionnelle (évaluation arith/booléenne, exécution d'instructions), fonction `run` pour exécuter un programme source sur un état initial, et une batterie de tests illustratifs.
- `TD6Miaouriz.v` : TD Coq sur la sémantique naturelle du langage WHILE (états en listes de `nat`, fonctions `get/update`, dérivations et propriétés).
- `TD7Makhy.v` : TD Coq sur la sémantique opérationnelle à petits pas (SOS) du même langage, avec définitions `config`, relation `SOS_1`, etc.
- `README.md` : ce document.

Exécuter / tester le projet OCaml
----------------------------------
Pré-requis : OCaml installé (toplevel `ocaml`). Pas de dépendance externe.

1) Lancer tous les tests OCaml (parsing + exécution) :
```bash
ocaml 2_2.ml
```
Ce chargement `#use` automatiquement `anacomb.ml`, `partie_1.ml` et `2_1_4.ml`, construit l'interpréteur, puis exécute la suite de tests définie en fin de fichier (assignations, if/while, cas limites).

2) Tester uniquement les parseurs :
```bash
ocaml 2_1_2.ml   # grammaire de base
ocaml 2_1_3.ml   # grammaire avec + . !
ocaml 2_1_4.ml   # grammaire tolérant les blancs
```
Chaque fichier affiche l'AST ou signale une erreur de parsing selon les cas de test inclus.

3) Exécuter votre propre programme :
```bash
ocaml 2_2.ml
# tracez vos propres appels après chargement :
let s0 = [0;0;0;0];;
let _ = run "a:=1; w(a){ a:=0; b:=1 }" s0;;
```
`run <code> <etat_initial>` renvoie l'état final et affiche programme/AST/états.

Travailler avec les TD Coq
--------------------------
- Compilation en ligne de commande :
```bash
coqc TD6Miaouriz.v
coqc TD7Makhy.v
```
- Ouverture interactive : `coqide TD6Miaouriz.v` / `coqide TD7Makhy.v`.

Questions traitées et répartition
---------------------------------
- Exercices OCaml : 1.1.x (définitions d'AST et grammaires), 2.1.x (parseurs successifs), 2.2.1 (sémantique fonctionnelle + interprète). Réalisé par l'équipe projet (OCaml).
- TD6 Coq (sémantique naturelle) : fichier `TD6Miaouriz.v` — contribution Miaouriz.
- TD7 Coq (SOS petit pas) : fichier `TD7Makhy.v` — contribution Makhy.

