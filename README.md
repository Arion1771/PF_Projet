# PF_Projet — Mini langage WHILE en OCaml et Coq

Ce dépôt contient une implémentation d'un mini langage impératif de type WHILE, son analyseur via combinateurs, un interprète OCaml avec jeux de tests, ainsi que deux TD Coq (sémantique naturelle et petit-pas).

## Contenu des fichiers
- `anacomb.ml` : bibliothèque de combinateurs d'analyse récursive descendante (terminaux, choix, répétition) et utilitaires `list_of_string`.
- `partie_1.ml` : définitions des AST (`aexp`, `instr`, `prog`, `state`) et rappels de grammaires (exercices 1.1.x) + schéma de sémantique (ex. 1.2.1).
- `2_1_2.ml` : premier parseur du langage sans opérateurs booléens complexes (if/while/assign/skip, variables `a`..`d` et valeurs 0/1) avec fonctions de rendu d'AST et jeux de tests basiques.
- `2_1_3.ml` : parseur enrichi gérant disjonction/conjonction/négation booléenne via `+`, `.`, `!` et parenthèses ; mêmes tests adaptés.
- `2_1_4.ml` : parseur tolérant les espaces/retours ligne/tabulations autour des lexèmes, avec tests incluant du code formaté sur plusieurs lignes.
- `2_2_1.ml` : sémantique fonctionnelle, fonction `run` pour exécuter un programme source respectant la grammaire WHILEb-- sur un état initial, et une batterie de tests illustratifs.
- `2_2_2.ml` : sémantique fonctionnelle, fonction `run` pour exécuter un programme source respectant la grammaire WHILEb sur un état initial, et une batterie de tests illustratifs.
- `Preuves.v` : TD Coq sur la sémantique opérationnelle à petits pas (SOS) du même langage, avec définitions `config`, relation `SOS_1`, etc.

## Exécuter / tester le projet OCaml

**Pré-requis :** OCaml installé (toplevel `ocaml`). Pas de dépendance externe.

### 1) Tester uniquement les parseurs
```bash
ocaml 2_1_2.ml   # grammaire de base
ocaml 2_1_3.ml   # grammaire avec + . !
ocaml 2_1_4.ml   # grammaire tolérant les blancs
```

Chaque fichier affiche l'AST ou signale une erreur de parsing selon les cas de test inclus.

### 2) Tester les parseurs avec les états
```bash
ocaml 2_2_1.ml   # parseurs WHILEb-- avec etats
ocaml 2_2_2.ml   # parseurs WHILEb avec etats
```

### 3) Exécuter votre propre programme
```bash
ocaml 2_2_1.ml
# tracez vos propres appels après chargement :
let s0 = [0;0;0;0];;
let _ = run "a:=1; w(a){ a:=0; b:=1 }" s0;;
```

`run <code> <etat_initial>` renvoie l'état final et affiche programme/AST/états.

```bash
ocaml 2_2_2.ml
# tracez vos propres appels après chargement :
let s0 = [0;0;0;0];;
let _ = run "a:=1; w(a){ a:=0; b:=1 }" s0;;
```

`run <code> <etat_initial>` renvoie l'état final et affiche programme/AST/états.

## Travailler avec les TD Coq
Compilation en ligne de commande :
```bash
coqc Preuves.v
```

## Questions traitées et répartition

### Exercices OCaml :
- 1.1.1 : Réalisé par Matthieu, Sami et Marie  
- 1.1.2 : Réalisé par Matthieu, Sami et Marie  
- 1.1.3 : Réalisé par Matthieu, Sami et Marie  
- 1.1.4 : Réalisé par Matthieu et Sami  
- 1.2.1 : Réalisé par Matthieu, Sami et Marie  
- 2.1.1 : Réalisé par Matthieu, Sami et Marie  
- 2.1.2 : Réalisé par Matthieu, Sami et Marie  
- 2.1.3 : Réalisé par Matthieu et Sami  
- 2.1.4 : Réalisé par Marie  
- 2.2.1 : Réalisé par Sami  
- 2.2.2 : Réalisé par Sami  

### Exercices Coq :
- TD6 : Réalisé par Matthieu, Sami et Marie  
- TD7 : Réalisé par Marie  
