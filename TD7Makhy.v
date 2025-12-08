(* Sémantique petit pas                      *)
(* Structural Operational Semantics (SOS)    *)


(* On importe les bibliothèques de Coq utiles pour le TD   *)

Require Import Bool Arith List.
Import List.ListNotations.

(* ========================================================================== *)
(** * Préliminaires *)
(** ** Syntaxe des expressions arithétiques *)

Inductive aexp :=
| Aco : nat -> aexp (** constantes *)
| Ava : nat -> aexp (** variables *)
| Apl : aexp -> aexp -> aexp
| Amu : aexp -> aexp -> aexp
| Amo : aexp -> aexp -> aexp
.

(** ** Syntaxe des expressions booléennes *)

Inductive bexp :=
| Btrue : bexp
| Bfalse : bexp
| Bnot : bexp -> bexp
| Band : bexp -> bexp -> bexp
| Bor : bexp -> bexp -> bexp
| Beq : bexp -> bexp -> bexp    (* test égalité de bexp *)
| Beqnat : aexp -> aexp -> bexp (* test égalité de aexp *)
.

(** ** Syntaxe du langage impératif WHILE *)

Inductive winstr :=
| Skip   : winstr
| Assign : nat -> aexp -> winstr
| Seq    : winstr -> winstr -> winstr
| If     : bexp -> winstr -> winstr -> winstr
| While  : bexp -> winstr -> winstr
.

(* -------------------------------------------------- *)
(** ** États *)

Definition state := list nat.


Fixpoint get (x:nat) (s:state) : nat :=
match x,s with
| 0   , v::_      => v
| S x1, _::l1 => get x1 l1
| _   , _         => 0
end.


Fixpoint update (s:state) (v:nat) (n:nat): state :=
match v,s with
| 0   , a :: l1 => n :: l1
| 0   , nil     => n :: nil
| S v1, a :: l1 => a :: (update l1 v1 n)
| S v1, nil     => 0 :: (update nil v1 n)
end.

(* ----------------------------------------------- *)
(** ** Sémantique fonctionnelle de aexp et de bexp *)

Fixpoint evalA (a: aexp) (s: state) : nat :=
  match a with
  | Aco n => n
  | Ava x => get x s
  | Apl a1 a2 =>  evalA a1 s + evalA a2 s
  | Amu a1 a2 =>  evalA a1 s * evalA a2 s
  | Amo a1 a2 =>  evalA a1 s - evalA a2 s
  end.

Definition eqboolb b1 b2 : bool :=
  match b1, b2  with
  | true , true  => true
  | false, false => true
  | _    , _     => false
  end.

Fixpoint eqnatb n1 n2 : bool :=
  match n1, n2 with
  | O    , O     => true
  | S n1', S n2' => eqnatb n1' n2'
  | _    , _     => false
  end.

Fixpoint evalB (b : bexp) (s : state) : bool :=
  match b with
  | Btrue => true
  | Bfalse => false
  | Bnot b => negb (evalB b s)
  | Band e1 e2 => (evalB e1 s) && (evalB e2 s)
  | Bor e1 e2 => (evalB e1 s) || (evalB e2 s)
  | Beq e1 e2 => eqboolb (evalB e1 s) (evalB e2 s)
  | Beqnat n1 n2 => eqnatb (evalA n1 s) (evalA n2 s)
  end.


(* ========================================================================== *)

(** * SOS (Sémantique opérationnelle à petits pas du langage While *)

Inductive config :=
| Inter : winstr -> state -> config
| Final : state -> config.

(* La relation pour un pas de SOS *)

Inductive SOS_1: winstr -> state -> config -> Prop :=
| SOS1_Skip     : forall s,
                 SOS_1 Skip s (Final s)

| SOS1_Assign   : forall x a s,
                 SOS_1 (Assign x a) s (Final (update s x (evalA a s)))

| SOS1_Seqf     : forall i1 i2 s s1,
                 SOS_1 i1 s (Final s1) ->
                 SOS_1 (Seq i1 i2) s (Inter i2 s1)
| SOS1_Seqi     : forall i1 i1' i2 s s1,
                 SOS_1 i1 s (Inter i1' s1) ->
                 SOS_1 (Seq i1 i2) s (Inter (Seq i1' i2) s1)

| SOS1_If_true  : forall b i1 i2 s,
                 evalB b s = true  ->
                 SOS_1 (If b i1 i2) s (Inter i1 s)
| SOS1_If_false : forall b i1 i2 s,
                 evalB b s = false ->
                 SOS_1 (If b i1 i2) s (Inter i2 s)

| SOS1_While    : forall b i s,
                 SOS_1 (While b i) s (Inter (If b (Seq i (While b i)) Skip) s)
.

(** Fermeture réflexive-transitive de SOS_1 *)
(** Cette sémantique donne toutes les configurations atteignables
    par un (AST de) programme en partant d'un état initial.
 *)

Inductive SOS : config -> config -> Prop :=
| SOS_stop  : forall c, SOS c c
| SOS_again : forall i1 s1 c2 c3,
              SOS_1 i1 s1 c2 -> SOS c2 c3 ->
              SOS (Inter i1 s1) c3.


(* ========================================================================== *)

Definition N0 := Aco 0.
Definition N1 := Aco 1.
Definition N2 := Aco 2.
Definition N3 := Aco 3.
Definition N4 := Aco 4.


(** * I *)

(** *** Calcul du carré avec des additions *)
(** On code dans While un programme Pcarre correspondant à
    while not (i=n) do {i:= 1+i; x:= y+x ; y:= 2+y} *)
Definition Il := 0.
Definition Ir := Ava Il.
Definition Xl := 1.
Definition Xr := Ava Xl.
Definition Yl := 2.
Definition Yr := Ava Yl.

Definition incrI := Assign Il (Apl N1 Ir).
Definition incrX := Assign Xl (Apl Yr Xr).
Definition incrY := Assign Yl (Apl N2 Yr).
Definition corps_carre := Seq incrI (Seq incrX incrY).
Definition Pcarre_2 := While (Bnot (Beqnat Ir (Aco 2))) corps_carre.
Definition Pcarre n := While (Bnot (Beqnat Ir (Aco n))) corps_carre.
(** Nouveau : on peut jouer avec des programmes qui bouclent *)
Definition Pcarre_inf := While Btrue corps_carre.

(* ======================= 3.1.2 DEBUT  ========================*)


(* Question : Démontrer que Pcarre_2 mène d’un état avec i = 0, x = 0 et y = 1 à une configuration intermédiaire où Pcarre_2 peut être exécuté en partant d’un état avec i = 1, x = 1 et y = 3.*)
Lemma SOS_Pcarre_2_1er_tour : SOS (Inter Pcarre_2 [0;0;1]) (Inter Pcarre_2 [1; 1; 3]).
Proof.
   eapply SOS_again. {apply SOS1_While.}
   eapply SOS_again. {apply SOS1_If_true. cbn. reflexivity.}
   eapply SOS_again. {apply SOS1_Seqi. unfold corps_carre. eapply SOS1_Seqf. unfold incrI. eapply SOS1_Assign.}
   eapply SOS_again. {eapply SOS1_Seqi. eapply SOS1_Seqf. unfold incrX. apply SOS1_Assign.}
   eapply SOS_again. {eapply SOS1_Seqf. unfold incrY. apply SOS1_Assign.}
   apply SOS_stop.
   Qed.    
 

(* Question : Indiquer ce que signifie l'énoncé et le démontrer *)
(* Réponse : L'énoncé décrit l'exécution d'un seul tour de boucle pour le programme infini. Il signifie que si l'on part de la configuration où le programme est Pcarre_inf et l'état de la mémoire est [0; 0; 1], après un nombre fini de petits pas (correspondant à l'évaluation de la condition true et à l'exécution du corps de la boucle), on arrive à une nouvelle configuration où :
   - Le programme est revenu au début de la boucle (Pcarre_inf).
   - L'état de la mémoire est devenu [1; 1; 3] (les variables ont été incrémentées).*)

Theorem SOS_Pcarre_inf_1er_tour : SOS (Inter Pcarre_inf [0;0;1]) (Inter Pcarre_inf [1; 1; 3]).
Proof.
  eapply SOS_again. { apply SOS1_While. }
  eapply SOS_again. { apply SOS1_If_true. simpl. reflexivity. }
  eapply SOS_again. {apply SOS1_Seqi. unfold corps_carre. apply SOS1_Seqf. unfold incrI. apply SOS1_Assign.}
  eapply SOS_again. {apply SOS1_Seqi. apply SOS1_Seqf. unfold incrX. apply SOS1_Assign.}
  eapply SOS_again. { apply SOS1_Seqf. unfold incrY. apply SOS1_Assign.}
  apply SOS_stop.
Qed.



Theorem SOS_Pcarre_2_V0 : SOS (Inter Pcarre_2 [0;0;1]) (Final [2;4;5]).
Proof.
 eapply SOS_again. { apply SOS1_While. }
  eapply SOS_again. { apply SOS1_If_true. simpl. reflexivity. }
  eapply SOS_again.
  { apply SOS1_Seqi. unfold corps_carre. apply SOS1_Seqf. unfold incrI. apply SOS1_Assign. }
  eapply SOS_again.
  { apply SOS1_Seqi. apply SOS1_Seqf. unfold incrX. apply SOS1_Assign.}
  eapply SOS_again.
  { apply SOS1_Seqf. unfold incrY. apply SOS1_Assign.}

  eapply SOS_again. { apply SOS1_While. }
  eapply SOS_again. { apply SOS1_If_true. simpl. reflexivity. }
  eapply SOS_again.
  { apply SOS1_Seqi. unfold corps_carre. apply SOS1_Seqf. unfold incrI. apply SOS1_Assign. }
  eapply SOS_again.
  { apply SOS1_Seqi. apply SOS1_Seqf. unfold incrX. apply SOS1_Assign. }
  eapply SOS_again.
  { apply SOS1_Seqf. unfold incrY. apply SOS1_Assign. }
  eapply SOS_again. { apply SOS1_While. }


  eapply SOS_again. { apply SOS1_If_false. simpl. reflexivity. }
  eapply SOS_again. { apply SOS1_Skip. }
  apply SOS_stop.
Qed.

(** Le but de la suite est d'éviter les redites, puis éventuellement
    de considérer le cas général Pcarre. *)

(** Propriété essentielle de SOS, qui a un intérêt pratique. *)
Theorem SOS_trans : forall c1 c2 c3, SOS c1 c2 -> SOS c2 c3 -> SOS c1 c3.
Proof.
  intros c1 c2 c3 H12 H23.
  (* On fait une induction sur la preuve que c1 mène à c2 *)
  induction H12.
  - apply H23. 
  - eapply SOS_again.
    apply H.    
    apply IHSOS.
    apply H23.
Qed.

(* Question : Indiquer la signification du théorème SOS_Pcarre_2_2e_tour. *)
(* Réponse : Ce lemme représente le deuxième tour de boucle du programme Pcarre_2 (qui s'arrête quand i=2).
   - On part de l'état obtenu à la fin du 1er tour : [1; 1; 3] (donc i=1).
   - Le programme vérifie la condition (i!=2, ce qui est Vrai), entre dans la boucle, et exécute les assignations.
   -  On aboutit à l'état [2; 4; 5] (car i devient 1+1=2, x devient 1+3=4, y devient 3+2=5).*)

(** Il n'est pas demandé de faire celui-ci
    (bien qu'un copié-collé d'un lemme précédent fonctionne). *)
Lemma SOS_Pcarre_2_2e_tour : SOS (Inter Pcarre_2 [1; 1; 3]) (Inter Pcarre_2 [2; 4; 5]).
Proof.
Admitted.

(* Question : Indiquer la signification du théorème SOS_Pcarre_2_fini et le démontrer.*)
(* Réponse : Ce théorème représente la terminaison de la boucle While. Nous sommes dans l'état [2; 4; 5], ce qui signifie que la variable i vaut 2. La condition de la boucle Pcarre_2 est not (i = 2).
   - Puisque i vaut 2, i = 2 est Vrai.
   - Donc la condition not (i = 2) est Fausse.
   - La boucle While se termine : elle n'exécute pas son corps et passe à l'instruction suivante (ou termine le programme).
   - Le programme passe donc de l'état Inter à l'état Final sans modifier la mémoire. *)
Theorem SOS_Pcarre_2_fini : SOS (Inter Pcarre_2 [2; 4; 5]) (Final [2; 4; 5]).
Proof.
  eapply SOS_again. { apply SOS1_While. }
  eapply SOS_again. 
  { apply SOS1_If_false. simpl. reflexivity. }
  eapply SOS_again. { apply SOS1_Skip. }
  apply SOS_stop.
Qed.

(* Question : Démontrer que Pcarre_2 mène d’un état avec i = 0, x = 0 et y = 1 à une configuration finale d’état i = 2, x = 4 et y = 5. On pourra utiliser la transitivité de SOS.*)
(** Même énoncé que SOS_Pcarre_2_V0. Utiliser SOS_trans *)
Theorem SOS_Pcarre_2_fin_V1 : SOS (Inter Pcarre_2 [0;0;1]) (Final [2;4;5]).
Proof.
   apply SOS_trans with (Inter Pcarre_2 [1; 1; 3]).
    apply SOS_Pcarre_2_1er_tour. 
   apply SOS_trans with (Inter Pcarre_2 [2; 4; 5]).
    apply SOS_Pcarre_2_2e_tour.
    apply SOS_Pcarre_2_fini.
Qed.


(* ======================= 3.1.2 FIN  ========================*)




(** Généralisation à Pcarre *)

(** On a besoin de deux lemmes arithmétiques, démontrables avec la tactique ring. *)
Lemma Sn_2 n : S n + S n = S (S (n + n)).
Proof. ring. Qed.

Lemma Sn_carre n : S n * S n = S (n + n + n * n).
Proof. ring. Qed.

Definition invar_cc n := [n; n*n; S (n+n)].
Theorem SOS_corps_carre n : SOS (Inter corps_carre (invar_cc n)) (Final (invar_cc (S n))).
Proof.
Admitted.

(** Celui-ci est court mais difficile. Laisser Admitted au début. *)
Fixpoint SOS_seqf i1 i2 s1 s2 (so : SOS (Inter i1 s1) (Final s2)) :
  SOS (Inter (Seq i1 i2) s1) (Inter i2 s2).
Proof.
Admitted.

(** Réutiliser les lemmes précédents (facile et très court). *)
Lemma SOS_corps_carre_inter n i :
  SOS (Inter (Seq corps_carre i) (invar_cc n)) (Inter i (invar_cc (S n))).
Proof.
  apply SOS_seqf.
Admitted.

Lemma eqnatb_refl : forall n, eqnatb n n = true.
Proof.
Admitted.

(** Réutiliser les lemmes précédents (facile). *)
Lemma SOS_Pcarre_tour :
  forall n i, eqnatb i n = false ->
  SOS (Inter (Pcarre n) (invar_cc i)) (Inter (Pcarre n) (invar_cc (S i))).
Proof.
Admitted.

(** Facile *)
Theorem SOS_Pcarre_n_fini :
  forall n, SOS (Inter (Pcarre n) (invar_cc n)) (Final (invar_cc n)).
Proof.
Admitted.

Theorem SOS_Pcarre_2_fin_V2 : SOS (Inter Pcarre_2 [0;0;1]) (Final [2;4;5]).
Proof.
  eapply SOS_trans.
  { apply SOS_Pcarre_tour. reflexivity. }
  eapply SOS_trans.
  { apply SOS_Pcarre_tour. reflexivity. }
  eapply SOS_trans.
  { apply SOS_Pcarre_n_fini. }
  apply SOS_stop.
Qed.

(** On peut dire des choses sur la version qui boucle sans fin. *)
Lemma SOS_Pcarre_inf_tour :
  forall i,
  SOS (Inter Pcarre_inf (invar_cc i)) (Inter Pcarre_inf (invar_cc (S i))).
Proof.
Admitted.

Theorem SOS_Pcarre_inf_i :
  forall i,
  SOS (Inter Pcarre_inf [0; 0; 1]) (Inter Pcarre_inf (invar_cc i)).
Proof.
Admitted.

(** Énoncer et démontrer le théorème général pour Pcarre (****) *)
(**
  Cette partie est plus difficile, voici quelques indications
  entrecoupées de QUESTIONS (numérotées Pcarre_Q1, Pcarre_Q2, etc.,
  avec des indications de difficulté proportionnées au nombre d'étoiles)
  auxquelles il vous est demandé de répondre.

  Il faut tout d'abord bien comprendre la signification de SOS_Pcarre_inf_i.
  On peut énoncer le même théorème en mettant (Pcarre 0), (Pcarre 1), (Pcarre 2)
  ou plus généralement (Pcarre n) à la place de Pcarre_inf.
  Par exemple :

Theorem SOS_Pcarre_2_i :
  forall i,
  SOS (Inter Pcarre_inf [0; 0; 1]) (Inter (Pcarre 2) (invar_cc i)).

Theorem SOS_Pcarre_n_i :
  forall n i,
  SOS (Inter Pcarre_inf [0; 0; 1]) (Inter (Pcarre n) (invar_cc i)).

En fait ces énoncés sont trop gourmands pour être vrais, par exemple
SOS_Pcarre_2_i peut se démontrer pour i valant 0, 1, 2 mais pas au-delà.

* QUESTION Pcarre_Q1 ( ** ) : expliquer pourquoi.

Un bon énoncé pour SOS_Pcarre_n_i serait en fait :
Lemma SOS_Pcarre_n_i_le :
  forall n i, i <= n ->
  SOS (Inter (Pcarre n) [0; 0; 1]) (Inter (Pcarre n) (invar_cc i)).

* QUESTION Pcarre_Q2 ( ** ) : ce qui nous intéresse au final est un
théorème généralisant SOS_Pcarre_2_fin_V1, portant sur l'état final atteint par
(Pcarre n). Donner son énoncé, en disant pourquoi intuitivement
il pourra être une conséquence de SOS_Pcarre_n_i (non encore démontré pour l'instant).

SOS_Pcarre_n_i est démontrable, mais il faut pour cela bien savoir s'y prendre
avec la relation <=, ce qui demande un peu d'entraînement et dépasse
probablement le temps disponible dans le cadre de ce cours.

Nous vous proposons à la place d'utiliser deux astuces possibles.
Vous pouvez choisir l'une au l'autre (ou les deux).

A/ La première astuce consiste à remplacer l'usage de <= par une addition.
En effet, i <= n équivaut à dire que n est de la forme i + d,
en profitant du fait que nos entiers, en particulier d, sont des
entiers naturels (et non pas relatifs).
L'idée est alors de reformuler un énoncé de la forme
forall n i, i <= n ->  P n i
en l'énoncé
forall i d, (i + d) i.

* QUESTION Pcarre_Q3 ( * ) : reformuler l'énoncé SOS_Pcarre_n_i_le
Lemma SOS_Pcarre_n_plus : A_COMPLETER.

(Vous pouvez demander à un enseignant si votre énoncé est correct
avant de continuer).

* QUESTION Pcarre_Q4 ( ****, demande de bien maîtriser les techniques
  de démonstration vues auparavant) :
démontrer SOS_Pcarre_n_i_plus


* QUESTION Pcarre_Q5 ( ** ) :
démontrer le théorème prévu à la question Pcarre_Q2.
Conseil : énoncer préalablement un lemme auxiliaire
portant uniquement sur la variable n, disant que la configuration
Inter (Pcarre n) (invar_cc n) peut être atteinte à partir de la configuration
initiale.

B/ La seconde astuce possible n'a rien d'arithmétique. Elles consiste à
remplacer  SOS_Pcarre_n_i par une disjonction qui elle, est démontrable :
Lemma SOS_Pcarre_n_i_disj :
  forall n i,
    SOS (Inter (Pcarre n) [0; 0; 1]) (Inter (Pcarre n) (invar_cc i)) \/
    SOS (Inter (Pcarre n) [0; 0; 1]) (Inter (Pcarre n) (invar_cc n)).

Au cours de la preuve, n reste fixé.
Schématiquement i est incrémenté de 1 à partir de 0, et à chaque étape
on teste si i = n ou non.

* QUESTION Pcarre_Q6 ( *** ):
Rédiger en français la démonstration de SOS_Pcarre_n_i_disj,
en raisonnant par cas lorsqu'on a une disjonction.
Modèle, avec une hypothèse ou un théorème préalable Dis : A \/ B,
énonçant que l'on a soit A soit B.
" On raisonne par cas sur Dis.
  - soit on a A
    [raisonnement conduisant à démontrer C, en utilisant A]
  - soit on a B
    [raisonnement conduisant à démontrer C, en utilisant B]
  On a ainsi démontré C."

Et pour démontrer une disjonction A \/ B, il suffit de démontrer A
ou de démontrer B (en choisissant bien lequel).

Exercice facile d'entraînement préalable :
démontrer B \/ A à partir de A \/ B.

En Coq ces principes de preuve sont réalisés par les tactiques suivantes :
- raisonnement par cas : tactique destruct, comme d'habitude.
  Sur le modèle ci-dessus : 
  destruct Dis as [HA | HB].
- démontrer A \/ B en choisissant de démontrer A : tactique left.
- démontrer A \/ B en choisissant de démontrer B : tactique right.

* QUESTION Pcarre_Q7 (entraînement facile en Coq) : démontrer
Lemma or_commut : forall (A B : Prop), A \/ B -> B \/ A.

* QUESTION Pcarre_Q8 (court mais *** ) :
  démontrer SOS_Pcarre_n_i_disj en Coq.
Pour tester l'égalité entre i et n, on pourra utiliser
case_eq (eqnatb i n).

* QUESTION Pcarre_Q9 ( * ) : en déduire un lemme ne portant que sur n,
puis le théorème désiré sur l'état finant atteint par (Pcarre n).

*)

(* ========================================================================== *)


(** * II *)


(** ** Définir une version fonctionnelle de SOS_1 *)
Fixpoint f_SOS_1 (i : winstr) (s : state) : config.
Admitted.

(** ** Utilisation de f_SOS_1 pour éviter les eapply SOS_again *)

(** PC = pt de contrôle *)
Definition PC0 := Pcarre_2.
Eval cbn in (f_SOS_1 PC0 [0;0;1]).

(** Il faut un peu désosser le code pour y retrouver les points de contrôle *)

Definition PC2 := Seq corps_carre PC0.
Definition PC1 := If (Bnot (Beqnat Ir (Aco 2))) PC2 Skip.

(** On vérifie la progression *)
Fact fa1 : f_SOS_1 PC0 [0;0;1] = Inter PC1 [0;0;1]. reflexivity. Qed.
Eval cbn in (f_SOS_1 PC1 [0;0;1]).
(** Continuer, on retombe sur PC0 après quelques étapes. *)

(** Utilisation sur un lemme SOS *)
Lemma SOS_Pcarre_2_1er_tour_V1 :
  SOS (Inter Pcarre_2 [0;0;1]) (Inter Pcarre_2 [1; 1; 3]).
Proof.
  change Pcarre_2 with PC0.
  apply SOS_again with (Inter PC1 [0;0;1]).
  { apply SOS1_While. }
Admitted.

(** ** Théorèmes généraux reliant SOS_1 et f_SOS_1 *)

(** Court mais non trivial. *)
Lemma f_SOS_1_corr : forall i s, SOS_1 i s (f_SOS_1 i s).
Proof.
Admitted.

(** Court. Attention : utiliser la tactique injection. *)
Lemma f_SOS_1_compl : forall i s c, SOS_1 i s c -> c = f_SOS_1 i s.
Proof.
Admitted.





