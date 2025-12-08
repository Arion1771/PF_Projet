(*Preuves Rocq TD6/TD7*)

Require Import Bool Arith List.
Import List.ListNotations.

(** ** Syntaxe des expressions arithétiques *)

Inductive aexp :=
| Aco : nat -> aexp (* constantes *)
| Ava : nat -> aexp (* variables *)
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
| Beq : bexp -> bexp -> bexp (* test égalité de bexp *)
| Beqnat : aexp -> aexp -> bexp (* test égalité d'aexp *)
.

(** ** Syntaxe du langage impératif WHILE *)

Inductive winstr :=
| Skip   : winstr
| Assign : nat -> aexp -> winstr
| Seq    : winstr -> winstr -> winstr
| If     : bexp -> winstr -> winstr -> winstr
| While  : bexp -> winstr -> winstr
.

(** ** Quelques listes/états pour faire des tests *)
(** Ci-dessous, S1 est un état dans lequel la variable numéro 0
    vaut 1, la variable numéro 1 vaut 2, et toutes les autres
    valent 0' (valeur par défaut).                                      *)
(** Plus généralement, une variable (Ava i) étant représentée par le
    numéro i, sa valeur dans un état S est la valeur en ieme position
    de la liste qui représente cet état S. *)

Definition state := list nat.

Definition S1 := [1; 2].
Definition S2 := [0; 3].
Definition S3 := [0; 7; 5; 41].

(** * Sémantique *)
(** On reprend les sémantiques fonctionnelles
    des expressions artihmétiques et booléennes      *)

(** La fonction get x s rend la valeur de x dans s. *)
(** Elle rend 0 par défaut, par exemple si la variable
    n'est pas définie/initialisée    *)

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

(** ** Sémantique fonctionnelle de aexp*)
Fixpoint evalA (a: aexp) (s: state) : nat :=
  match a with
  | Aco n => n
  | Ava x => get x s
  | Apl a1 a2 =>  evalA a1 s + evalA a2 s
  | Amu a1 a2 =>  evalA a1 s * evalA a2 s
  | Amo a1 a2 =>  evalA a1 s - evalA a2 s
  end.


(** ** Sémantique fonctionnelle de Baexp*)

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

(** Pour définir plus facilement des expressions de test on prédéfinit
    des constantes entières ... *)

Definition N0 := Aco 0.
Definition N1 := Aco 1.
Definition N2 := Aco 2.
Definition N3 := Aco 3.
Definition N4 := Aco 4.

(** ...  et des variables *)

Definition X := Ava 1.
Definition Y := Ava 2.
Definition Z := Ava 3.

(* =============================== DEBUT PREUVES TD6 ==================================*)


(** ** Version relationnelle, appelée "sémantique naturelle" *)

(** Vu dans le CM précédent.
    La sémantique naturelle (ou sémantique opérationnelle à grands pas)
    du langage WHILE est donnée sous la forme d'un prédicat inductif. *)

Inductive SN: winstr -> state -> state -> Prop :=
| SN_Skip        : forall s,
                   SN Skip s s
| SN_Assign      : forall x a s,
                   SN (Assign x a) s (update s x (evalA a s))
| SN_Seq         : forall i1 i2 s s1 s2,
                   SN i1 s s1 -> SN i2 s1 s2 -> SN (Seq i1 i2) s s2
| SN_If_true     : forall b i1 i2 s s1,
                   (evalB b s = true)  ->  SN i1 s s1 -> SN (If b i1 i2) s s1
| SN_If_false    : forall b i1 i2 s s2,
                   (evalB b s = false) ->  SN i2 s s2 -> SN (If b i1 i2) s s2
| SN_While_false : forall b i s,
                   (evalB b s = false) ->  SN (While b i) s s
| SN_While_true  : forall b i s s1 s2,
                   (evalB b s = true)  ->  SN i s s1 -> SN (While b i) s1 s2 ->
                   SN (While b i) s s2
.


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


Theorem reduction_Pcarre_2 : SN (Pcarre_2) [0;0;1] [2;4;5].
Proof.
  cbv [Pcarre_2]. cbv [corps_carre]. cbv [incrI]. cbv [incrX]. cbv [incrY].
  eapply SN_While_true.
  - cbn. reflexivity.
  - eapply SN_Seq. apply SN_Assign. eapply SN_Seq. apply SN_Assign. apply SN_Assign.
  - cbn. eapply SN_While_true.
    -- cbn. reflexivity.
    -- eapply SN_Seq.
       --- apply SN_Assign.
       --- eapply SN_Seq. apply SN_Assign. apply SN_Assign.
    -- cbn. eapply SN_While_false. cbn. reflexivity.
Qed.


(* SN et SN' *)
Inductive SN': winstr -> state -> state -> Prop :=
| SN'_Skip        : forall s,
                    SN' Skip s s
| SN'_Assign      : forall x a s,
                    SN' (Assign x a) s (update s x (evalA a s))
| SN'_Seq         : forall i1 i2 s s1 s2,
                    SN' i1 s s1 -> SN' i2 s1 s2 -> SN' (Seq i1 i2) s s2
| SN'_If_true     : forall b i1 i2 s s1,
                    (evalB b s = true)  ->  SN' i1 s s1 -> SN' (If b i1 i2) s s1
| SN'_If_false    : forall b i1 i2 s s2,
                    (evalB b s = false) ->  SN' i2 s s2 -> SN' (If b i1 i2) s s2
| SN'_While_false : forall b i s,
                    (evalB b s = false) ->  SN' (While b i) s s
| SN'_While_true  : forall b i s s1,
                    (evalB b s = true)  ->  SN' (Seq i (While b i)) s s1 ->
                    SN' (While b i) s s1
.


Inductive SN1_Seq i1 i2 s s2 : Prop :=
| SN1_Seq_intro : forall s1,
                  SN i1 s s1 -> SN i2 s1 s2 -> SN1_Seq i1 i2 s s2
.

Inductive SN1_trivial (s s1 : state) : Prop := Triv : SN1_trivial s s1.

Definition dispatch (i: winstr) : state -> state -> Prop :=
  match i with
  | Seq i1 i2 => SN1_Seq i1 i2
  | _ => SN1_trivial
  end.

Definition SN_inv {i s s2} (sn : SN i s s2) : dispatch i s s2 :=
  match sn with
  | SN_Seq i1 i2 s s1 s2 sn1 sn2 =>
    SN1_Seq_intro _ _ _ _ s1 sn1 sn2
  | _ => Triv _ _
  end.

Lemma inv_Seq : forall {i1 i2 s s2}, SN (Seq i1 i2) s s2 -> SN1_Seq i1 i2 s s2.
Proof.
  intros * sn. apply (SN_inv sn).
Qed.


(** La direction suivante ne pose pas de nouvelle difficulté *)
Lemma SN_SN' : forall i s s1, SN i s s1 -> SN' i s s1.
Proof.
  intros i s s1 sn.
  induction sn as  [ (* SN_Skip *) s
                   | (* SN_Assign *) x s a
                   | (* SN_Seq *) i1 i2 s s1 s' sn1 hrec_sn1 sn2 hrec_sn2
                   | (* SN_If_true *) cond i1 i2 s s' e sn hrec_sn
                   | (* SN_If_false *) cond i1 i2 s s' e sn hrec_sn
                   | (* SN_While_false *) cond i1 i2 s s' e sn hrecsn
                   | (* SN_While_true *)  cond i1 i2 s s' e sn hrecsn].
  - apply SN'_Skip.
  - apply SN'_Assign.
  - eapply SN'_Seq. apply hrec_sn1. apply hrec_sn2.
  - apply SN'_If_true. apply e. apply hrec_sn.
  - apply SN'_If_false. apply e. apply hrec_sn.
  - apply SN'_While_false. apply s. 
  - apply SN'_While_true. apply e. eapply SN'_Seq. apply hrecsn. apply IHsn1.
    Qed.

(** Pour la réciproque le script est semblable SAUF au dernier sous-but,
    qui précisément demande une inversion. *)
Lemma SN'_SN : forall i s s1, SN' i s s1 -> SN i s s1.
Proof.
  intros i s s1 sn'.
  induction sn' as [ (* SN_Skip *) s
                   | (* SN_Assign *) x s a
                   | (* SN_Seq *) i1 i2 s s1 s' sn1 hrec_sn1 sn2 hrec_sn2
                   | (* SN_If_true *) cond i1 i2 s s' e sn hrec_sn
                   | (* SN_If_false *) cond i1 i2 s s' e sn hrec_sn
                   | (* SN_While_false *) cond i s e
                   | (* SN_While_true *)
                     cond i s s' e sn hrec_sn
                   ].
  - apply SN_Skip.
  - apply SN_Assign.
  - eapply SN_Seq. apply hrec_sn1. apply hrec_sn2.
  - apply SN_If_true. apply e. apply hrec_sn.
  - apply SN_If_false. apply e. apply hrec_sn.
  - apply SN_While_false. apply e.
  - (** NIVEAU 4 *)
    (** Ici il faut exploiter l'hypothèse
        hrec_sn : SN (Seq i (While cond i)) s s'
        On observe que cette hypothèse est de la forme SN (Seq i1 i2) s s'
        qui est un cas particulier de SN i s s' ;
        cependant un destruct de hrec_sn oublierait que l'on est
        dans ce cas particulier *)
    destruct hrec_sn as [ | | | | | | ].
    + (** Le but obtenu ici correspond au cas où
          [Seq i (While cond i)] serait en même temps [Skip]
          un cas qui est hors propos. *)
      Undo 1.
    Undo 1.
    (** Cela est résolu en utilisant
        conséquence de hrec_sn indiquée par inv_Seq.
        Voir le mode d'emploi indiqué ci-dessus.
     *)
    destruct (inv_Seq hrec_sn) as [s1 sn1 sn2].
    (** On termine en utilisant ici SN_While_true *)
    + eapply SN_While_true.
      -- apply e.
      -- apply sn1.
      -- apply sn2.
Qed.

(* Repeat *)
Inductive rinstr :=
| RSkip   : rinstr
| RAssign : nat -> aexp -> rinstr
| RSeq    : rinstr -> rinstr -> rinstr
| RIf     : bexp -> rinstr -> rinstr -> rinstr
| Repeat  : rinstr -> bexp -> rinstr.

(** Définir la sémantique naturelle du langage REPEAT *)

Inductive SNr: rinstr -> state -> state -> Prop :=
| SNr_Skip        : forall s,
                    SNr RSkip s s
| SNr_Assign      : forall x e s,
                    SNr (RAssign x e) s (update s x (evalA e s))
| SNr_Seq         : forall i1 i2 s s1 s2,
                    SNr i1 s s1 -> SNr i2 s1 s2 -> SNr (RSeq i1 i2) s s2
| SNr_If_true     : forall b i1 i2 s s1,
                    evalB b s = true -> SNr i1 s s1 -> SNr (RIf b i1 i2) s s1
| SNr_If_false    : forall b i1 i2 s s2,
                    evalB b s = false -> SNr i2 s s2 -> SNr (RIf b i1 i2) s s2
| SNr_Repeat_true : forall b i s s1,
                    SNr i s s1 -> evalB b s1 = true -> SNr (Repeat i b) s s1 
| SNr_Repeat_false : forall b i s s1 s2,
                     SNr i s s1 -> evalB b s1 = false -> SNr (Repeat i b) s1 s2 -> SNr (Repeat i b) s s2     
.

Fixpoint repeat_while (i:rinstr) : winstr :=
    match i with
    | RSkip        => Skip
    | RAssign v a  => Assign v a
    | RSeq i1 i2   => Seq (repeat_while i1) (repeat_while i2)
    | RIf b r1 r2  => If b (repeat_while r1) (repeat_while r2)
    | Repeat r b => Seq (repeat_while r) (While (Bnot b) (repeat_while r))
                                            
    end.

(** Avant d'aborder la preuve suivante, il est recommandé de tester
    sur un petit programme REPEAT qu'après transformation son exécution
    à partir d'un état initial concret donne bien le même état final.
*)

(** Montrer que cette transformation préserve la sémantique c-a-d : *)

Theorem repeat_while_correct : forall i s1 s2, SNr i s1 s2 -> SN (repeat_while i) s1 s2.
Proof.
  intros i s1 s2 sn.
  induction sn.
  - apply SN_Skip.
  - apply SN_Assign.
  - eapply SN_Seq. 
    -- apply IHsn1.
    -- apply IHsn2.
  - eapply SN_If_true. apply H. apply IHsn.
  - eapply SN_If_false. apply H. apply IHsn.
  - cbn. eapply SN_Seq. apply IHsn. apply SN_While_false. cbn. rewrite H. cbn. reflexivity.
  - cbn. eapply SN_Seq. apply IHsn1. inversion IHsn2. eapply SN_While_true with (s1 := s3). cbn. rewrite H. cbn. reflexivity.
    -- apply H2.
    -- apply H5.    
  Qed.


    
(* -------------------------------------------------------------------------- *)
(** Transformation inverse *)
Fixpoint while_repeat (i:winstr) : rinstr :=
    match i with
    | Skip        => RSkip
    | Assign v a  => RAssign v a
    | Seq i1 i2   => RSeq (while_repeat i1) (while_repeat i2)
    | If b i1 i2  => RIf b (while_repeat i1) (while_repeat i2)
    | While b i   => RIf b (Repeat (while_repeat i) (Bnot b)) RSkip
    end.

(** Avant d'aborder la preuve suivante, il est recommandé de tester
    sur un petit programme WHILE qu'après transformation son exécution
    à partir d'un état initial concret donne bien le même état final.
*)


(** Montrer que cette transformation préserve la sémantique *)
(** La preuve suivante requiert quelques techniques supplémentaires,
    à considérer seulement après la semaine 7 *)

Theorem while_repeat_correct :
  forall i s1 s2, SN i s1 s2 -> SNr (while_repeat i) s1 s2.
Proof.
  intros i s1 s2 sn.
  induction sn.
  - apply SNr_Skip.
  - apply SNr_Assign.
  - eapply SNr_Seq.
    -- apply IHsn1.
    -- apply IHsn2.
  - apply SNr_If_true. apply H. apply IHsn.
  - apply SNr_If_false. apply H. apply IHsn.
  - apply SNr_If_false. apply H. apply SNr_Skip.
  - apply SNr_If_true.
    -- apply H.
    -- inversion IHsn2.
      --- eapply SNr_Repeat_false.
        ---- apply IHsn1.
        ---- cbn. rewrite H5. simpl. reflexivity.
        ---- apply H6.
      --- inversion H6. eapply SNr_Repeat_true.
      ---- rewrite H9 in IHsn1. apply IHsn1.
      ---- cbn. rewrite H9 in H5. rewrite H5. cbn. reflexivity.
Qed.

(* =============================== FIN PREUVES TD6 ==================================*)


(* =============================== DEBUT PREUVES TD7 ==================================*)


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


(* ======================= 3.1.3 DEBUT  ========================*)

(** Généralisation à Pcarre *)

(** On a besoin de deux lemmes arithmétiques, démontrables avec la tactique ring. *)
Lemma Sn_2 n : S n + S n = S (S (n + n)).
Proof. ring. Qed.

Lemma Sn_carre n : S n * S n = S (n + n + n * n).
Proof. ring. Qed.

Definition invar_cc n := [n; n*n; S (n+n)].

(* Question : Indiquer la signification de SOS_corps_carre. Démontrer ce théorème. *)
(* Réponse : Ce théorème établit la correction par récurrence du corps de la boucle. Il démontre que si l'on se trouve dans un état cohérent pour l'étape n (où i=n, x=n^2 et y=2n+1, représenté par invar_cc n), l'exécution des trois instructions du corps de la boucle nous amène dans l'état cohérent pour l'étape suivante n+1 (invar_cc (S n)).*)

  Theorem SOS_corps_carre n : SOS (Inter corps_carre (invar_cc n)) (Final (invar_cc (S n))).
Proof.
  unfold corps_carre.
  eapply SOS_again. { apply SOS1_Seqf. unfold incrI. apply SOS1_Assign. }
  simpl.
  eapply SOS_again. { apply SOS1_Seqf. unfold incrX. apply SOS1_Assign. }
  simpl.
  eapply SOS_again. { unfold incrY. apply SOS1_Assign. }
  simpl.
  unfold invar_cc.
  rewrite Sn_carre. 
  rewrite Sn_2.
  apply SOS_stop.
Qed.
  

(** Celui-ci est court mais difficile. Laisser Admitted au début. *)
Fixpoint SOS_seqf i1 i2 s1 s2 (so : SOS (Inter i1 s1) (Final s2)) :
  SOS (Inter (Seq i1 i2) s1) (Inter i2 s2).
Proof.
Admitted.

(* Question : Indiquer la signification de SOS_corps_carre_inter. Démontrer ce théorème *)
(*Réponse : Ce théorème valide le bilan d'un tour de boucle. Il prouve que l'exécution de la séquence corps ; suite consomme entièrement le corps pour faire passer la mémoire de l'état n à n+1, et nous dépose proprement devant la suite avec les variables mises à jour. *)
(** Réutiliser les lemmes précédents (facile et très court). *)
Lemma SOS_corps_carre_inter n i : 
  SOS (Inter (Seq corps_carre i) (invar_cc n)) (Inter i (invar_cc (S n))).
Proof.
  apply SOS_seqf.
  apply SOS_corps_carre.
Qed.

Lemma eqnatb_refl : forall n, eqnatb n n = true.
Proof.
intros n.
  induction n.
  -  simpl. reflexivity.
  - simpl. apply IHn.
Qed.

(* Question : Indiquer la signification de SOS_Pcarre_tour. Démontrer ce théorème. *)
(*Réponse : Ce théorème décrit un tour complet de boucle réussi, si la condition d'arrêt n'est pas encore atteinte (i!=n), alors le programme effectue un tour :
   - Il évalue la condition (qui est vraie).
   - Il exécute le corps de la boucle (mettant à jour la mémoire de l'étape i à i+1).
   - Il revient au point de départ (Pcarre), prêt à tester à nouveau la condition. *)
(** Réutiliser les lemmes précédents (facile). *)
Lemma SOS_Pcarre_tour :
  forall n i, eqnatb i n = false ->
  SOS (Inter (Pcarre n) (invar_cc i)) (Inter (Pcarre n) (invar_cc (S i))).
Proof.
  intros n i Hneq.
  eapply SOS_again. { apply SOS1_While. }
  eapply SOS_again.
  { apply SOS1_If_true. 
    simpl.
    rewrite Hneq.
    reflexivity. }
  apply SOS_corps_carre_inter.
Qed.

(* Question : Indiquer la signification de SOS_Pcarre_n_fini. Démontrer ce théorème. *)
(* Réponse : Ce théorème représente la condition d'arrêt de la boucle. Il affirme que si le programme est dans l'état où le compteur i est égal à la cible n (c'est le cas dans l'état invar_cc n), alors :
   - La condition de boucle not (i = n) est évaluée à Faux.
   - Le programme n'entre pas dans la boucle.
   - Il termine immédiatement (passe à l'état Final) sans changer la mémoire. *)
(** Facile *)
Theorem SOS_Pcarre_n_fini :
  forall n, SOS (Inter (Pcarre n) (invar_cc n)) (Final (invar_cc n)).
Proof.
  intros n.
  eapply SOS_again. { apply SOS1_While. }
  eapply SOS_again. 
  { 
    apply SOS1_If_false. 
    simpl. 
    rewrite eqnatb_refl. 
    reflexivity. 
  }
  eapply SOS_again. { apply SOS1_Skip. }
  apply SOS_stop.
Qed.

(* Question : Expliquer en français la démonstration de SOS_Pcarre_2_fin_V2. *)
Theorem SOS_Pcarre_2_fin_V2 : SOS (Inter Pcarre_2 [0;0;1]) (Final [2;4;5]).
Proof.
  (* Étape 1 : On utilise la transitivité pour effectuer le 1er tour de boucle. *)
  (* On part de i=0. L'objectif intermédiaire est d'arriver à i=1. *)
  eapply SOS_trans.
  { 
    (* On applique le lemme générique d'un tour de boucle. *)
    apply SOS_Pcarre_tour. 
    (* On doit prouver la condition d'entrée : "i != n", soit ici "0 != 2". *)
    (* reflexivity le fait automatiquement. *)
    reflexivity. 
  }
  (* Étape 2 : On utilise la transitivité pour le 2eme tour de boucle. *)
  (* On part de i=1. L'objectif intermédiaire est d'arriver à i=2. *)
  eapply SOS_trans.
  { 
    apply SOS_Pcarre_tour. 
    (* Preuve de la condition d'entrée : "1 != 2". *)
    reflexivity. 
  }
  (* Étape 3 : On gère la terminaison *)
  (* On est arrivé à i=2. *)
  eapply SOS_trans.
    (* Si i=n (ici 2=2), le programme s'arrête. *)
    { apply SOS_Pcarre_n_fini. }
 (*Terminaison*)
  apply SOS_stop.
Qed.

(* Question : Indiquer la signification de SOS_Pcarre_inf_tour. Démontrer ce théorème. *)
(* Réponse : Ce théorème décrit un tour de boucle infinie. Contrairement à la boucle finie précédente, ici il n'y a pas de condition d'arrêt à vérifier(While true). Le théorème affirme que quelque soit i, le programme :
    - exécute le corps (mettant à jour les variables de l'étape i à i+1).
    - revient exactement au point de départ (Pcarre_inf), prêt à recommencer indéfiniment.
C'est la preuve que la boucle tourne correctement *) 
Lemma SOS_Pcarre_inf_tour :
  forall i,
  SOS (Inter Pcarre_inf (invar_cc i)) (Inter Pcarre_inf (invar_cc (S i))).
Proof.
intros i.
  eapply SOS_again. { apply SOS1_While. }
  eapply SOS_again. 
  { apply SOS1_If_true. reflexivity. }
  apply SOS_corps_carre_inter.
Qed.

(* Question : Indiquer la signification de SOS_Pcarre_inf_i. Démontrer ce théorème.*)
(* Réponse : Le théorème affirme que quel que soit i, le programme peut atteindre l'état correspondant à la ième itération. Càd, en partant de l'état initial (i=0), il est possible d'exécuter la boucle suffisamment longtemps pour que les variables contiennent i, i^2 et 2i+1. *) 
Theorem SOS_Pcarre_inf_i :
  forall i,
  SOS (Inter Pcarre_inf [0; 0; 1]) (Inter Pcarre_inf (invar_cc i)).
Proof.
intros i.
  induction i.
  - simpl. 
    apply SOS_stop.
  - eapply SOS_trans.
     apply IHi. 
     apply SOS_Pcarre_inf_tour.
Qed.

(* ======================= 3.1.3 FIN  ========================*)

(* =============================== FIN PREUVES TD7 ==================================*)
