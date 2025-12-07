(**********************  TD n°6  ***************************)
(* Ce TD porte sur la sémantique naturelle codée en Coq    *)
(* du petit langage impératif WHILE déjà vu précédemment.  *)
(* On va l'utiliser pour faire des dérivations, montrer    *)
(* des propriétés, étudier des extensions.                 *)
(***********************************************************)

(* On importe les bibliothèques de Coq utiles pour le TD   *)

Require Import Bool Arith List.
Import List.ListNotations.

(** * On choisit de définir ici un état comme une liste d'entiers naturels.
      On utilise ici le type list de la bibliothèque standard de Coq.
      Ce type est polymorphe. On le spécialise pour des éléments de type nat. *)

Check list.
Check list nat.

Print list.

Check 0::1::3::nil.

(* Ici on observe que la notation des listes façon ocaml est gérée par Coq. *)

Check [0;1;3].

Remark List_Notation: [0;1;3] = 0::1::3::nil.
Proof.
  reflexivity.
  Qed.

(** * On reprend ici les AST définis aux séances précédentes *)

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

(** Exemples *)

Compute (get 0 S3).
Compute (get 1 S3).
Compute (get 2 S3).
Compute (get 3 S3).
Compute (get 4 S3).

(** La mise à jour d'une variable v par un nouvel entier n dans un état s
    s'écrit 'update s v n'
    Cette fonction n'échoue jamais et écrit la valeur à sa place même
    si elle n'est pas encore définie dans l'état *)

Fixpoint update (s:state) (v:nat) (n:nat): state :=
  match v,s with
  | 0   , a :: l1 => n :: l1
  | 0   , nil     => n :: nil
  | S v1, a :: l1 => a :: (update l1 v1 n)
  | S v1, nil     => 0 :: (update nil v1 n)
  end.

Definition S4 := update (update (update (update (update S1 4 1) 3 2) 2 3) 1 4) 0 5.

Compute S1.
Compute S4.

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


(** Quelques expressions arithmétiques pour tester *)

(** exp1 = x + 3 *)
Definition E1 := Apl X N3.

(** exp2 = y - 1 *)
Definition E2 := Amo Y N1.

(** exp3 = (x + y) * 2 *)
Definition E3 := Amu (Apl X Y) N2.

Compute (evalA E1 S1).
Compute (evalA E1 S2).
Compute (evalA E2 S1).
Compute (evalA E2 S2).
Compute (evalA E3 S1).
Compute (evalA E3 S2).

(** Quelques expressions booléennes pour tester *)

(** B1 :=  exp1 = 4 *)
Definition B1 := Beqnat E1 N4.

(** B2 := not ( bexp1 /\ (exp1 = 7) *)
Definition B2 := Bnot (Band B1 (Beqnat X N2)).

Compute (evalB B1 S1).
Compute (evalB B1 S2).
Compute (evalB B2 S1).
Compute (evalB B2 S2).

(** Corrigé du travail effectué en TD5 *)

Fail Fixpoint evalW (i : winstr) (s : state) {struct i} : state :=
  match i with
  | Skip       => s
  | Assign x e => update s x (evalA e s)
  | Seq i1 i2  => let s1 := evalW i1 s in evalW i2 s1
               (*   (evalW i2 (evalW i1 s) *)
  | If e i1 i2 => match evalB e s with
                  | true  => evalW i1 s
                  | false => evalW i2 s
                  end
  | (While e i1) as i => match evalB e s with
                  | true  =>
                    let s1 := evalW i1 s in evalW (While e i1) s1
               (*   let s1 := evalW i1 s in evalW i s1                *)
                  | false => s
                  end
  end.



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

(** On code dans WHILE un programme P1 correspondant à
    while not (i=0) do {i:=i-1;x:=1+x} *)
Definition Il := 0.
Definition Ir := Ava Il.
Definition Xl := 1.
Definition Xr := Ava Xl.

Definition corps_boucle := Seq (Assign Il (Amo Ir N1)) (Assign Xl (Apl N1 Xr)).
Definition P1 := While (Bnot (Beqnat Ir N0)) corps_boucle.

(** On montre que P1 transforme l'état S1 en l'état S2  *)

Theorem reduction1 : SN P1 S1 S2.
(** Regarder les états courants tout au long de la preuve *)
Proof.
  cbv [P1]. cbv [S1]. cbv [S2].
  (** Ou de façon équivalente :
  unfold P1. unfold S1. unfold S2. *)

  (** Ce but devrait être prouvé par l'une des deux dernières règles de SN,
      qui portent sur le cas While.
      On peut deviner laquelle de tête, ou demander de l'aide ainsi : *)
  Compute (evalB (Bnot (Beqnat Ir N0)) [1; 2]).
  (** Ce sera donc avec SN_While_true.
      On peut essayer d'avancer avec 'apply SN_While_true.'  ... mais ça échoue.
      Ici Coq ne peut pas deviner ce que sera l'état intermédiaire s1. *)
  Fail apply SN_While_true.
  (** Une stratégie possible serait d'indiquer directement l'état
      intermédiaire avec la variante 'apply ... with (s1:= ...)'.
      Il faut deviner les paramètres corrects ce qui n'est pas toujours facile.
      Dans notre cas cela serait : *)
  apply SN_While_true with (s1:=[0;3]).
  (** On va donc proposer une autre stratégie· *)
  Undo 1.
  (** Une première possibilité est avec refine, déjà connu :
      ici on indique un joker '_' pour chacun des HUIT arguments ;
      [b], [i], [s] et [s2] se trouvent déterminés par la forme du but,
      [s1] sera déterminé par la preuve de [SN s i s s1] et ne donne donc
      pas lieu à un sous-but. Il restera à prouver :
      [evalB b s = true], [SN i s s1] et [SN (While b i) s1 s2].  *)
  refine (SN_While_true _ _ _ _ _ _ _ _).
  (** On obtient le même effet avec la tactique [eapply], plus commode. *)
  Undo 1.
  eapply SN_While_true.
  - reflexivity.
  - cbv [corps_boucle].
    (** Un nouvel état intermédiaire est à deviner *)
    eapply SN_Seq.
    + apply SN_Assign.
    (* En appliquant cette règle nous avons fixé la valeur de l'état d'arrivée *)
    + (* L'état de départ vient du cas précédent ;
         comme les états sont connus on peut simplifier. *)
      cbn [evalA Ir Il N1 get minus update].
      (** Ou, plus rapidement *)
      Undo 1.
      cbn.
      apply SN_Assign.
  - cbn.
    (** SN_While_true ou SN_While_false ? *)
    Compute (evalB (Bnot (Beqnat Ir N0)) [0; 3]).
    apply SN_While_false.
    cbn.
    reflexivity.
Qed.

(** À FAIRE (NIVEAU 1) :
    terminer la présentation de  reduction1 sous forme d'arbre ci-dessous *)
Definition AFAIRE_dessin_reduction1 : unit.

(*

evalB "i<>0" [1; 2] = true          SN corps [1; 2] [0; 3]                                  SN P1 [0; 3] [0; 3]
------------------------------------------------------------------------------------------------------------------ SN_While_true
                                     SN P1 [1; 2] [0; 3]

 *)
Admitted.

(** Une autre présentation de ce script, structurée par accolades.
    Cela permet de gérer l'indentation autrement
    (surtout utile quand le corps de boucle s'exécute plusieurs fois. *)
Theorem reduction1_accolades : SN P1 S1 S2.
Proof.
  cbv [P1]. cbv [S1]. cbv [S2].
  eapply SN_While_true.
  Compute Ir.
  { cbn. reflexivity. }
  { cbv [corps_boucle].
    eapply SN_Seq.
    + apply SN_Assign.
    + cbn. apply SN_Assign. }
  cbn.
  Compute (evalB (Bnot (Beqnat Ir N0)) [0; 3]).
  apply SN_While_false.
  cbn. reflexivity.
Qed.

(** Exercice d'entraînement *)

Theorem entrainement_P1 : SN P1 [2; 5] [0; 7].
Proof.
  cbv [P1].
  eapply SN_While_true.
  - cbn. reflexivity.
  - cbv [corps_boucle]. eapply SN_Seq. apply SN_Assign. cbn. apply SN_Assign.
  - cbn. Compute (evalB (Bnot (Beqnat Ir N0)) [1;6]).
    eapply SN_While_true.
    cbn. reflexivity.
    cbv [corps_boucle]. eapply SN_Seq. apply SN_Assign. cbn. apply SN_Assign.
    cbn.
    Compute (evalB (Bnot (Beqnat Ir N0)) [0;7]).
    eapply SN_While_false. cbn. reflexivity.
    Qed.
(** On veut montrer maintenant que P1 rend toujours un état où
    i vaut 0 et x voit sa valeur augmenter de la valeur initiale de i. *)

(** Rappel : En Coq les entiers naturels sont définis par un type inductif
    comprenant les constructeurs O pour zéro et S pour le successeur. *)

Print nat.
Require Import Arith. (* pour Nat.sub_0_r *)
Check Nat.sub_0_r.

(** Les lemmes suivants de la bibliothèque Coq peuvent être utiles
    - Lemma Nat.sub_0_r : forall n, n - 0 = n.
    - Lemma minus_n_O :   forall n, n = n - 0. (* obsolète depuis coq.8.16.0 *)
    - Lemma plus_n_Sm :   forall n m, S (n + m) = n + S m. *)

Theorem reduction2 : forall x y, SN P1 [x;y] [0;x+y].
Proof.
  cbv[P1 Ir Il N1 Xr Xl]; intros x.
  induction x as [ | x Hrec_x];
    intros; cbn [evalA];cbn [evalB].
  -eapply SN_While_false. cbn. reflexivity.
  -eapply SN_While_true.  cbn. reflexivity.
   --cbv [corps_boucle]. eapply SN_Seq. apply SN_Assign. cbn. apply SN_Assign.
   --cbn. rewrite Nat.sub_0_r. rewrite plus_n_Sm. apply Hrec_x.
Qed.

       (* 1 goal (ID 150) *)
  
  (* x : nat *)
  (* Hrec_x : *)
  (*   forall y : nat, *)
  (*   SN (While (Bnot (Beqnat (Ava 0) N0)) corps_boucle) [x; y] [0; x + y] *)
  (* y : nat *)
  (* ============================ *)
  (* SN (While (Bnot (Beqnat (Ava 0) N0)) corps_boucle) *)
  (*   (update [x - 0; y] Xl (evalA (Apl N1 Xr) [x - 0; y])) [ *)
(*   0; S x + y] *)

(** *** Calcul du carré avec des additions *)
(** On code dans While un programme Pcarre correspondant à
    while not (i=n) do {i:= 1+i; x:= y+x ; y:= 2+y} *)
(* (* *déjà définis *)
Definition Il := 0.
Definition Ir := Ava Il.
Definition Xl := 1.
Definition Xr := Ava Xl.*)
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

       

  (* complétez ici NIVEAU 1 *)

(** Énoncer et démontrer que Pcarre n permet de calculer le carré de n *)
(* Complétez ici NIVEAU 4
   (pas de technique nouvelle, mais demande de la créativité *)

(** Sur le même modèle, trouver un programme Pcube n'utilisant que des
    additions, énoncer et démontrer qu'il est correct. *)
(* Complétez ici NIVEAU 6
   (pas de technique nouvelle, mais demande de la créativité
  Quelques lemmes utiles.
 *)

Check Nat.add_comm.
Check Nat.add_assoc.
(*Check plus_assoc.*)

(* -------------------------------------------------------------------------- *)
(** ** Preuve par récurrence structurelle dans un prédicat inductif *)


(** Transformation simple de programme :
   -  if true  then X else Y ---> X
   -  if false then X else Y ---> Y  *)
Fixpoint simpl_test_Btrue_Bfalse (i: winstr) : winstr :=
  match i with
  | Skip => Skip
  | Assign v a => i
  | Seq w1 w2 => Seq (simpl_test_Btrue_Bfalse w1)
                     (simpl_test_Btrue_Bfalse w2)
  | If Btrue i1 i2 => simpl_test_Btrue_Bfalse i1
  | If Bfalse i1 i2 => simpl_test_Btrue_Bfalse i2
  | If b i1 i2 => If b (simpl_test_Btrue_Bfalse i1) (simpl_test_Btrue_Bfalse i2)
  | While b i1 => While b (simpl_test_Btrue_Bfalse i1)
    
end.                    

    (* complétez ici NIVEAU 1 *)


(** Comme indiqué ci-dessus on va procéder par récurrence structurelle sur
     les arbres de preuve de [SN i s s'] *)
Theorem simpl_test_Btrue_Bfalse_correct :
  forall i s s', SN i s s' -> SN (simpl_test_Btrue_Bfalse i) s s'.
Proof.
  (** On essaie d'abord une récurrence sur i.
      Même avec prudence, de la façon la plus générale possible
      (les états [s] et [s'], ainsi que l'hypothèse [SN i s s'] sont
      introduits APRÈS [induction i]), on verra que les buts ne sont
      pas comme souhaité *)
  intro i.
  induction i as [ | | | | ]; (** sans nommer les composantes pour alléger *)
    intros s s' sn (** les introductions sont effectuées systématiquement
                         sur chaque sous-but *).
  (** On observe que l'hypothèse [sn] devrait entraîner [s = s'],
      mais on ne l'a pas obtenu directement ;
      tous les autres sous-buts souffrent de problèmes analogues. *)
  Undo 2.
  (** Il est bien plus opportun de raisonner par récurrence sur [sn] car
      on aura naturellement non seulement la décomposition de [i] mais en plus
      les contraintes dictées par la définition de SN *)
  intros i s s' sn.
  induction sn as  [ (* SN_Skip *) s
                   | (* SN_Assign *) x s a
                   | (* SN_Seq *) i1 i2 s s1 s' sn1 hrec_sn1 sn2 hrec_sn2
                   | (* SN_If_true *) cond i1 i2 s s' e sn hrec_sn
                   | (* SN_If_false *) cond i1 i2 s s' e sn hrec_sn
                   | (* SN_While_false *) b i s H  (* complétez ici NIVEAU 1 *)
                   | (* SN_While_true *) b i s s' s'' H (* complétez ici NIVEAU 1 *)
                   ]; cbn [simpl_test_Btrue_Bfalse].

 (** La preuve qui suit est un peu fastidieuse, on verra plus tard
     des moyens de la fabriquer plus intelligemment. *)
 (** Certains buts contiendront en une hypothèse se convertissant en
     [false = true] (plus visible en utilisant [cbn in ...].
     On pourra se souvenir d'une technique vue auparavant (égalités contagieuses).
  *)
(** complétez ici
     - nombreux sous-buts NIVEAU 2
     - quelques sous-buts NIVEAU 3, utiliser admit si besoin *)
  - apply SN_Skip.
  - apply SN_Assign.   
  - eapply SN_Seq.
    -- apply hrec_sn1.
    -- apply hrec_sn2.
  - destruct cond.
    -- apply hrec_sn.
    -- discriminate e.
    -- eapply SN_If_true. apply e. apply hrec_sn.
    -- eapply SN_If_true. apply e. apply hrec_sn.
    -- eapply SN_If_true. apply e. apply hrec_sn.
    -- eapply SN_If_true. apply e. apply hrec_sn.   
    -- eapply SN_If_true. apply e. apply hrec_sn.
  - destruct cond.
    -- discriminate e.
    -- apply hrec_sn.
    -- eapply SN_If_false. apply e. apply hrec_sn.
    -- eapply SN_If_false. apply e. apply hrec_sn.
    -- eapply SN_If_false. apply e. apply hrec_sn.   
    -- eapply SN_If_false. apply e. apply hrec_sn.
    -- eapply SN_If_false. apply e. apply hrec_sn.
  - eapply SN_While_false. apply H.
  - eapply SN_While_true. apply H.
    -- apply IHsn1.
    -- eapply IHsn2.  
Qed.

      (* -------------------------------------------------------------------------- *)
(** Une autre transformation simple (facultatif, pour l'entraînement)
  -  if (Bnot b) then X else Y ---> if b then Y else X  *)

Fixpoint simpl_test_echange (i: winstr) : winstr.
  (** complétez ici NIVEAU 1 *)
Admitted.

Lemma negb_negb : forall b, b = negb (negb b).
Proof.
  (** complétez ici NIVEAU 1 *)
Admitted.

Lemma Bnot_negb : forall {B s b}, evalB (Bnot B) s = b -> evalB B s = negb b.
Proof.
 (** complétez ici NIVEAU 1 *)
Admitted.

(** Suivre le même principe que pour simpl_test_Btrue_Bfalse_correct. *)
Theorem simpl_test_echange_correct :
  forall i s s', SN i s s' -> SN (simpl_test_echange i) s s'.
Proof.
  (** complétez ici
     - nombreux sous-buts NIVEAU 2
     - quelques sous-buts NIVEAU 3, utiliser admit si besoin *)
Admitted.

(* -------------------------------------------------------------------------- *)
(** ** Interlude sur l'inversion *)

(** Dans l'exercice qui suit, on aura un but comprenant
    une hypothèse de la forme [SN i s s2],
    où [i] est lui-même de la forme [Seq i1 i2]    (1).
    Sans la condition (1), il serait naturel de procéder par
    cas sur [i], ce qui donnerait lieu aux 7 cas ;
    mais avec la condition (1), on voit que seul un cas
    est pertinent, correpondant à SN_Seq.
    Cette technique de preuve est dite "par inversion".
    On va se ramener à une situation plus simple au moyen du
    prédicat auxiliaire suivant, qui isole le cas intéressant de SN.
 *)

Inductive SN1_Seq i1 i2 s s2 : Prop :=
| SN1_Seq_intro : forall s1,
                  SN i1 s s1 -> SN i2 s1 s2 -> SN1_Seq i1 i2 s s2
.

(** On peut alors démontrer la conséquence suivante d'une hypothèse
    respectant la condition (1) ci-dessus *)

Lemma inv_Seq' : forall {i1 i2 s s2}, SN (Seq i1 i2) s s2 -> SN1_Seq i1 i2 s s2.
Proof.
  intros i1 i2 s s2 sn.
  (** Ici in utilise une tactique magique de Coq. *)
  inversion sn.
  (** Puis une autre, pour nettoyer les égalités. *)
  subst.
  apply (SN1_Seq_intro _ _ _ _ _ H1 H4).
Qed.

(** Mode d'emploi. Devant ce but :

 H18 : SN (Seq i1 i2) s s2
 =========================
 conclusion

Au lieu d'un
  destruct H18
qui donne 7 cas, on observe que
[inv_Seq' H18]   est de type   [SN1_Seq i1 i2 s s2]
et donc on peut de manière plus adéquate utiliser
  destruct (inv_Seq' H18) as [s1 sn1 sn2]
qui ne prévoit qu'un cas, celui qui est pertinent.
*)

(** Voici une preuve par "petites inversions" du même théorème,
    qui n'utilise que les connaissances élémentaires déjà acquises,
    en particulier un programme à la "ouf_ouf" (voir coq3_B_A_BA_ouf.v
    dans les supports de CM).
    Il n'est pas indispensable de la comprendre avant de l'utiliser.
    EXERCICES FACULTATIFS :
    1) expliquer le fonctionnement cette preuve.
    2) dans les scripts à suivre, utiliser SN_inv
       au lieu de inv_Seq
       (intérêt : cela pourra être généralisé).
 *)
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

(** *** Illustration *)
(** Une autre manière d'exprimer la sémantique de WHILE ;
    on prouvera que SN et SN' sont équivalentes. *)
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

(* -------------------------------------------------------------------------- *)
(** ** Le langage REPEAT *)
(** On considère maintenant un langage impératif sans la commande While,
    mais comportant une autre instruction de boucle
                       'repeat i until b
    qui exécute i puis sort si b est vrai, et sinon recommence.
 *)

(** Voici la syntaxe du langage REPEAT
    (on redéfinit un nouveau type avec de nouveaux constructeurs.    *)

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

(** On code dans REPEAT un programme P2 correspondant à
    repeat {i:=i-1;x:=1+x} until i=0 *)

Definition corps_boucleR : rinstr. Admitted.
Definition P2 := Repeat corps_boucleR (Beqnat Ir N0).

Lemma P2_test : SNr P2 [2; 5] [0; 7].
Proof.
Admitted.

(** À FAIRE : présenter P2_test sous forme d'arbre *)
Definition AFAIRE_dessin_P2_test : unit.
Admitted.
(*
                                     (Arrêt de la récursion)
                                     -----------------------
                                     evalB cond s2 = true   (0 == 0)
                                              ^
                                              |
                     (Exécution du corps)     |
                     --------------------     |
                     SNr corps s1 s2          |
                            ^                 |
                            |                 |
(Règle utilisée)            |                 |
SNr_Repeat_true  --------------------------------------------
(Conclusion 2)           SNr (Repeat ...) s1 s2
                                    ^
                                    |
                                    |
                                    |
           (Condition échouée)      |
           -------------------      |
           evalB cond s1 = false    |
           (1 == 0) -> Faux         |
                    ^               |
                    |               |
(Exécution du corps)|               |
--------------------                |
SNr corps s0 s1                     |
        ^                           |
        |                           |
        |                           |
SNr_Repeat_false ----------------------------------------------------
  (Conclusion Globale)     SNr (Repeat ...) s0 s2

*)


(** *** Preuves sur SNr *)
(** On va maintenant montrer que : 'Repeat i until b'
    peut être traduit  par       : 'i; while (not b) do i'     *)

(** Ecrire une fonction qui traduit toute expression rinstr en winstr en
    remplaçant les Repeat par l'expression équivalente ci-dessus
 *)

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

       
(* -------------------------------------------------------------------------- *)
(** ** Le langage WHILE-REPEAT *)
(** Remarque : on pourrait également considérer un langage WHILE_REPEAT
    comprenant à la fois l'instruction While et l'instruction Repeat
 *)

