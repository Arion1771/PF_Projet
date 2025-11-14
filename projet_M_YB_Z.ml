(*Exercice 1.1.1*)

type Aexp =
  | Aco of int
  | Ava of int
  | Amu of Aexp * Aexp
  | Apl of Aexp * Aexp
  | Amo of Aexp * Aexp

type Bexp =
  | Btrue of Bexp
  | Bfalse of Bexp
  | Bnot of Bexp -> Bexp
  | Band of Bexp -> Bexp -> Bexp
  | Bor of Bexp -> Bexp -> Bexp
  | Beq of Bexp -> Bexp -> Bexp
  | Beqnat of Aexp -> Aexp -> Bexp

