(*Exercice 1.1.1*)

type aexp =
  | Aco of int
  | Ava of int
  | Amu of aexp * aexp
  | Apl of aexp * aexp
  | Amo of aexp * aexp

type bexp =
  | Btrue of bexp
  | Bfalse of bexp
  | Bnot of bexp
  | Band of bexp*bexp
  | Bor of bexp*bexp
  | Beq of bexp*bexp
  | Beqnat of aexp*aexp

type instr =
  | Skip of instr
  | Assign of int * aexp
  | Seq of instr * instr
  | If of bexp * instr * instr
  | While of bexp * instr

type prog = instr
type state = int list 
