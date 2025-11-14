(*Exercice 1.1.1*)

type aexp =
  | Aco of int
  | Ava of int


type bexp =
  | Btrue of bexp
  | Bfalse of bexp
  | Bnot of bexp

type instr =
  | Skip of instr
  | Assign of int * aexp
  | Seq of instr * instr
  | If of bexp * instr * instr
  | While of bexp * instr

type prog = instr
type state = int list


(*

<Programme> ::= <Instruction> <ProgrammeSuite>

<ProgrammeSuite> ::= ";" <Instruction> <ProgrammeSuite> | ε

<Instruction> ::= "s"
                | <Variable> ":=" <Valeur>
                | "i" "(" <Variable> ")" "{" <Programme> "}" "{" <Programme> "}"
                | "w" "(" <Variable> ")" "{" <Programme> "}"

<Variable> ::= "a" | "b" | "c" | "d"
<Valeur> ::= "0" | "1" | <Variable> | <Expression>

*)
