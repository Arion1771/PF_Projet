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


(*Exercice 1.1.2 and 1.1.3

<Programme> ::= <Instruction> <ProgrammeSuite>

<ProgrammeSuite> ::= ";" <Instruction> <ProgrammeSuite> | ε

<Instruction> ::= "s"
                | <Variable> ":=" <Valeur>
                | "i" "(" <Expression> ")" "{" <Programme> "}" "{" <Programme> "}"
                | "w" "(" <Expression> ")" "{" <Programme> "}"

<Variable> ::= "a" | "b" | "c" | "d"
<Valeur> ::= "0" | "1" | 
<Expression> ::= <Variable> | <Valeur> 

*)

(*Exercice 1.1.4*)

(*Exercice 1.2.1

  [[expr]]s1 = false                                [[expr]]s1 = true                        
-----------------------                            -----------------------   
        else Q                                             then P
s1 ---------------> s3                             s1 ---------------> s2

*)

(*Exercice 1.2.2*)

