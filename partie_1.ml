  (*Exercice 1.1.1*)

  type aexp =
    | Aco of int
    | Ava of int
    | Bnot of aexp
    | Bconj of aexp * aexp
    | Bdisj of aexp * aexp

  type instr =
    | Skip 
    | Assign of aexp * aexp
    | Seq of instr * instr
    | If of aexp * instr * instr
    | While of aexp * instr

  type prog = instr
  type state = int list


  (*Exercice 1.1.2 and 1.1.3

  <Programme> ::= <Instruction> <ProgrammeSuite>

  <ProgrammeSuite> ::= ";" <Instruction> <ProgrammeSuite> | ε

  <Instruction> ::= "s"
                  | <Variable> ":=" <Expression>
                  | "i" "(" <Expression> ")" "{" <Programme> "}" "{" <Programme> "}"
                  | "w" "(" <Expression> ")" "{" <Programme> "}"

  <Variable> ::= "a" | "b" | "c" | "d"
  <Valeur> ::= "0" | "1" | 
  <Expression> ::= <Variable> | <Valeur> 

  *)

  (*Exercice 1.1.4*)
  (* 
  <Valeur> ::= "0" | "1" | 
  <Variable> ::= "a" | "b" | "c" | "d"
  <Expression> ::= <Variable> | <Valeur> 
  <Disjonction> ::= <Conjonction> <DisjonctionSuite>
  <DisjonctionSuite> ::= "+" <Conjonction> <DisjonctionSuite> | ε
  <Conjonction> ::= <Final> <ConjonctionSuite>
  <ConjonctionSuite> ::= "." <Final> <ConjonctionSuite> | ε
  <Final> ::= "!" <Final> | <Expression> | "(" <Disjonction> ")"

  *)
  (*Exercice 1.2.1*)
  (*
    [[expr]]s1 = false                                [[expr]]s1 = true                        
  -----------------------                            -----------------------   
          else Q                                             then P
  s1 ---------------> s3                             s1 ---------------> s2

  *)
