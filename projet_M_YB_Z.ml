#use "anacomb.ml";;
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




(*Exercice 1.2.1*)
(*
  [[expr]]s1 = false                                [[expr]]s1 = true                        
-----------------------                            -----------------------   
        else Q                                             then P
s1 ---------------> s3                             s1 ---------------> s2

*)

(*Exercice 2.1.1*)

let p_var : (aexp, char) ranalist = 
  terminal_res (fun c -> match c with
    | 'a' -> Some (Ava 0)
    | 'b' -> Some (Ava 1)
    | 'c' -> Some (Ava 2)
    | 'd' -> Some (Ava 3)
    | _   -> None)

let p_val : (aexp, char) ranalist = 
  terminal_res (fun c -> match c with
    | '0' -> Some (Aco 0)
    | '1' -> Some (Aco 1)
    | _   -> None)

let p_expr : (aexp, char) ranalist = 
  p_var -| p_val

let p_instr :(aexp, char) ranalist =
  terminal ( 's' ) -| ( )
let p_prog_s :(aexp, char) ranalist =
  terminal ( ';' ) --> ( )

let p_prog :(aexp, char) ranalist =