#use "anacomb.ml";;
(*Exercice 1.1.1*)

type aexp =
  | Aco of int
  | Ava of int

type bexp =
  | Btrue
  | Bfalse
  | Bnot of bexp

type instr =
  | Skip
  | Assign of aexp * aexp
  | Seq of instr * instr
  | If of bexp * instr * instr
  | While of bexp * instr

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
    |  _  -> None)

let p_val : (aexp, char) ranalist = 
  terminal_res (fun c -> match c with
    | '0' -> Some (Aco 0)
    | '1' -> Some (Aco 1)
    |  _  -> None)

let p_expr : (aexp, char) ranalist = 
  p_var +| p_val
  
let p_skip : (instr, char) ranalist =
  epsilon_res ()

let p_assign : (instr, char) ranalist =
  p_var ++> fun i ->
  terminal ':' -->
  terminal '=' -+>
  (p_expr ++> fun e ->
      epsilon_res (Assign(i, e)))

let rec p_prog_s :(instr, char) ranalist =
  terminal_res (fun c -> if c = ';' then Some () else None) ++> p_instr ++> p_prog_s
  +|  terminal_res (fun c -> if c = ';' then Some () else None) ++> epsilon_res 

and p_prog :(instr, char) ranalist =
  p_instr ++> (fun i -> p_prog_s ++> (fun s -> match s with
      | Skip -> epsilon_res i
      | _    -> epsilon_res (Seq (i, s))))

and p_instr :(instr, char) ranalist =
   p_if
  +| p_while
  +| p_assign
  +| epsilon_res

and p_while : (instr, char) ranalist =
  (terminal_res (fun c -> if c = 'w' then Some () else None)) ++>
  (fun _ -> (terminal_res (fun c -> if c = '(' then Some () else None)) ++>
  (fun _ -> p_expr ++>  (fun cond ->
  (terminal_res (fun c -> if c = ')' then Some () else None)) ++>
  (fun _ ->
  (terminal_res (fun c -> if c = '{' then Some () else None)) ++>
  (fun _ ->
  p_prog ++>
  (fun body ->
  (terminal_res (fun c -> if c = '}' then Some () else None)) ++>
  (fun _ ->
      epsilon_res (While (cond, body))
  )))))))

and p_if :(instr, char) ranalist =

  
let test_assign = p_assign ['a';":";"=";'3']

