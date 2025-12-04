  #use "anacomb.ml";;
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

  let rec p_final : (aexp, char) ranalist =
    fun l -> 
      ( (terminal '!' -+> p_final ++> fun f -> epsilon_res (Bnot f))
      +| (p_expr ++> fun e -> epsilon_res (e))
      +| (terminal '(' -+> p_disj ++> fun d -> terminal ')' -+> epsilon_res d)
      ) l
  and p_disj : (aexp, char) ranalist =
    fun l ->
      (p_conj ++> fun c1 ->
      p_disj_s c1
      ) l
  and p_disj_s (acc : aexp) : (aexp, char) ranalist =
    fun l ->
      ( (terminal '+' -+> p_conj ++> fun c2 ->
          let c = Bdisj (acc, c2) in
          p_disj_s c)
        +|
        epsilon_res (acc)
      ) l
  and p_conj : (aexp, char) ranalist =
    fun l ->
      ( p_final ++> fun f1 ->
        p_conj_s f1 
      ) l
  and p_conj_s (acc : aexp) : (aexp, char) ranalist =
    fun l -> 
      ( ( terminal '.' -+> p_final ++> fun f2 ->
      let acc' = Bconj (acc, f2) in
      p_conj_s acc')
      +| epsilon_res (acc)
      ) l
  and p_expr : (aexp, char) ranalist = 
    fun l -> 
    ( p_var +| p_val
    ) l
  and p_assign : (instr, char) ranalist =
    fun l ->
    ( p_var ++> fun i ->
    terminal ':' -->
    terminal '=' -+>
    (p_expr ++> fun e -> epsilon_res (Assign(i, e)))
    ) l
  let p_skip : (instr,char) ranalist =
    epsilon_res (Skip)

  let rec p_prog : (instr, char) ranalist =
    fun l ->
      (p_instr ++> fun i1 ->
      p_prog_s i1) l
  and p_prog_s (acc : instr) : (instr, char) ranalist =
    fun l ->
      ( (terminal ';' -+> p_instr ++> fun i2 ->
          let acc' = Seq (acc, i2) in
          p_prog_s acc')
        +|
        epsilon_res acc
      ) l
  and p_instr : (instr, char) ranalist =
    fun l ->
      (     p_if
        +|  p_while
        +|  p_assign
        +|  p_skip
      ) l
  and p_if :(instr, char) ranalist =
    fun l ->
      (terminal ('i') --> terminal ('(') -+> p_disj ++> fun cond -> terminal (')') -->
      terminal ('{') -+> p_prog ++> fun i1 -> terminal ('}') -->
      terminal ('{') -+> p_prog ++> fun i2 -> terminal ('}') -+>
      epsilon_res (If (cond, i1, i2))) l
  and p_while : (instr, char) ranalist =
    fun l ->
      ((terminal_res (fun c -> if c = 'w' then Some () else None)) ++>
      (fun _ -> (terminal_res (fun c -> if c = '(' then Some () else None)) ++>
      (fun _ -> p_disj ++>  (fun cond -> (terminal_res (fun c -> if c = ')' then Some () else None)) ++>
      (fun _ -> (terminal_res (fun c -> if c = '{' then Some () else None)) ++>
      (fun _ -> p_prog ++>
      (fun body -> (terminal_res (fun c -> if c = '}' then Some () else None)) ++>
      (fun _ -> epsilon_res (While (cond, body)))))))))
    ) l
  

  (*Exercice 2.1.2*)

  let rec string_of_aexp = function
    | Aco n -> "Aco " ^ string_of_int n
    | Ava n -> "Ava " ^ string_of_int n
    | Bnot e ->
      "Bnot ( " ^ string_of_aexp e ^ ")"
    | Bconj (e1, e2) ->
      "Bconj ( " ^ string_of_aexp e1 ^ ", " ^ string_of_aexp e2 ^ ")"
    | Bdisj (e1, e2) ->
      "Bdisj ( " ^ string_of_aexp e1 ^ ", " ^ string_of_aexp e2 ^ ")"

  let rec string_of_instr = function
    | Skip ->
        "Skip"
    | Assign (v, e) ->
        "Assign (" ^ string_of_aexp v ^ ", " ^ string_of_aexp e ^ ")"
    | Seq (i1, i2) ->
        "Seq (" ^ string_of_instr i1 ^ ", " ^ string_of_instr i2 ^ ")"
    | If (cond, i1, i2) ->
        "If (" ^ string_of_aexp cond ^ ", "
              ^ string_of_instr i1 ^ ", "
              ^ string_of_instr i2 ^ ")"
    | While (cond, body) ->
        "While (" ^ string_of_aexp cond ^ ", "
                  ^ string_of_instr body ^ ")"

  let show_ast s =
    let (ast, _) = p_prog (list_of_string s) in
    print_endline (string_of_instr ast)
(*TESTS : Grammaire basique*)
  let _ =show_ast "a:=0";;
  let _=  show_ast "a:=0;b:=1";;
  let _=  show_ast "w(a){a:=1}";;
  let _=  show_ast "i(a){a:=0}{b:=1}";;
  let _=  show_ast ";";;
  let _= show_ast "a:=1;b:=1;c:=1;w(a){i(c){c:=0;a:=b}{b:=0;c:=a}}";;
  let _= show_ast "i(a+b){a:=1}{b:=0}";;
  ()
  (*TESTS : Expression avec disjonctions et conjonctions*)

  let test_aexp s =
  try
    let (e, rest) = p_disj (list_of_string s) in
    Printf.printf "=== Test expr : %S ===\n" s;
    Printf.printf "AST  : %s\n" (string_of_aexp e);
    Printf.printf "Reste: %S\n\n"
      (String.of_seq (List.to_seq rest));
    e
  with Echec ->
    Printf.printf "Échec de l'analyse d'expression sur : %S\n\n" s;
    raise Echec
   
  let _ = string_of_aexp (Bdisj (
   Bconj (Ava 0, Ava 1),
   Aco 1
))

  let _ =
  (* Expressions seules *)
  ignore (test_aexp "a");
  ignore (test_aexp "0");
  ignore (test_aexp "a+b");
  ignore (test_aexp "a.b");
  ignore (test_aexp "a.b+1");
  ignore (test_aexp "!a");
  ignore (test_aexp "!a.b+0");
  ignore (test_aexp "(a+b).c");
  ignore (test_aexp "!(a+b).c");
  ignore (test_aexp "!(a.b+!(c.d))");
  ignore (test_aexp "!!a");
  ignore (test_aexp "a+b.c+d.e");
  ignore (test_aexp "((a))");
  ignore (test_aexp "!!!!!!!!!!!!!!!!!!!!!!a");
  ignore (test_aexp "(a))");
  ignore (test_aexp "(a");
  ()
    
  (*Exercice 2.2.1*)

