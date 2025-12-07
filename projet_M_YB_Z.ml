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

(*Exercice 2.1.1 adapté au 2.1.4 optionel*)
let rec p_blanc : char list -> char list = fun l ->
  match l with
  | ' ' :: rest | '\t' :: rest | '\n' :: rest -> p_blanc rest
  | _ -> l

let lexeme p = 
  p_blanc -+> p ++> fun x ->
  p_blanc -+> epsilon_res x

let terminal_res_blanc f = lexeme (terminal_res f)

let terminal_blanc c = 
  p_blanc --> terminal c --> p_blanc

  let p_var : (aexp, char) ranalist = 
    terminal_res_blanc (fun c -> match c with
      | 'a' -> Some (Ava 0)
      | 'b' -> Some (Ava 1)
      | 'c' -> Some (Ava 2)
      | 'd' -> Some (Ava 3)
      |  _  -> None)

  let p_val : (aexp, char) ranalist = 
    terminal_res_blanc (fun c -> match c with
      | '0' -> Some (Aco 0)
      | '1' -> Some (Aco 1)
      |  _  -> None)

  let rec p_final : (aexp, char) ranalist =
    fun l -> 
      ( (terminal_blanc '!' -+> p_final ++> fun f -> epsilon_res (Bnot f))
      +| (p_expr ++> fun e -> epsilon_res (e))
      +| (terminal_blanc '(' -+> p_disj ++> fun d -> terminal_blanc ')' -+> epsilon_res d)
      ) l
  and p_disj : (aexp, char) ranalist =
    fun l ->
      (p_conj ++> fun c1 ->
      p_disj_s c1
      ) l
  and p_disj_s (acc : aexp) : (aexp, char) ranalist =
    fun l ->
      ( (terminal_blanc '+' -+> p_conj ++> fun c2 ->
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
      ( ( terminal_blanc '.' -+> p_final ++> fun f2 ->
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
    terminal_blanc ':' -->
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
      ( (terminal_blanc ';' -+> p_instr ++> fun i2 ->
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
      (terminal_blanc ('i') --> terminal_blanc ('(') -+> p_disj ++> fun cond -> terminal_blanc (')') -->
      terminal_blanc ('{') -+> p_prog ++> fun i1 -> terminal_blanc ('}') -->
      terminal_blanc ('{') -+> p_prog ++> fun i2 -> terminal_blanc ('}') -+>
      epsilon_res (If (cond, i1, i2))) l
  and p_while : (instr, char) ranalist =
    fun l ->
      ((terminal_res_blanc (fun c -> if c = 'w' then Some () else None)) ++>
      (fun _ -> (terminal_res_blanc (fun c -> if c = '(' then Some () else None)) ++>
      (fun _ -> p_disj ++>  (fun cond -> (terminal_res_blanc (fun c -> if c = ')' then Some () else None)) ++>
      (fun _ -> (terminal_res_blanc (fun c -> if c = '{' then Some () else None)) ++>
      (fun _ -> p_prog ++>
      (fun body -> (terminal_res_blanc (fun c -> if c = '}' then Some () else None)) ++>
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

(* Bloc vide *)
let _ = show_ast "w(a){}" 
let _ = show_ast "a:=0" 

(*Tests aspirateurs de blancs*)
let _ = show_ast "a := 0"
let _ = show_ast "a:=0;       b:=1"
let _ = show_ast "a:=0; \n\n \t b:=1"
let _ = show_ast "w     (  1) {  \t a:= 0  ; b := 1}"
let _ = show_ast "i   (  b) {  \t a:= 0  ; b := 1}{ b:=0\n}"
    

(* Attention : On retrourne Skip lorsqu'on ne reconnait pas une instruction/variable (p_skip = epsilon) *)
let _ = show_ast "z:=0" 
(* Valeur hors limites '9' *)
let _ = show_ast "a:=9"

let _ = show_ast "w   (!a) \t  {  \t a:= 0  ; b := 1}"
let _ = show_ast "i    \t (!a.b+!c\n  )  {  \t a:= 0  ; b := 1}{ b:=0\n}"

 

(*Exercice 2.2.1*)

let rec update (s : state) (n : int) (v : int) : state =
  match s, n with
  | [], _ -> []
  | _ :: rest, 0 -> v :: rest
  | x :: rest, _ -> x :: update rest (n - 1) v

  let rec get (s : state) (n : int) : int =
    match s, n with
    | [], _ -> 0
    | x :: _, 0 -> x
    | _ :: rest, _ -> get rest (n - 1)

let evalA (e : aexp) (s : state) : int =
  match e with
  | Aco n -> n
  | Ava n -> get s n
  | _ -> failwith "evalA: expression is not an arithmetic expression"

let rec evalB (e : aexp) (s : state) : bool =
  match e with
  | Ava _ ->
      let v = evalA e s in
      if v = 0 then false else true
  | Aco 1 -> true
  | Aco 0 -> false
  | Bnot e1 -> not (evalB e1 s)
  | Bconj (e1, e2) -> (evalB e1 s) && (evalB e2 s)
  | Bdisj (e1, e2) -> (evalB e1 s) || (evalB e2 s)
  | _ -> failwith "evalB: expression is not a boolean expression"

    

let rec exec (i : instr) (s : state) : state =
  match i with
 
  | Skip ->
      s

  | Assign (l, r) ->
      (match l with
       | Ava k ->
           let v = evalA r s in
           update s k v
       | _ ->
           failwith "Assign: left-hand side must be a variable (Ava i)")


  | Seq (i1, i2) ->
      let s1 = exec i1 s in
      exec i2 s1


  | If (cond, i1, i2) ->
      if evalB cond s
      then exec i1 s
      else exec i2 s


  | While (cond, body) ->
      let rec loop s =
        if evalB cond s then
          let s1 = exec body s in
          loop s1
        else
          s
      in
      loop s

(*==========TESTS===========*)
let string_of_state (s : state) : string =
  (* Associer chaque indice à un nom de variable *)
  let var_name i =
    match i with
    | 0 -> "a"
    | 1 -> "b"
    | 2 -> "c"
    | 3 -> "d"
    | n -> "x" ^ string_of_int n
  in
  let rec aux i st =
    match st with
    | [] -> ""
    | x :: xs ->
        let name = var_name i in
        let rest = aux (i + 1) xs in
        match rest with
        | "" -> Printf.sprintf "%s=%d" name x
        | _  -> Printf.sprintf "%s=%d; %s" name x rest
  in
  aux 0 s

let run (code : string) (s0 : state) : state =
  let (ast, rest) = p_prog (list_of_string code) in
  if rest <> [] then
    failwith "Parsing incomplet : des caractères restent à analyser"
  else
    let sf = exec ast s0 in
    Printf.printf "Programme : %S\n" code;
    Printf.printf "AST       : %s\n" (string_of_instr ast);
    Printf.printf "Etat init : %s\n" (string_of_state s0);
    Printf.printf "Etat fin  : %s\n\n" (string_of_state sf);
    sf

(* état initial : a=b=c=d=0 *)
let s0 = [0; 0; 0; 0];;

(* 1) Affectation simple *)
let _ = run "a:=1" s0;;

(* 2) Deux assignations *)
let _ = run "a:=1;b:=1" s0;;

(* 3) If : i(a){b:=1}{b:=0} *)
let s1 = [1; 0; 0; 0];;
let _ = run "i(a){b:=1}{b:=0}" s1;;

(* 4) While simple : w(a){a:=0}  *)
let s2 = [1; 0; 0; 0];;
let _ = run "w(a){a:=0}" s2;;

(* 5) *)
let _ =run "a:=1;b:=1;c:=1;w(a){i(c){c:=0;a:=b}{b:=0;c:=a}}" [0;0;0;0];;

(* 6) Swap avec variable temporaire *)
let swap = [1; 0; 0; 0]
let _ = run "c:=a; a:=b; b:=c" swap
(* Résultat attendu : a=0; b=1; c=1; d=0 *)


(* 7) Test des constantes booléennes dans un IF *)
let _ = run "i(1){a:=1}{a:=0}; i(0){b:=1}{b:=0}" [0;0;0;0]
(* État fin : a=1; b=0; c=0; d=0 *)


(* 8) While *)
let _ = run "a:=1; w(a){ a:=0; b:=1 }" [0;0;0;0]
(* État fin : a=0; b=1; c=0; d=0 *)


(* 9) Ecrasement de variable *)
let _ = run "a:=1; a:=0; a:=1" [0;0;0;0]
(*  État fin attendu : a=1; b=0, c=0, d=0 *)


(* 10) a AND b avec des if*)
let a_and_b = [1; 1; 0; 0] (* a=1, b=1 *)
let _ = run "c:=0; i(a){ i(b){ c:=1 }{ c:=0 } }{ c:=0 }"  a_and_b
(* État fin attendu : a=1; b=1; c=1; d=0 *)
  


(* TESTS COMPLEXES / CAS LIMITES *)


(* Test 11 : Blocs vides*)
let _ = run "i(a){}{a:=1}" [0;0;0;0];;
(* Résultat attendu : state = [1; 0; 0; 0] *)


(* Test 12 : Blancs *)
let code_blancs = "
    a 
    := 
    1 
    ; 
    
    w 
    ( 
       a 
    ) 
    { 
       i(  a  )  
       { 
          a := 0 
       } 
       { 
          b := 1 
       } 
    }
"
let _ = run code_blancs [0;0;0;0];;
(* Résultat attendu : state = [0; 0; 0; 0] *)


(* Test 13 : Imbrication profonde (While dans If dans While) *)
let imbrication = "b:=1; w(b){ i(a){ a:=1 }{ i(b){ b:=0 }{ a:=1 } } }"
let _ = run imbrication [0;0;0;0];;
(* Résultat attendu : state = [0; 0; 0; 0] *)


(* Test 14 : Le piège du Point-Virgule final *)
let _ = run "a:=1;" [0;0;0;0];;
(* Résultat attendu : state = [1; 0; 0; 0] *)


(* Test 15 : Priorité d'exécution (Séquence vs While) *)
let _ = run "a:=1; w(a){ a:=0 }; b:=1" [0;0;0;0];;
(* Résultat attendu : state = [0; 1; 0; 0] *)


(* Test 16 : Erreur de syntaxe volontaire (Valeur non gérée) *)
let _ = 
  try 
    let _ = run "a:=2" [0;0;0;0] in 
    print_endline "ECHEC : Le test doit échouer car p_val ne connait que 0 et 1"
  with Failure msg -> 
    Printf.printf "SUCCES : Erreur capturée correctement -> %s\n" msg
