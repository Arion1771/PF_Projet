#use "anacomb.ml";;
#use "partie_1.ml";;


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
  
let p_skip : (instr,char) ranalist =
  epsilon_res (Skip)

let p_assign : (instr, char) ranalist =
  p_var ++> fun i ->
  terminal ':' -->
  terminal '=' -+>
  (p_expr ++> fun e ->
      epsilon_res (Assign(i, e)))

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
    (terminal ('i') --> terminal ('(') -+> p_var ++> fun cond -> terminal (')') -->
    terminal ('{') -+> p_prog ++> fun i1 -> terminal ('}') -->
    terminal ('{') -+> p_prog ++> fun i2 -> terminal ('}') -+>
    epsilon_res (If (cond, i1, i2))) l
and p_while : (instr, char) ranalist =
  fun l ->
    ((terminal_res (fun c -> if c = 'w' then Some () else None)) ++>
    (fun _ -> (terminal_res (fun c -> if c = '(' then Some () else None)) ++>
    (fun _ -> p_var ++>  (fun cond ->
    (terminal_res (fun c -> if c = ')' then Some () else None)) ++>
    (fun _ ->
    (terminal_res (fun c -> if c = '{' then Some () else None)) ++>
    (fun _ ->
    p_prog ++>
    (fun body ->
    (terminal_res (fun c -> if c = '}' then Some () else None)) ++>
    (fun _ -> 
    epsilon_res (While (cond, body)))))))))) l

  (*Exercice 2.1.2*)
  (*Fonctions de tests*)
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

  let run (code : string) =
  let (ast, rest) = p_prog (list_of_string code) in
  if rest <> [] then
    failwith "Parsing incomplet : des caractères restent à analyser"
  else
    Printf.printf "Programme : %S\n" code;
    Printf.printf "AST       : %s\n" (string_of_instr ast);



(*TESTS : Grammaire basique ( fonctionnels ) *)

  let _ = run "a:=0";;
  let _ =  run "a:=0;b:=1";;
  let _ =  run "w(a){a:=1}";;
  let _ =  run "i(a){a:=0}{b:=1}";;
  let _ =  run ";";;
  let _ =  run "a:=1;b:=1;c:=1;w(a){i(c){c:=0;a:=b}{b:=0;c:=a}}";;

(*TESTS : Grammaire basique ( non fonctionnels ) *)

  let _ = run "a:=2";;
  let _ =  run "a:=0;b=1";;
  let _ =  run "w(a:=1){a:=1}";;
  let _ =  run "w(0){a:=1}";;
  let _ =  run "w(a){a:=0}{c:=0;a:=b}";;
  let _ =  run "i(a){a:=0}";;
  let _ =  run "i(a){a:=0}{b:=1}{c:=2}";;
  let _ =  run "w(a+b){i(c){c:=0;a:=b}{b:=0;c:=a}}";;
  let _ =  run "1:=0";;
