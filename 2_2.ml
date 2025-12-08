  #use "anacomb.ml";;
  #use "partie_1.ml";;
  #use "2_1_4.ml";;

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
let _ = run "c:=0; i(a.b){c:=1}{}" a_and_b    

let a_or_b = [0; 0; 0; 0]
let _ = run "c:=0; w(!(a+b)){c:=1; a:=1}" a_or_b            


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
