type lexeme =
  (* Mots-clefs: *)
  | Select
  | From
  | Where
  | As
  | And | Or | Not
  | OrderBy | Asc | Desc
  | IsNull | IsNotNull
  | Limit | Offset
  | Union | Intersect | Except
  | Join | On
  | Min | Max | Sum | Avg
  | Count | Distinct
  | GroupBy | Having
  (* Autres lexemes *)
  | Asterisque
  | ParG | ParD
  | Virgule
  | Nom of string
  | Point
  | Egal | NonEgal
  | PlusGrand | PlusPetit
  | PlusGrandEgal | PlusPetitEgal
  | String of string
  | Valeur of int
  | Fois | Plus | Moins | Div

let colonne_parser (l : lexeme list) : lexeme list = 
    match l with
    | [] -> failwith "empty list"
    | Nom _::Point::Nom _::r -> r
    | Nom _::r -> r
    | _ -> failwith "no col"

let rec expression_parser (l : lexeme list) : lexeme list = 
    let arithm_parser (l : lexeme list) : lexeme list =
        match l with
        | Fois::e 
        | Plus::e
        | Moins::e
        | Div::e -> expression_parser l
        | _ -> l in

    match l with 
    | [] -> []
    | Valeur _::r
    | String _::r -> arithm_parser r
    | ParG::r -> begin match expression_parser r with ParD::q -> q | _ -> failwith "missing paranthesis" end
    | Nom _::r -> arithm_parser (colonne_parser l)
    | _::_ -> failwith "wrong expression"


let condition_parser (l : lexeme list) : lexeme list =
    match expression_parser l with
    | [] -> []
    | Egal::r
    | NonEgal::r 
    | PlusGrand::r 
    | PlusPetit::r 
    | PlusGrandEgal::r 
    | PlusPetitEgal::r -> expression_parser r
    | IsNull::r
    | IsNotNull::r -> r 
    | _::ParG::_ -> failwith "not implemented"
    | q -> q
    

let rec requete_parser (l : lexeme list) : lexeme list = l (** TODO. Q26 **)

let parser (l : lexeme list)  =
  match requete_parser l with
  | [] -> ()
  | _ -> failwith "Lexème inattendu."

let test (i : int) (l : lexeme list) (correct : bool) =
  let result = 
    try
      Printf.printf "Test no %d: " i;
      parser l;
      Printf.printf "Success\n";
      true
    with
    | Failure s -> (Printf.printf "Erreur : %s\n" s; false)
  in
  if correct <> result then failwith "Echec du test"

let _ =
  test 1 [Select ; Nom("A") ; From ; Nom("T")] true;
  test 2 [Select ; Nom("A") ; From] false;
  test 3 [Select ; Nom("A") ; Point ; Nom("A") ; From ; Nom("T")] true ;
  test 4 [Select ; Nom("A") ; Point ; Nom("A") ; From ; Nom("T") ; Where ; Nom("A") ; Egal ; Valeur(5)] true
