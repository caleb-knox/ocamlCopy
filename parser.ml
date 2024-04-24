open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)
let rec parse_expr toks = 
  match lookahead toks with
    | Some Tok_Let -> parse_let_expr toks
    | Some Tok_If -> parse_if_expr toks
    | Some Tok_Fun -> parse_fun_expr toks
    | _ -> parse_or_expr toks

and parse_let_expr toks =
  match lookahead toks with
    | Some Tok_Let -> 
        let t = match_token toks Tok_Let in
        let (t', bool_val) = match lookahead t with
          | Some Tok_Rec -> 
              (match_token t Tok_Rec, true)
          | _ -> 
              (t, false)
        in
        begin match lookahead t' with
          | Some Tok_ID id_val -> 
              let t'' = match_token t' (Tok_ID id_val) in
              let t''' = match_token t'' Tok_Equal in
              let (t'''', expr_val) = parse_expr t''' in
              let t''''' = match_token t'''' Tok_In in
              let (t'''''', expr_val') = parse_expr t''''' in
              (t'''''', Let (id_val, bool_val, expr_val, expr_val'))
          | _ -> raise (InvalidInputException "Error line 61.") end
    | _ -> raise (InvalidInputException "Error line 62.")

and parse_if_expr toks =
  match lookahead toks with
    | Some Tok_If -> let t = match_token toks Tok_If in
        let (t', expr_val) = parse_expr t in
        let t'' = match_token t' Tok_Then in
        let (t''', expr_val') = parse_expr t'' in
        let t'''' = match_token t''' Tok_Else in
        let (t''''', expr_val'') = parse_expr t'''' in
        (t''''', If (expr_val, expr_val', expr_val''))
    | _ -> raise (InvalidInputException "Error line 73.")

and parse_fun_expr toks =
  match lookahead toks with
    | Some Tok_Fun -> let t = match_token toks Tok_Fun in
        begin match lookahead t with
          | Some Tok_ID id_val ->
            let t' = match_token t (Tok_ID id_val) in
            let t'' = match_token t' Tok_Arrow in
            let (t''', expr_val) = parse_expr t'' in
            (t''', Fun (id_val, expr_val))
          | _ -> raise (InvalidInputException "Error line 84.")
        end
    | _ -> raise (InvalidInputException "Error line 86.")

and parse_or_expr toks =
  let (t, and_expr) = parse_and_expr toks in
  match lookahead t with
    | Some Tok_Or -> let t' = match_token t Tok_Or in
        let (t'', or_expr) = parse_or_expr t' in
        (t'', Binop (Or, and_expr, or_expr))
    | _ -> (t, and_expr)

and parse_and_expr toks =
  let (t, equality_expr) = parse_equality_expr toks in
  match lookahead t with
    | Some Tok_And -> let t' = match_token t Tok_And in
        let (t'', and_expr) = parse_and_expr t' in
        (t'', Binop (And, equality_expr, and_expr))
    | _ -> (t, equality_expr)

and parse_equality_expr toks =
  let (t, relational_expr) = parse_relational_expr toks in
  match lookahead t with
    | Some Tok_Equal -> let t' = match_token t Tok_Equal in
        let (t'', equality_expr) = parse_equality_expr t' in
        (t'', Binop (Equal, relational_expr, equality_expr))
    | Some Tok_NotEqual -> let t' = match_token t Tok_NotEqual in
        let (t'', equality_expr) = parse_equality_expr t' in
        (t'', Binop (NotEqual, relational_expr, equality_expr))
    | _ -> (t, relational_expr)

and parse_relational_expr toks =
  let (t, additive_expr) = parse_additive_expr toks in
  match lookahead t with
    | Some Tok_Less -> let t' = match_token t Tok_Less in
        let (t'', relational_expr) = parse_relational_expr t' in
        (t'', Binop (Less, additive_expr, relational_expr))
    | Some Tok_Greater -> let t' = match_token t Tok_Greater in
        let (t'', relational_expr) = parse_relational_expr t' in
        (t'', Binop (Greater, additive_expr, relational_expr))
    | Some Tok_LessEqual -> let t' = match_token t Tok_LessEqual in
        let (t'', relational_expr) = parse_relational_expr t' in
        (t'', Binop (LessEqual, additive_expr, relational_expr))
    | Some Tok_GreaterEqual -> let t' = match_token t Tok_GreaterEqual in
        let (t'', relational_expr) = parse_relational_expr t' in
        (t'', Binop (GreaterEqual, additive_expr, relational_expr))
    | _ -> (t, additive_expr)

and parse_additive_expr toks =
  let (t, multiplicative_expr) = parse_multiplicative_expr toks in
  match lookahead t with
    | Some Tok_Add -> let t' = match_token t Tok_Add in
        let (t'', additive_expr) = parse_additive_expr t' in
        (t'', Binop (Add, multiplicative_expr, additive_expr))
    | Some Tok_Sub -> 
        let t' = match_token t Tok_Sub in
        let (t'', additive_expr) = parse_additive_expr t' in
        (t'', Binop (Sub, multiplicative_expr, additive_expr))
    | _ -> (t, multiplicative_expr)

and parse_multiplicative_expr toks =
  let (t, concat_expr) = parse_concat_expr toks in
  match lookahead t with
    | Some Tok_Mult -> 
        let t' = match_token t Tok_Mult in
        let (t'', multiplicative_expr) = parse_multiplicative_expr t' in
        (t'', Binop (Mult, concat_expr, multiplicative_expr))
    | Some Tok_Div -> 
        let t' = match_token t Tok_Div in
        let (t'', multiplicative_expr) = parse_multiplicative_expr t' in
        (t'', Binop (Div, concat_expr, multiplicative_expr))
    | _ -> 
        (t, concat_expr)

and parse_concat_expr toks =
  let (t, unary_expr) = parse_unary_expr toks in
  match lookahead t with
    | Some Tok_Concat -> let t' = match_token t Tok_Concat in
        let (t'', concat_expr) = parse_concat_expr t' in
        (t'', Binop (Concat, unary_expr, concat_expr))
    | _ -> 
        (t, unary_expr)

and parse_unary_expr toks =
  match lookahead toks with
    | Some Tok_Not -> 
        let t = match_token toks Tok_Not in
        let (t', unary_expr) = parse_unary_expr t in
        (t', Not (unary_expr))
    | _ -> parse_function_call_expr toks

and parse_function_call_expr toks =
  let (t, primary_expr) = parse_primary_expr toks in
  match lookahead t with
    | Some Tok_Int integer ->
      let (t', primary_expr') = parse_primary_expr t in
      (t', FunctionCall (primary_expr, primary_expr'))
    | Some Tok_Bool boolean ->
      let (t', primary_expr') = parse_primary_expr t in
      (t', FunctionCall (primary_expr, primary_expr'))
    | Some Tok_String str ->
      let (t', primary_expr') = parse_primary_expr t in
      (t', FunctionCall (primary_expr, primary_expr'))
    | Some Tok_ID id ->
      let (t', primary_expr') = parse_primary_expr t in
      (t', FunctionCall (primary_expr, primary_expr'))
    | Some Tok_LParen ->
      let (t', primary_expr') = parse_primary_expr t in
      (t', FunctionCall (primary_expr, primary_expr'))
  | _ -> (t, primary_expr)

and parse_primary_expr toks =
  match lookahead toks with
    | Some Tok_Int integer -> 
        let t = match_token toks (Tok_Int integer) in
        (t, Value (Int integer))
    | Some Tok_Bool boolean -> 
        let t = match_token toks (Tok_Bool boolean) in
        (t, Value (Bool boolean))
    | Some Tok_String str -> 
        let t = match_token toks (Tok_String str) in
        (t, Value (String str))
    | Some Tok_ID id -> 
        let t = match_token toks (Tok_ID id) in
        (t, ID (id))
    | Some Tok_LParen -> 
        let t = match_token toks Tok_LParen in
        let (t', expr_val) = parse_expr t in
        let t'' = match_token t' Tok_RParen in
        (t'', expr_val)
    | _ -> raise (InvalidInputException "Error line 214.")
  

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match lookahead toks with
    | Some Tok_Def ->
        let t = match_token toks Tok_Def in
        begin match lookahead t with
          | Some Tok_ID id_val -> 
              let t' = match_token t (Tok_ID id_val) in
              let t'' = match_token t' Tok_Equal in
              let (t''', expr_val) = parse_expr t'' in
              let t'''' = match_token t''' Tok_DoubleSemi in
              (t'''', Def (id_val, expr_val))
          | _ -> raise (InvalidInputException "Error line 230.")
          end
    | Some Tok_DoubleSemi ->
        let t = match_token toks Tok_DoubleSemi in
        (t, NoOp)
    | _ -> 
        let (t, expr_val) = parse_expr toks in
        let t' = match_token t Tok_DoubleSemi in
        (t', Expr (expr_val))