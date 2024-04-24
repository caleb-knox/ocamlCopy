open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)
(*type values = Int of int|Bool of bool|String of string*)

(* Adds mapping [x:v] to environment [env] *)
let ref_extend env x v = (x, ref v)::env

let extend env x v = (x,v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let ref_extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
(* Removes the most recent variable,value binding from the environment *)
let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x)

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e =
  match e with
    | Value v -> 
        begin match v with
          | Int integer -> Int integer
          | Bool boolean -> Bool boolean
          | String str -> String str
          | Closure (environment, var, expr) -> Closure (environment, var, expr)
        end
    | ID id -> ref_lookup env id
    | Fun (id, expr) -> Closure (env, id, expr)
    | Not expr -> 
        let a = eval_expr env expr in
        begin match a with 
          | Bool boolean -> Bool (not boolean)
          | _ -> raise (TypeError "Type error not unary.")
        end
    | Binop (op, expr, expr') -> eval_binop_expr env op expr expr'
    | If (boolean_expr, expr, expr') ->
        let a = eval_expr env boolean_expr in
        begin match a with
          | Bool boolean -> 
            if boolean then 
              eval_expr env expr 
            else 
              eval_expr env expr'
          | _ -> raise (TypeError "Type error if guard.")
        end
    | FunctionCall (expr, expr') ->
        let a = eval_expr env expr in
        begin match a with
          | Closure (environment, id, closure_expr) ->
              let a' = eval_expr env expr' in
              let env' = ref_extend environment id a' in
              eval_expr env' closure_expr
          | _ -> raise (TypeError "Type error function call.")
        end
    | Let (id, boolean, expr, expr') -> 
        if boolean then
          let env' = ref_extend_tmp env id in
          let a = eval_expr env' expr in
          begin match a with
            | Int integer ->
                ref_update env' id (Int integer);
                eval_expr env' expr'
            | Bool boolean ->
                ref_update env' id (Bool boolean);
                eval_expr env' expr'
            | String str -> 
                ref_update env' id (String str);
                eval_expr env' expr'
            | Closure (environment, closure_id, closure_expr) ->
                ref_update environment id (Closure (environment, closure_id, closure_expr));
                eval_expr environment expr'
          end
        else
          let a = eval_expr env expr in
          begin match a with
            | Int integer ->
                let env' = ref_extend env id (Int integer) in
                eval_expr env' expr'
            | Bool boolean ->
                let env' = ref_extend env id (Bool boolean) in
                eval_expr env' expr'
            | String str -> 
                let env' = ref_extend env id (String str) in
                eval_expr env' expr'
            | Closure (environment, closure_id, closure_expr) ->
                let env' = ref_extend environment id (Closure (environment, closure_id, closure_expr)) in
                eval_expr env' expr'
          end


and eval_binop_expr env op expr expr' =
  match op with
    | Add ->
        let a = eval_expr env expr in
        let a' = eval_expr env expr' in
        begin match (a, a') with
          | (Int integer, Int integer') -> Int (integer + integer')
          | _ -> raise (TypeError "Type error adding.")
        end
    | Sub ->
        let a = eval_expr env expr in
        let a' = eval_expr env expr' in
        begin match (a, a') with
          | (Int integer, Int integer') -> Int (integer - integer')
          | _ -> raise (TypeError "Type error subtracting.")
        end
    | Mult ->
        let a = eval_expr env expr in
        let a' = eval_expr env expr' in
        begin match (a, a') with
          | (Int integer, Int integer') -> Int (integer * integer')
          | _ -> raise (TypeError "Type error multiplying.")
        end
    | Div ->
        let a = eval_expr env expr in
        let a' = eval_expr env expr' in
        begin match (a, a') with
          | (Int integer, Int integer') -> 
              if integer' = 0 then
                raise (DivByZeroError)
              else
                Int (integer / integer')
          | _ -> raise (TypeError "Type error adding.")
        end
    | Greater ->
        let a = eval_expr env expr in
        let a' = eval_expr env expr' in
        begin match (a, a') with
          | (Bool boolean, Bool boolean') -> Bool (boolean > boolean')
          | _ -> raise (TypeError "Type error greater.")
        end
    | Less ->
        let a = eval_expr env expr in
        let a' = eval_expr env expr' in
        begin match (a, a') with
          | (Bool boolean, Bool boolean') -> Bool (boolean < boolean')
          | _ -> raise (TypeError "Type error less.")
        end
    | GreaterEqual ->
        let a = eval_expr env expr in
        let a' = eval_expr env expr' in
        begin match (a, a') with
          | (Bool boolean, Bool boolean') -> Bool (boolean >= boolean')
          | _ -> raise (TypeError "Type error greater equal.")
        end
    | LessEqual ->
        let a = eval_expr env expr in
        let a' = eval_expr env expr' in
        begin match (a, a') with
          | (Bool boolean, Bool boolean') -> Bool (boolean <= boolean')
          | _ -> raise (TypeError "Type error less equal.")
        end
    | Concat -> 
        let a = eval_expr env expr in
        let a' = eval_expr env expr' in
        begin match (a, a') with
          | (String str, String str') -> String (str ^ str')
          | _ -> raise (TypeError "Type error concat.")
        end
    | Equal ->
        let a = eval_expr env expr in
        let a' = eval_expr env expr' in
        begin match (a, a') with
          | (Bool boolean, Bool boolean') -> Bool (boolean = boolean')
          | (String str, String str') -> Bool (str = str')
          | (Int integer, Int integer') -> Bool (integer = integer')
          | _ -> raise (TypeError "Type error equal.")
        end
    | NotEqual ->
        let a = eval_expr env expr in
        let a' = eval_expr env expr' in
        begin match (a, a') with
          | (Bool boolean, Bool boolean') -> Bool (boolean <> boolean')
          | (String str, String str') -> Bool (str <> str')
          | (Int integer, Int integer') -> Bool (integer <> integer')
          | _ -> raise (TypeError "Type error not equal.")
        end
    | Or ->
        let a = eval_expr env expr in
        let a' = eval_expr env expr' in
        begin match (a, a') with
          | (Bool boolean, Bool boolean') -> Bool (boolean || boolean')
          | _ -> raise (TypeError "Type error or.")
        end
    | And ->
        let a = eval_expr env expr in
        let a' = eval_expr env expr' in
        begin match (a, a') with
          | (Bool boolean, Bool boolean') -> Bool (boolean && boolean')
          | _ -> raise (TypeError "Type error and.")
        end

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with
    | Def (id, expr) -> 
      let env' = ref_extend_tmp env id in
      let a = eval_expr env' expr in
      begin match a with
        | Int integer ->
            ref_update env' id (Int integer);
            (env', Some (eval_expr env' expr))
        | Bool boolean ->
            ref_update env' id (Bool boolean);
            (env', Some (eval_expr env' expr))
        | String str -> 
            ref_update env' id (String str);
            (env', Some (eval_expr env' expr))
        | Closure (environment, closure_id, closure_expr) ->
            ref_update environment id (Closure (environment, closure_id, closure_expr));
            (env', Some (eval_expr env' expr))
      end
    | Expr expr -> (env, Some (eval_expr env expr))
    | NoOp -> (env, None)