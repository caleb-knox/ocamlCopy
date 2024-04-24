open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input =
  let input_len = String.length input in

  let rec tok pos =
    
    if pos >= input_len then
      []
    else if Str.string_match (Str.regexp "[0-9]+\\|(-[0-9]+)") input pos then 
      let num_str = Str.matched_string input in 
      let num = if String.contains num_str '(' then
        int_of_string (String.sub num_str 1 (String.length(num_str) - 2))
      else
        int_of_string(num_str)
      in
    Tok_Int(num) :: tok (pos + String.length(num_str))

    else if Str.string_match (Str.regexp "(") input pos then Tok_LParen::(tok (pos + 1))

    else if Str.string_match (Str.regexp ")") input pos then Tok_RParen::(tok (pos + 1))

    else if Str.string_match (Str.regexp ">=") input pos then Tok_GreaterEqual::(tok (pos + 2))

    else if Str.string_match (Str.regexp "<=") input pos then Tok_LessEqual::(tok (pos + 2))

    else if Str.string_match (Str.regexp "=") input pos then Tok_Equal::(tok (pos + 1))

    else if Str.string_match (Str.regexp "<>") input pos then Tok_NotEqual::(tok (pos + 2))

    else if Str.string_match (Str.regexp "->") input pos then Tok_Arrow::(tok (pos + 2))

    else if Str.string_match (Str.regexp ">") input pos then Tok_Greater::(tok (pos + 1))

    else if Str.string_match (Str.regexp "<") input pos then Tok_Less::(tok (pos + 1))

    else if Str.string_match (Str.regexp "||") input pos then Tok_Or::(tok (pos + 2))

    else if Str.string_match (Str.regexp "&&") input pos then Tok_And::(tok (pos + 2))

    else if Str.string_match (Str.regexp "not") input pos then Tok_Not::(tok (pos + 3))

    else if Str.string_match (Str.regexp "if") input pos then Tok_If::(tok (pos + 2))

    else if Str.string_match (Str.regexp "then") input pos then Tok_Then::(tok (pos + 4))

    else if Str.string_match (Str.regexp "else") input pos then Tok_Else::(tok (pos + 4))

    else if Str.string_match (Str.regexp "+") input pos then Tok_Add::(tok (pos + 1))

    else if Str.string_match (Str.regexp "-") input pos then Tok_Sub::(tok (pos + 1))

    else if Str.string_match (Str.regexp "*") input pos then Tok_Mult::(tok (pos + 1))

    else if Str.string_match (Str.regexp "/") input pos then Tok_Div::(tok (pos + 1))

    else if Str.string_match (Str.regexp "\\^") input pos then Tok_Concat::(tok (pos + 1))

    else if Str.string_match (Str.regexp "let") input pos then Tok_Let::(tok (pos + 3))

    else if Str.string_match (Str.regexp "def") input pos then Tok_Def::(tok (pos + 3))

    else if Str.string_match (Str.regexp "in") input pos then Tok_In::(tok (pos + 2))

    else if Str.string_match (Str.regexp "rec") input pos then Tok_Rec::(tok (pos + 3))

    else if Str.string_match (Str.regexp "fun") input pos then Tok_Fun::(tok (pos + 3))

    else if Str.string_match (Str.regexp ";;") input pos then Tok_DoubleSemi::(tok (pos + 2))

    else if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9_]*") input pos then
      let id = Str.matched_string input in
      if id = "true" then
        Tok_Bool(true)::(tok (pos + 4))
      else if id = "false" then 
        Tok_Bool(false)::(tok (pos + 5))
      else
        Tok_ID(id)::(tok (pos + String.length id))

    else if Str.string_match (Str.regexp "true") input pos then Tok_Bool(true)::(tok (pos + 4))

    else if Str.string_match (Str.regexp "false") input pos then Tok_Bool(false)::(tok (pos + 5))

    else if Str.string_match (Str.regexp "\"[^\"]*\"") input pos then 
      let str_val = Str.matched_string input in
      if String.length(str_val) = 2 then 
        raise(InvalidInputException "")
      else
        let sanitized_string = String.sub str_val 1 (String.length(str_val) - 2) in
        Tok_String(sanitized_string)::(tok (pos + String.length(str_val)))

    else tok (pos + 1)
  in
  tok 0