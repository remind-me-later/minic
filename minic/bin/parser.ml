open Angstrom
open Ast

let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false
let ws = skip_while is_space
let lex p = p <* ws
let keyword w = lex (string w)
let symbol s = lex (string s)

let parse_ident =
  let is_first = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false
  in
  let is_rest = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  lex
    (lift2
       (fun c cs -> String.of_seq (List.to_seq (c :: cs)))
       (satisfy is_first)
       (many (satisfy is_rest)))

let parse_integer =
  lex (take_while1 (function '0' .. '9' -> true | _ -> false))
  >>| int_of_string
  >>| fun n -> n

let parens p = symbol "(" *> p <* symbol ")"

let parse_exp : unit exp t =
  fix (fun (parse_exp : unit exp t) ->
      let parse_var_exp : unit exp t =
        parse_ident >>= fun id -> return (VarExp id)
      in
      let parse_int_exp : unit exp t =
        parse_integer >>= fun n -> return (IntExp n)
      in
      let parse_call : unit exp t =
        let* id = parse_ident in
        let* args = parens (sep_by (symbol ",") parse_exp) in
        return (CallExp (id, args))
      in
      let parse_factor : unit exp t =
        parse_int_exp <|> parens parse_exp <|> parse_call <|> parse_var_exp
      in
      let parse_mul : unit exp t =
        let* left = parse_factor in
        let* op = option None (symbol "*" >>| fun _ -> Some MulOp) in
        match op with
        | Some _ ->
            let* right = parse_exp in
            return (BinOpExp (MulOp, expa () left, right))
        | None -> return left
      in
      let parse_add_sub : unit exp t =
        let* left = parse_mul in
        let* op =
          option None
            ( symbol "+" <|> symbol "-" >>| fun s ->
              Some (if s = "+" then AddOp else SubOp) )
        in
        match op with
        | Some op ->
            let* right = parse_exp in
            return (BinOpExp (op, left, right))
        | None -> return left
      in
      let parse_cmp : exp t =
        let* left = parse_add_sub in
        let* op =
          option None
            ( symbol "==" <|> symbol "!=" <|> symbol "<" <|> symbol "<="
              <|> symbol ">" <|> symbol ">="
            >>| fun s ->
              match s with
              | "==" -> Some EqOp
              | "!=" -> Some NeqOp
              | "<" -> Some LtOp
              | "<=" -> Some LeOp
              | ">" -> Some GtOp
              | _ -> Some GeOp )
        in
        match op with
        | Some op ->
            let* right = parse_exp in
            return (BinOpExp (op, left, right))
        | None -> return left
      in
      parse_cmp)

let parse_ty =
  keyword "int" *> return IntTy
  <|> keyword "bool" *> return BoolTy
  <|> keyword "void" *> return VoidTy

let parse_vardef = lift2 (fun t id -> VarDef (id, t)) parse_ty parse_ident

let parse_stmt =
  fix (fun parse_stmt ->
      let expr_stmt =
        parse_exp >>= fun e -> symbol ";" *> return (ExprStmt e)
      in
      let def_stmt =
        parse_vardef >>= fun vd ->
        symbol "=" *> parse_exp >>= fun e ->
        symbol ";" *> return (DefStmt (vd, e))
      in
      let assign_stmt =
        lift2
          (fun id e -> AssignStmt (id, e))
          parse_ident
          (symbol "=" *> parse_exp <* symbol ";")
      in
      let parse_block : stmt list t =
        symbol "{" *> many parse_stmt <* symbol "}"
      in
      let if_stmt =
        lift3
          (fun cond then_block else_block ->
            IfStmt (cond, then_block, else_block))
          (keyword "if" *> parens parse_exp)
          parse_block
          (option None (keyword "else" *> (parse_block >>| fun b -> Some b)))
      in
      let parse_while_stmt =
        lift2
          (fun cond body -> WhileStmt (cond, body))
          (keyword "while" *> parens parse_exp)
          parse_block
      in
      let parse_return_stmt =
        keyword "return" *> option None (parse_exp >>| fun e -> Some e)
        <* symbol ";"
        >>| fun e -> ReturnStmt e
      in

      expr_stmt <|> def_stmt <|> assign_stmt <|> if_stmt <|> parse_while_stmt
      <|> parse_return_stmt)

let parse_block = symbol "{" *> many parse_stmt <* symbol "}"

let parse_fundef =
  lift4
    (fun t name args body -> FunDef (name, t, args, body))
    parse_ty parse_ident
    (parens (sep_by (symbol ",") parse_vardef))
    parse_block

let parse_program = ws *> many parse_fundef <* end_of_input
