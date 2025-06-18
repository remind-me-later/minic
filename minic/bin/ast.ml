type ident = string
type ty = IntTy | BoolTy | VoidTy | FunTy of ty list * ty

let rec show_ty = function
  | IntTy -> "int"
  | BoolTy -> "bool"
  | VoidTy -> "void"
  | FunTy (args, ret) ->
      Printf.sprintf "fun(%s) -> %s"
        (String.concat ", " (List.map show_ty args))
        (show_ty ret)

type op = AddOp | SubOp | MulOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

let show_op = function
  | AddOp -> "+"
  | SubOp -> "-"
  | MulOp -> "*"
  | EqOp -> "=="
  | NeqOp -> "!="
  | LtOp -> "<"
  | LeOp -> "<="
  | GtOp -> ">"
  | GeOp -> ">="

type 'a expa = { exp_annot : 'a; exp_value : 'a exp }

and 'a exp =
  | IntExp of int
  | VarExp of ident
  | BinOpExp of op * 'a expa * 'a expa
  | CallExp of ident * 'a expa list

let rec show_exp_annot { exp_annot; exp_value } =
  Printf.sprintf "ExpAnnot(%s, %s)" exp_annot (show_exp exp_value)

and show_exp = function
  | IntExp n -> string_of_int n
  | VarExp id -> id
  | BinOpExp (op, l, r) ->
      Printf.sprintf "(%s %s %s)" (show_exp_annot l) (show_op op)
        (show_exp_annot r)
  | CallExp (id, args) ->
      Printf.sprintf "%s(%s)" id
        (String.concat ", " (List.map show_exp_annot args))

type vardef = VarDef of ident * ty

let show_vardef (VarDef (id, t)) = Printf.sprintf "%s: %s" id (show_ty t)

type 'a stmt =
  | ExprStmt of 'a expa
  | DefStmt of vardef * 'a expa
  | AssignStmt of ident * 'a expa
  | IfStmt of 'a expa * 'a stmt list * 'a stmt list option
  | WhileStmt of 'a expa * 'a stmt list
  | ReturnStmt of 'a expa option

let rec show_stmt = function
  | ExprStmt e -> Printf.sprintf "ExprStmt(%s)" (show_exp_annot e)
  | DefStmt (vd, e) ->
      Printf.sprintf "DefStmt(%s, %s)" (show_vardef vd) (show_exp_annot e)
  | AssignStmt (id, e) ->
      Printf.sprintf "AssignStmt(%s = %s)" id (show_exp_annot e)
  | IfStmt (cond, then_block, else_block) ->
      Printf.sprintf "IfStmt(%s, %s, %s)" (show_exp_annot cond)
        (String.concat "; " (List.map show_stmt then_block))
        (match else_block with
        | Some b -> String.concat "; " (List.map show_stmt b)
        | None -> "None")
  | WhileStmt (cond, body) ->
      Printf.sprintf "WhileStmt(%s, %s)" (show_exp_annot cond)
        (String.concat "; " (List.map show_stmt body))
  | ReturnStmt e ->
      Printf.sprintf "ReturnStmt(%s)"
        (match e with Some exp -> show_exp_annot exp | None -> "None")

type ('a, 'b) block = { block_annot : 'b; block_stmts : 'a stmt list }

let show_block b =
  Printf.sprintf "Block(%s, [%s])" b.block_annot
    (String.concat "; " (List.map show_stmt b.block_stmts))

type ('a, 'b) fundef = FunDef of ident * ty * vardef list * ('a, 'b) block

let show_fundef (FunDef (name, t, args, body)) =
  Printf.sprintf "FunDef(%s: %s, args: [%s], body: %s)" name (show_ty t)
    (String.concat "; " (List.map show_vardef args))
    (show_block body)

type ('a, 'b) program = ('a, 'b) fundef list

let show_program p =
  Printf.sprintf "Program: [%s]" (String.concat "; " (List.map show_fundef p))
