module Ast.Parse (program) where

import Ast.Types
import Control.Applicative (Alternative (many, (<|>)), optional)
import Data.Functor (($>))
import Text.ParserCombinators.Parsec qualified as PC
import TypeSystem

comment :: PC.Parser ()
comment =
  PC.string "//"
    *> many (PC.satisfy (/= '\n'))
    *> ( PC.try (PC.char '\n')
           $> ()
           <|> PC.eof
       )

-- Space consumer: skips whitespace and comments
sc :: PC.Parser ()
sc = PC.skipMany (PC.try comment <|> PC.space $> ())

lex :: PC.Parser a -> PC.Parser a
lex p = p <* sc

keyword :: String -> PC.Parser String
keyword kw = Ast.Parse.lex (PC.string kw)

ifkw :: PC.Parser String
ifkw = keyword "if"

whilekw :: PC.Parser String
whilekw = keyword "while"

forkw :: PC.Parser String
forkw = keyword "for"

elsekw :: PC.Parser String
elsekw = keyword "else"

returnkw :: PC.Parser String
returnkw = keyword "return"

symbol :: String -> PC.Parser String
symbol sym = Ast.Parse.lex (PC.string sym)

parens :: PC.Parser a -> PC.Parser a
parens p = symbol "(" *> p <* symbol ")"

brackets :: PC.Parser a -> PC.Parser a
brackets p = symbol "[" *> p <* symbol "]"

braces :: PC.Parser a -> PC.Parser a
braces p = symbol "{" *> p <* symbol "}"

commaSep :: PC.Parser a -> PC.Parser [a]
commaSep p = PC.sepBy p (symbol ",")

identifier :: PC.Parser Id
identifier =
  let isFirstChar c = c `elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_"
      isOtherChar c = isFirstChar c || c `elem` ['0' .. '9']
   in Ast.Parse.lex (liftA2 (:) (PC.satisfy isFirstChar) (many (PC.satisfy isOtherChar)))

basicty :: PC.Parser Ty
basicty = Ast.Parse.lex (parseInt <|> parseChar <|> parseBool <|> parseVoid)
  where
    parseInt = keyword "int" $> IntTy
    parseChar = keyword "char" $> CharTy
    parseBool = keyword "bool" $> BoolTy
    parseVoid = keyword "void" $> VoidTy

ty :: PC.Parser Ty
ty = do
  baseTy <- basicty
  maybePtr <- optional (symbol "*" $> PtrTy {ptrTyElemTy = baseTy})
  case maybePtr of
    Just ptrTy -> return ptrTy
    Nothing -> return baseTy

num :: PC.Parser Int
num =
  let isDigit c = c `elem` ['0' .. '9']
   in read <$> Ast.Parse.lex (PC.many1 (PC.satisfy isDigit))

char :: PC.Parser Char
char = Ast.Parse.lex $ do
  _ <- PC.char '\''
  c <- PC.try (PC.char '\\' *> escapeChar) <|> PC.satisfy validChar
  _ <- PC.char '\''
  return c
  where
    validChar c = c /= '\'' && c /= '\\' && c >= ' ' && c <= '~'
    escapeChar =
      PC.choice
        [ PC.char '\\' $> '\\',
          PC.char '\'' $> '\'',
          PC.char '\"' $> '\"',
          PC.char 'n' $> '\n',
          PC.char 'r' $> '\r',
          PC.char 't' $> '\t'
        ]

expression :: PC.Parser RawExp
expression = eqexp
  where
    factor =
      PC.try callexp
        <|> PC.try arraccess
        <|> PC.try idexp
        <|> PC.try parenexp
        <|> PC.try charexp
        <|> PC.try takeAddress
        <|> numexp
      where
        numexp = do
          numberValue <- num
          return Exp {_expAnnot = (), _expInner = NumberExp {numberValue}}
        charexp = do
          charValue <- char
          return Exp {_expAnnot = (), _expInner = CharExp {charValue}}
        idexp = do
          idName <- identifier
          return Exp {_expAnnot = (), _expInner = IdExp {idName}}
        parenexp = parens expression
        callexp = do
          callId <- identifier
          callArgs <- parens (commaSep expression)
          return Exp {_expAnnot = (), _expInner = Call {callId, callArgs}}
        arraccess = do
          arrId <- identifier
          arrIndex <- brackets expression
          return Exp {_expAnnot = (), _expInner = ArrAccess {arrId, arrIndex}}
        takeAddress = do
          _ <- symbol "&"
          takeAddressId <- identifier
          return Exp {_expAnnot = (), _expInner = TakeAddress {takeAddressId}}

    unaryexp = do
      op <-
        optional $
          PC.try (symbol "-" $> UnarySub)
            <|> PC.try (symbol "!" $> UnaryNot)
            <|> (symbol "*" $> UnaryPtrDeref)
      case op of
        Just unaryOp -> do
          e <- PC.try unaryexp <|> factor
          return Exp {_expAnnot = (), _expInner = UnaryExp {unaryOp, unaryExp = e}}
        Nothing -> factor

    mulexp = do
      binLeft <- unaryexp
      maybeOp <-
        optional
          ( PC.try (symbol "*") $> Mul
              <|> PC.try (symbol "/" $> Div)
              <|> symbol "%" $> Mod
          )
      case maybeOp of
        Just binOp -> do
          binRight <- mulexp
          return Exp {_expAnnot = (), _expInner = BinExp {binLeft, binOp, binRight}}
        Nothing -> return binLeft

    addsubexp = do
      binLeft <- mulexp
      maybeOp <-
        optional
          ( PC.try
              (symbol "+" $> Add)
              <|> symbol "-" $> Sub
          )
      case maybeOp of
        Just binOp -> do
          binRight <- addsubexp
          return Exp {_expAnnot = (), _expInner = BinExp {binLeft, binOp, binRight}}
        Nothing -> return binLeft

    eqexp = do
      binLeft <- addsubexp
      maybeOp <-
        optional
          ( PC.try (symbol "==" $> Equal)
              <|> PC.try (symbol "!=" $> NotEqual)
              <|> PC.try (symbol "<=" $> LessThanOrEqual)
              <|> PC.try (symbol ">=" $> GreaterThanOrEqual)
              <|> PC.try (symbol "<" $> LessThan)
              <|> symbol ">" $> GreaterThan
          )
      case maybeOp of
        Just binOp -> do
          binRight <- eqexp
          return Exp {_expAnnot = (), _expInner = BinExp {binLeft, binOp, binRight}}
        Nothing -> return binLeft

vardef :: PC.Parser VarDef
vardef = do
  _varDefTy <- ty
  _varDefId <- identifier
  return VarDef {_varDefId, _varDefTy}

letstmt :: PC.Parser RawStmt
letstmt = do
  storageSpec <- optional storageSpecifier
  letVarDef <- vardef
  _ <- symbol "="
  letExp <- expression
  return $ LetStmt {letVarDef, letExp, letStorage = storageSpec}

letarrstmt :: PC.Parser RawStmt
letarrstmt = do
  storageSpec <- optional storageSpecifier
  letArrVarDef <- vardef
  letArrSize <- brackets num
  _ <- symbol "="
  letArrElems <- braces (commaSep expression)
  return $ LetArrStmt {letArrVarDef, letArrSize, letArrElems, letArrStorage = storageSpec}

assignstmt :: PC.Parser RawStmt
assignstmt = do
  assignId <- identifier
  _ <- symbol "="
  assignExp <- expression
  return $ AssignStmt {assignId, assignExp}

assignarrstmt :: PC.Parser RawStmt
assignarrstmt = do
  assignArrId <- identifier
  assignArrIndex <- brackets expression
  _ <- symbol "="
  assignArrExp <- expression
  return $ AssignArrStmt {assignArrId, assignArrIndex, assignArrExp}

retstmt :: PC.Parser RawStmt
retstmt = do
  _ <- returnkw
  returnExp <- optional expression
  return $ ReturnStmt {returnExp}

expstmt :: PC.Parser RawStmt
expstmt = do
  stmtExp <- expression
  return ExpStmt {stmtExp}

semicolonStmt :: PC.Parser RawStmt
semicolonStmt = do
  parsedStatement <-
    PC.try letstmt
      <|> PC.try letarrstmt
      <|> PC.try retstmt
      <|> PC.try assignstmt
      <|> PC.try assignarrstmt
      <|> expstmt
  _ <- symbol ";"
  return parsedStatement

stmt :: PC.Parser RawStmt
stmt =
  PC.try ifstmt
    <|> PC.try whilestmt
    <|> PC.try forstmt
    <|> PC.try semicolonStmt
    <|> expstmt
  where
    ifstmt = do
      _ <- ifkw
      ifCond <- parens expression
      ifBody <- block
      ifElseBody <- optional (elsekw *> block)
      return $ IfStmt {ifCond, ifBody, ifElseBody}

    whilestmt = do
      _ <- whilekw
      whileCond <- parens expression
      whileBody <- block
      return WhileStmt {whileCond, whileBody}

    forstmt = do
      _ <- forkw
      _ <- symbol "("
      forInit <- assignstmt
      _ <- symbol ";"
      ExpStmt {stmtExp = forCond} <- expstmt
      _ <- symbol ";"
      forUpdate <- assignstmt
      _ <- symbol ")"
      forBody <- block
      return ForStmt {forInit, forCond, forUpdate, forBody}

block :: PC.Parser RawBlock
block = do
  _blockStmts <- braces (many stmt)
  return $ Block {_blockAnnot = (), _blockStmts}

storageSpecifier :: PC.Parser StorageSpecifier
storageSpecifier =
  PC.try (keyword "static" $> Static)
    <|> PC.try (keyword "extern" $> Extern)
    <|> keyword "auto" $> Auto

fun :: PC.Parser RawFun
fun = do
  _funRetTy <- ty
  _funId <- identifier
  _funArgs <- parens (commaSep vardef)
  _funBody <- block
  return Fun {_funId, _funArgs, _funRetTy, _funBody}

externfun :: PC.Parser RawExternFun
externfun = do
  _ <- keyword "extern"
  externFunRetTy <- ty
  externFunId <- identifier
  externFunArgs <- parens (commaSep vardef)
  _ <- symbol ";"
  return
    ExternFun
      { externFunId,
        externFunArgs = _varDefTy <$> externFunArgs,
        externFunRetTy
      }

program :: PC.Parser RawProgram
program = do
  _ <- sc
  defs <- many (PC.try (Left <$> fun) <|> (Right <$> externfun))
  _ <- PC.eof

  let initialProgram = Program {programAnnot = (), programFuncs = [], programExternFuns = [], programMainFun = Nothing}
  let programResult =
        foldl
          ( \acc f -> case f of
              Left topLevelFun ->
                case _funId topLevelFun of
                  "main" -> acc {programMainFun = Just topLevelFun}
                  _ -> acc {programFuncs = programFuncs acc ++ [topLevelFun]}
              Right externFun -> acc {programExternFuns = programExternFuns acc ++ [externFun]}
          )
          initialProgram
          defs

  return programResult
