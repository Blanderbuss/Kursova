{-# OPTIONS_GHC -Wall #-}
module Parser where

import TypeSystem

import Data.Either
import Text.ParserCombinators.Parsec

fullExpr :: Parser Exp
fullExpr = do spaces;
              ex <- expr 
              eof 
              return ex  

astExpr :: String -> Maybe Exp
astExpr str = case (parse fullExpr "" str) of
               Left _     -> Nothing
               Right e -> Just e

parseExpr :: String -> Either ParseError Exp
parseExpr str = parse fullExpr "" str 

expr :: Parser Exp
expr = constant <|> cond <|> op <|> func <|> var <|> parens expr

constant, var, op,cond,func :: Parser Exp

constant = int <|> bool <|> arrint <|> arrbool

int,bool,arrint,arrbool :: Parser Exp
bool = true <|> false
true,false :: Parser Exp

int = do {n <- lexem number; return $ Const (I n) }
true = do{ reserved "True"; return $ Const (B True)}
false = do{ reserved "False"; return $ Const (B False)}
arrint = do a <- sqparens arri
            return $ Const a
arrbool = do a <- sqparens arrb
             return $ Const a
            
var = do {n <- lower; m <- many(letter<|>digit);spaces; return $ Var $ n:m }

op = addop <|> minusop <|> mulop <|> lessop <|> equalop <|> indexop

cond = do reserved "If"
          con <- expr <|> parens expr
          reserved "Then"
          tr <- expr <|> parens expr
          reserved "Else"
          fl <- expr <|> parens expr
          return (Cond con tr fl)

func = do reserved "Call"
          spaces
          n <- lower 
          m <- many(letter<|>digit)
          spaces
          es <- parens $ many expr
          return $ FunApp (n:m) es

addop, minusop, mulop, lessop, equalop, indexop :: Parser Exp

addop = do reserved "Add"
           e1 <- parens expr <|> expr
           e2 <- parens expr <|> expr
           return $ OpApp Add e1 e2

minusop = do reserved "Minus"
             e1 <- parens expr <|> expr
             e2 <- parens expr <|> expr
             return $ OpApp Minus e1 e2

mulop = do reserved "Mul"
           e1 <- parens expr <|> expr
           e2 <- parens expr <|> expr
           return $ OpApp Mul e1 e2


lessop = do reserved "Less"
            e1 <- parens expr <|> expr
            e2 <- parens expr <|> expr
            return $ OpApp Less e1 e2


equalop = do reserved "Equal"
             e1 <- parens expr <|> expr
             e2 <- parens expr <|> expr
             return $ OpApp Equal e1 e2


indexop = do reserved "Index"
             e1 <- parens expr <|> expr
             e2 <- parens expr <|> expr
             return $ OpApp Index e1 e2

sign :: Parser String 
sign = string "-" <|> pure "" 

number :: Parser Int
number = do s <- sign 
            cs <- many1 digit
            return $ read (s ++ cs)

boolean :: Parser Bool
boolean = do s <- string "True" <|> string "False"
             return $ read s

reserved :: String -> Parser ()
reserved s = do { _ <- string s; spaces} 

lexem :: Parser a -> Parser a
lexem p = do {a <- p; spaces; return a}

sqparens :: Parser a -> Parser a 
sqparens p = do reserved "[" 
                n <- lexem p 
                reserved "]" 
                return n

parens :: Parser a -> Parser a 
parens p = do reserved "(" 
              n <- lexem p 
              reserved ")" 
              return n

arri :: Parser Value
arri = do s <- many membi
          return $ AI (zip [0..] $ s)

membi :: Parser Int
membi = do i <- lexem number
           skipMany $ string ","
           return i

arrb :: Parser Value
arrb = do s <- many membb
          return $ AB (zip [0..] $ s)

membb :: Parser Bool
membb = do b <- string "True" <|> string "False"
           skipMany $ string ","
           return $ read b

--------------------------------------------------------

parseFunctions :: [String] -> Either ParseError [FunDef]
parseFunctions [] = Right []
parseFunctions (x:xs) | isLeft parsedfuncs = parsedfuncs
                      | isLeft parsedfunc = Left errorfunc
                      | otherwise = Right (fundef:fundefs)
                      where parsedfuncs = parseFunctions xs
                            parsedfunc = parse function "" x
                            (Left errorfunc) = parsedfunc
                            (Right fundefs) = parsedfuncs
                            (Right fundef) = parsedfunc


function :: Parser FunDef
function = do id' <- many(letter<|>digit)
              spaces
              (t,vs,exp') <- parens funchelp
              return (id', (t, vs, exp'))

funchelp :: Parser (Type, [VarDef], Exp)
funchelp = do t <- typeP
              spaces
              vd <- vardefs
              exp' <- parens expr
              return (t,vd,exp')

typeP :: Parser Type
typeP = tint <|> tbool <|> tarrint <|> tarrbool

tint,tbool,tarrint,tarrbool :: Parser Type
tint = do{_<-string "TInt";return TInt}
tbool = do{_<-string "TBool";return TBool}
tarrint = do{_<-string "TArrInt";return TArrInt}
tarrbool = do{_<-string "TArrBool";return TArrBool}

vardefs :: Parser [VarDef]
vardefs = sqparens $ many vardef

vardef :: Parser VarDef
vardef = defint <|> defbool <|> defarrint <|> defarrbool

defint,defbool,defarrint,defarrbool :: Parser VarDef
defint = do{_<-string "Int";spaces;id <- many1(letter<|>digit);skipMany $ string ",";spaces;return $ Int id}
defbool = do{_<-string "Bool";spaces;id <- many1(letter<|>digit);skipMany $ string ",";spaces;return $ Bool id}
defarrint = do{_<-string "ArrInt";spaces;id <- many1(letter<|>digit);skipMany $ string ",";spaces;return $ ArrInt id}
defarrbool = do{_<-string "ArrBool";spaces;id <- many1(letter<|>digit);skipMany $ string ",";spaces;return $ ArrBool id}

parseState :: String -> Either ParseError TypeSystem.State
parseState str = parse state "" str

state :: Parser TypeSystem.State
state = parens $ many actIdVar

actIdVar :: Parser Binding
actIdVar = do n <- lower
              m <- many(letter<|>digit)
              spaces
              v <- actVar
              skipMany $ string ","
              spaces
              return ((n:m),(Global,v))

actVar :: Parser Value
actVar = actInt <|> actBool <|> actArrInt <|> actArrBool

actInt, actBool, actArrInt, actArrBool :: Parser Value
actInt = do{n<-number;return $ I n}
actBool = do{b<-boolean;return $ B b}
actArrInt = do{ai<-sqparens arri;return ai}
actArrBool = do{ab<-sqparens arrb;return ab}