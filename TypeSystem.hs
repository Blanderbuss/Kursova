module TypeSystem where

type Id = String
data Value = I Int | B Bool | AI [(Int,Int)] | AB [(Int,Bool)] 
           deriving (Eq, Show)
data Op = Add | Minus | Mul | Less | Equal | Index
          deriving (Eq, Show)
data Exp = Const Value | 
           Var Id | 
           OpApp Op Exp Exp |
           Cond Exp Exp Exp |
           FunApp Id [Exp] 
         deriving (Eq, Show)

type VarDef  =  (Id, Type)

type FunDef  =  (Id, (Type, [VarDef], Exp))

data Scope = Local | Global
           deriving (Eq, Show)
type Binding = (Id, (Scope, Type))
type State = [Binding]

data Statement = Assign Id Exp |
                 AssignA Id Exp Exp |
                 If Exp Block Block |
                 While Exp Block |
                 Call Id Id [Exp] |
                 Return Exp 
               deriving (Eq, Show)

type Block     = [Statement]
type ProcDef   = (Id, ([VarDef], Block))
type Program   = ([VarDef], [FunDef], [ProcDef])

data Type = TInt | TArrInt | TArrBool | TBool | TFun Type [Type] | TProc [Type] | TNil
               deriving (Eq, Show)

{-data TSimple = TInt | TArrInt | TArrBool | TBool 
               deriving (Eq, Show)-}


