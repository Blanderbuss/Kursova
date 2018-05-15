{-# OPTIONS_GHC -Wall #-}
module Eval where

import TypeSystem

import Data.Maybe

getValue :: Id -> State -> Value
getValue x t = snd $ lookUp x t 

getLocals :: State -> State
getLocals s = filter ((==Local).fst.snd) s

getGlobals :: State -> State
getGlobals s = filter ((==Global).fst.snd) s

assignArray :: Value -> Value -> Value -> Value
assignArray (AI arr) (I ind) (I val) = AI $ (ind,val):(filter ((/=ind).fst) arr)
assignArray _ _ _ = undefined

-- Задача 4 -----------------------------------------
updateVar :: (Id, Value) -> State -> State
updateVar (i, val) st | ch == Nothing = (i, (Local, val)):st
                      | otherwise = changeBind (i,(sc,val)) st
                       where ch = lookup i st
                             Just (sc,_) = ch

changeBind :: Binding -> State -> State
changeBind _ [] = []
changeBind (i,(sc,val)) ((i0,(sc0,val0)):xs) | i == i0 = (i,(sc,val)):xs
                                             | otherwise = (i0,(sc0,val0)):(changeBind (i,(sc,val)) xs)

applyOp :: Op -> Value -> Value -> Value
applyOp Add (I i1) (I i2) = I $ i1+i2
applyOp Minus (I i1) (I i2) = I $ i1-i2
applyOp Mul (I i1) (I i2) = I $ i1*i2
applyOp Less (I i1) (I i2) = B $ i1<i2
applyOp Equal (I i1) (I i2) = B $ i1==i2
applyOp Equal (B b1) (B b2) = B $ b1==b2
applyOp Equal (AI arr1) (AI arr2) = B $ arr1 == arr2
applyOp Equal (AB arr1) (AB arr2) = B $ arr1 == arr2
applyOp Index (AI arr) (I i) = if lookup i arr == Nothing then (B False) else (I res)
                            where Just res = lookup i arr
applyOp Index (AB arr) (I i) = if lookup i arr == Nothing then (B False) else (B res)
                            where Just res = lookup i arr
applyOp _ _ _ = undefined

bindArgs :: [Id] -> [Value] -> State
bindArgs [] _ = []
bindArgs _ [] = []
bindArgs (i:is) (v:vs) = (i,(Local,v)):(bindArgs is vs)

eval :: Exp -> [FunDef] -> State -> Value
eval (Const c) _ _ = c
eval (Var i) _ st = getValue i st
eval (Cond c t f) dfx st | eval c dfx st == B True = eval t dfx st
                         | eval c dfx st == B False = eval f dfx st
                         | otherwise = error ("expression " ++ (show c) ++ " is not predicate")
eval (OpApp op e1 e2) dfx st = applyOp op (eval e1 dfx st) (eval e2 dfx st)
eval (FunApp f es) dfx st = eval ef dfx $ binArgs as vs ++ st
                          where (_,(_,as,ef)) = head $ filter ((==f).fst) dfx
                                vs = evalArgs es dfx st


evalArgs :: [Exp] -> [FunDef] -> State -> [Value]
evalArgs [] _ _ = []
evalArgs (e:es) dfx st = (eval e dfx st):(evalArgs es dfx st)

binArgs :: [VarDef] -> [Value] -> State
binArgs [] _ = []
binArgs _ [] = []
binArgs ((ArrInt i):is) ((AI v):vs) = (i,(Local,(AI v))):(binArgs is vs)
binArgs ((ArrBool i):is) ((AB v):vs) = (i,(Local,(AB v))):(binArgs is vs)
binArgs ((Bool i):is) ((B v):vs) = (i,(Local,(B v))):(binArgs is vs)
binArgs ((Int i):is) ((I v):vs) = (i,(Local,(I v))):(binArgs is vs)
binArgs _ _ = undefined

lookUp :: (Eq a, Show a) => a -> [(a, b)] -> b
lookUp x t = fromMaybe (error ("\nСпроба знайти  " ++ show x ++ 
                      " в таблиці котра має лише звязування: " ++ 
                      show (map fst t))) 
              (lookup x t)