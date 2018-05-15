{-# OPTIONS_GHC -Wall #-}
module Checker where

import Data.Maybe
import Data.Either
import TypeSystem


-- Type check

data Error 
  = Mismatch Type Type String
  | NotInScope Id
  | NoMain
               deriving (Eq)

instance Show Error where
  show (Mismatch a b s) = show ("Expected: " ++ (show a) ++". Actual: "++ (show b) ++ ".\n" ++ s) 
  show (NotInScope a) = show ("Variable not in scope: " ++ a)
  show (NoMain) = show ("No main procedure detected in program")

isInt :: Type -> Bool
isInt TInt = True
isInt _ = False


isBool :: Type -> Bool
isBool TBool = True
isBool _ = False

typeOfValue :: Value -> Type
typeOfValue (I _) = TInt
typeOfValue (B _) = TBool
typeOfValue (AI _) = TArrInt
typeOfValue (AB _) = TArrBool

typeOfVarDef :: VarDef -> Type
typeOfVarDef (_, t) = t

check :: Exp -> [FunDef] -> State -> Either Error Type
 
check (Const (I _)) _ _ = Right TInt
check (Const (B _)) _ _ = Right TBool
check (Const (AI _)) _ _ = Right TArrInt
check (Const (AB _)) _ _ = Right TArrBool

check (Var v) _ st = let maybeValue = lookup v st 
                     in case maybeValue of
                      Nothing -> Left $ NotInScope v
                      Just (_, t) -> Right t

check (Cond cond true false) dfx st | isLeft tCond = tCond
                                    | isLeft tTrue = tTrue
                                    | isLeft tFalse = tFalse
                                    | not $ isBool (fromRight TInt tCond) = Left $ Mismatch TBool (fromRight TInt tCond) (show cond)
                                    | tTrue /= tFalse = Left $ Mismatch (fromRight TInt tTrue) (fromRight TInt tFalse) (show (Cond cond true false))
                                    | otherwise = tTrue
                                     where tCond = check cond dfx st
                                           tTrue = check true dfx st
                                           tFalse = check false dfx st

check (OpApp op exp1 exp2) dfx st | isLeft chExp1 = chExp1
                                  | isLeft chExp2 = chExp2
                                  | opRequiresInts && (typeExp1 /= TInt) = Left $ Mismatch TInt typeExp1 (show (OpApp op exp1 exp2))
                                  | opRequiresInts && (typeExp2 /= TInt) = Left $ Mismatch TInt typeExp2 (show (OpApp op exp1 exp2))
                                  | opRequiresInts && (op == Less) = Right TBool
                                  | opRequiresInts = Right TInt
                                  | (op == Equal) && (typeExp1 /= typeExp2) = Left $ Mismatch typeExp1 typeExp2 (show (OpApp op exp1 exp2))
                                  | (op == Equal) = Right TBool
                                  | (op == Index) && (typeExp1 /= TArrInt) = Left $ Mismatch TArrInt typeExp1 (show (OpApp op exp1 exp2))
                                  | (op == Index) && (typeExp1 /= TArrBool) = Left $ Mismatch TArrBool typeExp1 (show (OpApp op exp1 exp2))
                                  | (op == Index) && (typeExp2 /= TInt) = Left $ Mismatch TInt typeExp2 (show (OpApp op exp1 exp2))
                                  | (op == Index) && (typeExp1 = TArrInt) = Right TInt
                                  | (op == Index) && (typeExp1 = TArrBool) = Right TBool
                                  | otherwise = Right TNil
                                  where opRequiresInts = (op == Add) || (op == Minus) || (op == Mul) || (op == Less)
                                        chExp1 = check exp1 dfx st
                                        chExp2 = check exp2 dfx st
                                        typeExp1 = fromRight TNil chExp1
                                        typeExp2 = fromRight TNil chExp2

check (FunApp fid args) dfx st | null funcList = Left $ NotInScope fid
                               | not $ null errorArgs = Left $ head errorArgs
                               | not $ cba == Nothing = Left $ errCba
                               | otherwise = Right $ ftype
                               where funcList = filter ((==fid).fst) dfx 
                                     (_,(ftype,decArgs,_)) = head funcList
                                     typeArgs = checkArgs args dfx st
                                     errorArgs = lefts typeArgs
                                     goodArgs = rights typeArgs
                                     cba =  checkBindingArgs decArgs goodArgs
                                     (Just errCba) = cba

checkBindingArgs :: [VarDef] -> [Type] -> Maybe Error
checkBindingArgs [] [] = Nothing
checkBindingArgs [] (x:_) = Just $ Mismatch TNil x "Function actual arguments not corresponding to formal"
checkBindingArgs (x:_) [] = Just $ Mismatch (typeOfVarDef x) TNil "Function actual arguments not corresponding to formal"
checkBindingArgs ((_, TArrInt):is) (TArrInt:vs) = checkBindingArgs is vs
checkBindingArgs ((_, TArrBool):is) (TArrBool:vs) = checkBindingArgs is vs
checkBindingArgs ((_, TBool):is) (TBool:vs) = checkBindingArgs is vs
checkBindingArgs ((_, TInt):is) (TInt:vs) = checkBindingArgs is vs
checkBindingArgs (x:_) (y:_) = Just $ Mismatch (typeOfVarDef x) y "Function actual arguments not corresponding to formal"


checkArgs :: [Exp] -> [FunDef] -> State -> [Either Error Type]
checkArgs [] _ _ = []
checkArgs (e:es) dfx st = (check e dfx st):(checkArgs es dfx st)

checkFunctions :: [FunDef] -> [FunDef] -> State -> Either Error Type
checkFunctions (cdf:cdfx) dfx st | isLeft expCheck = expCheck
                                 | expType /= t = Left $ Mismatch t expType (show ex)
                                 | otherwise = checkFunctions cdfx dfx st
                                 where expCheck = check ex dfx (s++st)
                                       (_, (t, vardef, ex)) = cdf
                                       (Right expType) = expCheck
                                       s = bindTempArgs vardef
checkFunctions [] _ _ = Right TNil

bindTempArgs :: [VarDef]-> State
bindTempArgs [] = []
bindTempArgs ((i, t):vs) = (i,(Local,t)):(bindTempArgs vs)

checkStatement :: Statement -> [FunDef] -> [ProcDef] -> State -> Either Error Type
checkStatement (Assign varId ex) dfx _ st | isLeft exCheck = exCheck
                                          | maybeVar == Nothing = Left $ NotInScope varId
                                          | typeVar /= typeExp = Left $ Mismatch typeVar typeExp (show (Assign varId ex))
                                          | otherwise = Right TNil
                                          where exCheck = check ex dfx st
                                                (Right typeExp) = exCheck
                                                maybeVar = lookup varId st
                                                (Just (_,typeVar)) = maybeVar

checkStatement (AssignA arrId exInd ex) dfx _ st | isLeft exCheck = exCheck
                                                 | isLeft exIndCheck = exIndCheck
                                                 | maybeArr == Nothing = Left $ NotInScope arrId
                                                 | typeInd /= TInt = Left $ Mismatch TInt typeInd (show exInd)
                                                 | typeArr /= TArrInt || typeArr /= TArrBool = Left $ Mismatch TArrInt typeArr (arrId ++ " NotAnArray")
                                                 | typeArr == TArrInt && typeExp /= TInt = Left $ Mismatch TInt typeExp (show (AssignA arrId exInd ex))
                                                 | typeArr == TArrBool && typeExp /= TBool = Left $ Mismatch TBool typeExp (show (AssignA arrId exInd ex))
                                                 | otherwise = Right TNil
                                                 where exCheck = check ex dfx st
                                                       exIndCheck = check exInd dfx st
                                                       (Right typeExp) = exCheck
                                                       (Right typeInd) = exIndCheck
                                                       maybeArr = lookup arrId st
                                                       (Just (_,typeArr)) = maybeArr

checkStatement (If condExp blockTrue blockFalse) dfx dpx st | isLeft condCheck = condCheck
                                                            | isLeft trueCheck = trueCheck
                                                            | isLeft falseCheck = falseCheck
                                                            | typeCond /= TBool = Left $ Mismatch TBool typeCond (show condExp)
                                                            | typeTrue /= typeFalse = Left $ Mismatch typeTrue typeFalse (show ((If condExp blockTrue blockFalse)))
                                                            | otherwise = Right TNil
                                                            where condCheck = check condExp dfx st
                                                                  trueCheck = checkBlock blockTrue dfx dpx st
                                                                  falseCheck = checkBlock blockFalse dfx dpx st
                                                                  (Right typeCond) = condCheck
                                                                  (Right typeTrue) = trueCheck
                                                                  (Right typeFalse) = falseCheck

checkStatement (While condExp block) dfx dpx st | isLeft condCheck = condCheck
                                                | isLeft blockCheck = blockCheck
                                                | typeCond /= TBool = Left $ Mismatch TBool typeCond (show condExp)
                                                | otherwise = Right TNil
                                                where condCheck = check condExp dfx st
                                                      blockCheck = checkBlock block dfx dpx st
                                                      (Right typeCond) = condCheck

checkStatement (Call "" idProc exArgs) dfx dpx st | maybeProc == Nothing = Left $ NotInScope idProc
                                                  | not $ null errFactArgs = Left $ head errFactArgs
                                                  | cba /= Nothing = Left errCba
                                                  | otherwise = Right TNil
                                                  where maybeProc = lookup idProc dpx
                                                        (Just (formalArgs, _)) = maybeProc
                                                        checkExpArgs = checkArgs exArgs dfx st
                                                        errFactArgs = lefts checkExpArgs
                                                        typesFactArgs = rights checkExpArgs
                                                        cba = checkBindingArgs formalArgs typesFactArgs
                                                        (Just errCba) = cba

--TODO not idFun, but idProc
checkStatement (Call idVar idFun exArgs) dfx _ st | maybeVar == Nothing = Left $ NotInScope idVar
                                                  | isLeft checkFunApp = checkFunApp
                                                  | typeVar /= funType = Left $ Mismatch typeVar funType (show (Call idVar idFun exArgs))
                                                  | otherwise = Right TNil
                                                  where maybeVar = lookup idVar st
                                                        checkFunApp = check (FunApp idFun exArgs) dfx st
                                                        (Right funType) = checkFunApp
                                                        (Just (_,typeVar)) = maybeVar

checkStatement (Return ex) dfx _ st | isLeft checkEx = checkEx
                                    | maybeRes == Nothing = Left $ NotInScope "$res"
                                    | typeRes /= typeEx = Left $ Mismatch typeRes typeEx (show ex)
                                    | otherwise = Right TNil
                                    where checkEx = check ex dfx st
                                          (Right typeEx) = checkEx
                                          maybeRes = lookup "$res" st
                                          (Just (_, typeRes)) = maybeRes

checkBlock :: [Statement] -> [FunDef] -> [ProcDef] -> State -> Either Error Type
checkBlock [] _ _ _ = Right TNil
checkBlock (stm:stx) dfx dpx st | isLeft checkSt = checkSt
                                | isLeft checkStx = checkStx
                                | otherwise = Right TNil
                                where checkSt = checkStatement stm dfx dpx st
                                      checkStx = checkBlock stx dfx dpx st

checkProcedures :: [ProcDef] -> [FunDef] -> [ProcDef] -> State -> Either Error Type
checkProcedures (cdp:cdpx) dfx dpx st | isLeft blockCheck = blockCheck
                                      | otherwise = checkProcedures cdpx dfx dpx st
                                      where blockCheck = checkBlock block dfx dpx (s++st)
                                            (_, (tvardef, block)) = cdp
                                            s = bindTempArgs tvardef
checkProcedures [] _ _ _ = Right TNil

checkProgram :: Program -> Maybe Error
checkProgram (dvx, dfx, dpx) | isLeft checkFuns = Just errFuns
                             | isLeft checkProcs = Just errProcs
                             | maybeMain == Nothing = Just NoMain
                             | otherwise = Nothing
                             where state = bindGlobalArgs dvx
                                   checkFuns = checkFunctions dfx dfx state
                                   checkProcs = checkProcedures dpx dfx dpx state
                                   (Left errFuns) = checkFuns
                                   (Left errProcs) = checkProcs
                                   maybeMain = lookup "main" dpx

bindGlobalArgs :: [VarDef] -> State
bindGlobalArgs [] = []
bindGlobalArgs ((i, t):vs) = (i,(Global,t)):(bindGlobalArgs vs)