module Helper where

import TypeSystem

{-sampleState :: State
sampleState  = [("x", (Local, I 5)), ("y", (Global, I 2)), ("a", (Global, listToVal [4,2,7])), ("b", (Local, B True))]-}

listToVal :: [Int] -> Value
listToVal xs  = AI (zip [0..] xs)

intToExp :: Int -> Exp
intToExp n
  = Const (I n)

add :: FunDef
add
  = ("add", 
     (TInt,
     [("a", TInt), ("b",TInt)],OpApp Add (Var "a") (Var "b")
     )
    )

fib :: FunDef
fib
  = ("fib", 
     (TInt,
     [("n", TInt)], Cond (OpApp Less (Var "n") (Const (I 3)))
                  (Const (I 1))
                  (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const (I 1))])
                             (FunApp "fib" [OpApp Minus (Var "n") (Const (I 2))]))
     )
    )


-- Масив
sampleArray :: Exp
sampleArray 
  = Const (listToVal [9,5,7,1])

-- Сума елементів масиву 0..n ...
sumA1 :: ProcDef
sumA1
  = ("sumA1",
     ([("a", TArrInt), ("n", TInt)], 
                  [Assign "s" (Const (I 0)),
                   Assign "i" (Const (I 0)),
                   Assign "limit" (OpApp Add (Var "n") (Const (I 1))),
                   While (OpApp Less (Var "i") (Var "limit"))
                         [Assign "s" (OpApp Add (Var "s") 
                                                (OpApp Index (Var "a") (Var "i"))),
                          Assign "i" (OpApp Add (Var "i") (Const (I 1)))
                         ],
                   Return (Var "s")]
     )
    )

-- Додавання двох чисел...
gAdd :: ProcDef
gAdd
  = ("gAdd", 
     ([("x", TInt), ("y", TInt)], 
      [
        Assign "gSum" (OpApp Add (Var "x") (Var "y"))
      ])
    )

-- Повна програма
pr1 :: Program
pr1 = (
      [("gSum", TInt)], 
      [add], 
      [
        ("gAdd", 
         ([("x", TInt), ("y", TInt)], 
          [
           Assign "gSum" (OpApp Add (Var "x") (Var "y"))
          ])
        ),
        ("main",([],
          [ 
           Call "" "gAdd" [Const $ I 5, Const $ I 7] 
          ])
        )
      ]
     )