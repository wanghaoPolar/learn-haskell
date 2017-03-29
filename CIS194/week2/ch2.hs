module Main (main) where

-- algebrabic data type
data AlgDataType = Constr1 String Double
                 | Constr2 Bool
                 | Constr3 Int Integer Double
                 | Constr4

data SealingWax = SealingWax

data Person = Person String Integer [String]
    deriving (Show)

-- p@ to match the whole data
baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

-- nested pattern match
checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _)          = n ++ ", your favorite thing is lame."

-- recursive data type
data IntList = Empty | Cons Int IntList

data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

intListProd :: IntList -> Int
intListProd Empty      = 1
intListProd (Cons x l) = x * intListProd l

main :: IO ()
main = return ()
