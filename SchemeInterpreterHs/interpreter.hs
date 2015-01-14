module Main where
import Data.String.Utils
import Data.List.Utils

data LispVal = Atom String
             | Number Integer
             | String String
             | Bool Bool
             | List [LispVal]
               deriving (Eq, Ord, Show)

lv1 = List [Atom "define", Atom "x", Number 10]
lv2 = List [Atom "define", Atom "y"]
lv3 = List [Atom "define", List [Atom "add", Atom "x", Atom "y"], List [Atom "+", Atom "x", Atom "y"]]

data T = ConsInt Int
       | ConsString String
       | ConsChar Char
         deriving (Show)

tt = [ConsInt 42, ConsChar 'a', ConsString "foo"]

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Tree = Empty
          | Leaf Int
          | Node Tree Tree

depth :: Tree -> Int
depth Empty = 0
depth (Leaf n) = 1
depth (Node l r) = 1 + max (depth l) (depth r)

--data List a = Nil | Cons a (List a) deriving (Show)
data Expression = Numb Int
                | Add Expression Expression
                | Minus Expression Expression
                | Mult Expression Expression
                | Divide Expression Expression
                  deriving (Show)


e1 = "(define x 10)"
e2 = "(define twice (lambda (x) (* 2 x)))"
e3 = "(define compose (lambda (f g) (lambda (x) (f (g x)))))"
e4 = "(define repeat (lambda (f) (compose f f)))"
e5 = "(define (add x y) (+ x y))"

tokenize :: String -> [String]
tokenize exp = filter (\x -> (not $ null x)) (split " " (replace ")" " ) " (replace "(" " ( " exp)))

{-
readtokens :: [String] -> LispVal
readtokens [] = List []
readtokens (x:xs) =
    if x == "(" then readtokens xs
    else if x == ")" then readtokens xs
    else List ([Atom x] ++ (readtokens xs))
-}


t1 = tokenize e1
t2 = tokenize e2
t3 = tokenize e3
t4 = tokenize e4
t5 = tokenize e5




--ll = ["define", ["add", "x", "y"], ["+", "x", "y"]]
ll = ["define", "add", "x", "y", "+", "x", "y"]

main = do
	print t1
	print t2
	print t3
	print t4

