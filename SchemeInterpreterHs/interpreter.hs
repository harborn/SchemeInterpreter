module Main where
import Data.String.Utils
import Data.List.Utils

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

e1 = "(define x 10)"
e2 = "(define twice (lambda (x) (* 2 x)))"
e3 = "(define compose (lambda (f g) (lambda (x) (f (g x)))))"
e4 = "(define repeat (lambda (f) (compose f f)))"
e5 = "(define (add x y) (+ x y))"

tokenize :: String -> [String]
tokenize exp = filter (\x -> (not $ null x)) (split " " (replace ")" " ) " (replace "(" " ( " exp)))

readtokens :: [String] -> [String]
readtokens [] = []
--readtokens [x:xs] = if x == "("

t1 = tokenize e1
t2 = tokenize e2
t3 = tokenize e3
t4 = tokenize e4




--ll = ["define", ["add", "x", "y"], ["+", "x", "y"]]
ll = ["define", "add", "x", "y", "+", "x", "y"]

main = do
	print t1
	print t2
	print t3
	print t4

