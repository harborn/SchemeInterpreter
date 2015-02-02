
--module Main where
import Data.List
import Data.String.Utils
import Data.List.Utils
import Data.Word
import Control.Monad
import Control.Monad.Error
import Data.IORef
import System.IO
import System.Environment
import Data.Either

--import Text.Parsec.Prim
--import Text.Parsec.Token (commaSep, integer, decimal)
--import Text.ParserCombinators.Parsec

{-
simple :: Parser Char
simple = letter

run :: Show a => Parser a -> String -> IO ()
run p input =
            case (parse p "" input) of
              Left err -> do { putStr "parse error at "
                             ; print err
                             }
              Right x -> print x


openClose :: Parser Char
openClose = do {
				char '('
			   ;char ')'
			   }

parens :: Parser ()
parens = do {
			  char '('
			; parens
			; char ')'
			; parens
			}
		<|> return ()

--test = case (parse (commaSep integer) "" "11, 2, 43") of
--		Left err -> print err
--		Right xs -> print (sum xs)

--numbers = commaSep integer
-}


--data IP = IP Word8 Word8 Word8 Word8 deriving Show

--parseIP :: Parser IP
--parseIP = do
--  d1 <- decimal
--  char '.'
--  d2 <- decimal
--  char '.'
--  d3 <- decimal
--  char '.'
--  d4 <- decimal
--  return $ IP d1 d2 d3 d4

--test = parse parseIP "" "192.168.1.1"

--main :: IO ()
--main = print $ parseOnly parseIP "131.45.68.123"


data LispVal = Atom String
             | Number Integer
             | Float Double
             | String String
             | Bool Bool
             | List [LispVal]
               deriving (Eq, Ord, Show)

type Env = IORef [(String, IORef LispVal)]

-- "(define x 10)"
-- "(define (add x y) (+ x y)"
-- ["(","define","(","add","x","y",")","(","+","x","y",")",")"]
-- [Atom "define",List [Atom "add",Atom "x",Atom "y"],List [Atom "+",Atom "x",Atom "y"]]"

tokenize :: String -> [String]
tokenize exp = words $ replace ")" " ) " $ replace "(" " ( " exp

readExpr :: String -> LispVal
readExpr = head . readToken . tokenize

readToken :: [String] -> [LispVal]
readToken [] = []
readToken (x:xs) =
	if x == "(" then
		let
			ts1 = takeWhile (/="(") xs
			ts2 = takeWhile (/=")") xs
		in if length ts1 > length ts2 then
			[List (readToken ts2)]++
			(readToken (drop (length ts2) xs))
		   else [List (readToken xs)]
	else if x == ")" then readToken xs
	else let val =
			if (isInteger x) then (Number (read x::Integer))
			else if (isDouble x) then (Float (read x::Double))
			 	 else if (isBool x) then (Bool (read x::Bool))
			 	  	  else (Atom x)
		 in [val]++readToken xs where



printLispVal :: LispVal -> String
printLispVal x = case x of
	Atom a 	 -> a ++ " "
	Number i -> show(i) ++ " "
	String s -> s ++ " "
	Bool b 	 -> show(b) ++ " "
	List l   -> "( "++(concat $ map printLispVal l)++") "


isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  	[(_, "")] -> True
  	_         -> False

isDouble :: String -> Bool
isDouble s = case reads s :: [(Double, String)] of
  	[(_, "")] -> True
  	_         -> False

isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s

isBool :: String -> Bool
isBool "True" = True
isBool "False" = True
isBool _ = False


--parseLispVal :: String -> LispVal
--parseLispVal x = case x of

numericOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericOp op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (List [n]) = unpackNum n


main :: IO ()
main = do
    ref <- newIORef (0 :: Int)
    modifyIORef ref (+1)
    readIORef ref >>= print


nullEnv :: IO Env
nullEnv = newIORef []


--env <- primitiveBindings >>= flip eval "(+ 1 2 3 4 5 6)" >>= putStrLn
--env <- primitiveBindings >>= flip evalString "(+ 1 2 3 4 5 6)" >>= putStrLn

{-
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Char _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval
-}

eleft :: Either a b -> a
eleft e = (lefts [e])!!0

eright :: Either a b -> b
eright e = (rights [e])!!0

getEitherVal :: Either a b -> String
getEitherVal e = case e of
    Left a -> "Left Value "
    Right b -> "Right Value"

packVal :: a -> IO a
packVal a = return a

unpackVal :: IO (Either a b) -> String
unpackVal v = v >>= getEitherVal