
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

data LispVal = Atom String
             | Number Integer
             | Float Double
             | String String
             | Bool Bool
             | Error String
             | List [LispVal]
             | PrimitiveFunc ([LispVal] -> LispVal)
             | Func { params :: [String]
             		, vararg :: (Maybe String)
             		, body :: [LispVal]
             		, closure :: Env }
               
type Env = IORef [(String, IORef LispVal)]

tokenize :: String -> [String]
tokenize exp = words $ replace ")" " ) " $ replace "(" " ( " exp

getExpr :: [String] -> Int -> [String]
getExpr [] _ = []
getExpr (x:xs) cnt = case x of 
    "(" -> x:(getExpr xs (cnt+1))
    ")" -> if cnt == 1 then [x] else x:(getExpr xs (cnt-1))
    _   -> x:(getExpr xs cnt)

readExpr :: String -> LispVal
readExpr = head . readToken . tokenize

readToken :: [String] -> [LispVal]
readToken [] = []
readToken (x:xs) =
	if x == "(" then
		let
            se1 = x:(getExpr xs 1)
            se2 = drop (length se1 - 1) xs
		in [List (readToken (init $ tail se1))] ++ readToken se2
	else let val =
			if (isInteger x) then (Number (read x::Integer))
			else if (isDouble x) then (Float (read x::Double))
			 	 else if (isBool x) then (Bool (read x::Bool))
			 	  	  else (Atom x)
		 in [val]++readToken xs where

showVal :: LispVal -> String
showVal x = case x of
    (Atom a) -> a ++ " "
    (Number i) -> show(i) ++ " "
    (String s) -> s ++ " "
    (Bool b) -> if b then "#t " else "#f "
    (Error e) -> "[Error] " ++ e
    (Func {params=args, vararg=varargs, body=body, closure=env}) -> "this is a function"
    (PrimitiveFunc p) -> "this is a primitive function"
    (List l) -> "( "++(concat $ map showVal l) ++ ") "

showVal2 :: LispVal -> IO ()
showVal2 = putStrLn . showVal

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


unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (List [n]) = unpackNum n

unpackStr :: LispVal -> String
unpackStr (String s) = s
unpackStr (List [n]) = unpackStr n

unpackBool :: LispVal -> Bool
unpackBool (Bool b) = b
unpackBool (List [b]) = unpackBool b

eval :: Env -> LispVal -> IO LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", expr, branch1, branch2]) = do
    res <- eval env expr
    case res of 
        Bool True -> eval env branch1
        Bool False -> eval env branch2
        _ -> return (Error "in if expression")

eval env (List (Atom "cond" : [])) = return (Error "no clause in cond expression")
eval env (List (Atom "cond" : clauses)) = evalCond env clauses
eval env (List (Atom "case" : [])) = return (Error "no key and clause in case expression")
eval env (List (Atom "case" : key : clauses)) = evalCase env key clauses
        
{-
eval env (List [Atom "define", Atom var, form]) = 
    eval env form >>= defineVar env var

eval env (List (Atom "define": List (Atom var : params): body)) =

-}


eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

evalCond :: Env -> [LispVal] -> IO LispVal
evalCond env ((List [cond, expression]) : clauses) = do  
    case cond of 
        Atom "else" -> eval env expression
        _ -> do res <- eval env cond
                case res of 
                    Bool True -> eval env expression
                    Bool False -> evalCond env clauses
                    _ -> return (Error "in cond expression")
evalCond env _ = return (Error "in cond expression")

evalCase :: Env -> LispVal -> [LispVal] -> IO LispVal
evalCase env key ((List ((List vals) : expression)) : clauses) = do
    k <- eval env key
    -- key must be an expression. each <case clause> must have one of the following forms:
    -- ((<datum1> ...) <expression1> <expression2> ...)
    -- (else <expression1> <expression2> ...)
    case (any compVal $ map (\x -> equal [x, k]) vals) of
        True -> last $ fmap (eval env) expression
        False -> evalCase env key clauses
    where 
        compVal (Bool True) = True
        compVal _ = False
evalCase env _ [List (Atom "else" : expression)] = last $ fmap (eval env) expression
evalCase _ _ [] = return (Error "in case expression")

getEitherVal :: Either a b -> IO String
getEitherVal e = case e of
    Left a -> packVal "Error happened"
    Right b -> packVal "parse successfully"

packVal :: a -> IO a
packVal a = return a


primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", add)
			 ,("-", minus)
			 ,("*", multi)
             ,("/", divide)
             ,("=", equal)
             ,("<", less)
             ,(">", great)
             ,("/=", notEqual)
             ,(">=", greatEqual)
             ,("<=", lessEqual)
             ,("&&", and2)
             ,("||", or2)
             
             ,("car", car)
             ,("cdr", cdr)
             ,("cons", cons)
             ,("list", list)
             ,("length", len)
			 ]

add :: [LispVal] -> LispVal
add params = Number $ foldl1 (+) $ map unpackNum params

minus :: [LispVal] -> LispVal
minus params = Number $ foldl1 (-) $ map unpackNum params

multi :: [LispVal] -> LispVal
multi params = Number $ foldl1 (*) $ map unpackNum params

divide :: [LispVal] -> LispVal
divide params = Number $ foldl1 (div) $ map unpackNum params

equal :: [LispVal] -> LispVal
equal [Number a, Number b] = Bool (a == b)
equal [Atom a, Atom b] = Bool (a == b)
equal [String a, String b] = Bool (a == b)
equal [Bool a, Bool b] = Bool (a == b)
equal [List a, List b]
    | length a /= length b = Bool False
    | otherwise = Bool $ all compPair $ zip a b
      where compPair (a, b) = case (equal [a, b]) of
                                Bool True -> True
                                _ -> False
equal _ = (Error "wrong paramters for =")

notEqual :: [LispVal] -> LispVal
notEqual vals = case equal vals of 
    Bool True -> Bool False
    Bool False -> Bool True
    _ -> (Error "wrong paramters for not")

great :: [LispVal] -> LispVal
great [Number a, Number b] = Bool (a > b)
great _ = (Error "wrong paramters for >")

less :: [LispVal] -> LispVal
less [Number a, Number b] = Bool (a < b)
less _ = (Error "wrong paramters for <")

greatEqual :: [LispVal] -> LispVal
greatEqual [Number a, Number b] = Bool (a >= b)
greatEqual _ = (Error "wrong paramters for >=")

lessEqual :: [LispVal] -> LispVal
lessEqual [Number a, Number b] = Bool (a <= b)
lessEqual _ = (Error "wrong paramters for <=")

and2 :: [LispVal] -> LispVal
and2 [Bool b1, Bool b2] = Bool (b1 == b2)
and2 _ = (Error "wrong paramters for &&")

or2 :: [LispVal] -> LispVal
or2 [Bool b1, Bool b2] = Bool (b1 || b2)
or2 _ = (Error "wrong paramters for ||")

car :: [LispVal] -> LispVal
car [List (x:xs)] = x
car _ = (Error "wrong paramters for car")

cdr :: [LispVal] -> LispVal
cdr [List (x:xs)] = List xs
cdr _ = (Error "wrong paramters for cdr")

cons :: [LispVal] -> LispVal
cons = List

list :: [LispVal] -> LispVal
list = List

len :: [LispVal] -> LispVal
len [List x] = Number (toInteger (length x))
len _ = (Error "wrong paramters for length")

getVar :: Env -> String -> IO LispVal
getVar env var = do
    func <- readIORef env
    case (lookup var func) of
        Just v -> readIORef v
        _ -> return (List [])

--setVar :: Env -> String -> LispVal -> IO LispVal
--setVar env var val = do
--    vars <- readIORef env
    

isDefined :: Env -> String -> IO Bool
isDefined env var = do 
    val <- getVar env var
    case val of 
        List [] -> return False
        _ -> return True

--defineVar :: Env -> String -> LispVal -> LispVal
--defineVar env var val = do
--    defined <- isDefined env var
--    if defined 
--        then

apply :: LispVal -> [LispVal] -> IO LispVal
apply (PrimitiveFunc func) args = return (func args)
apply badVal args = return (Error "expression error")

evalString :: Env -> String -> IO ()
evalString env expr = (eval env $ readExpr expr) >>= showVal2

evalExpr :: String -> IO ()
evalExpr expr = primitiveBindings >>= (flip evalString expr)

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do
            ref <- newIORef value
            return (var, ref)

primitiveBindings :: IO Env
primitiveBindings = (newIORef []) >>= (flip bindVars $ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = (var, constructor func)

--makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
                    
--globalFunctions :: IO Env
--globalFunctions = 

