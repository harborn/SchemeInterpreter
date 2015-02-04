

import Control.Monad
import Data.IORef

data Person = Person {
		firstName :: String
	   ,lastName :: String
	   ,age :: Int
	   ,height :: Float
	   ,phoneNumber :: String
	   ,flavor :: String } deriving (Show)

data Car = Car {
		company :: String
	   ,model :: String
	   ,year :: Int } deriving (Show)

f :: Int -> Int
f = (+3) . (*4)

f2 :: Int
f2 = 3

fun :: [a] -> Int -> [a]
fun x n = x >>= replicate (f n)


magic :: IORef (Maybe Int) -> IO ()
magic ref = do
    value <- readIORef ref

    case value of
        Just _ -> return ()
        Nothing -> writeIORef ref (Just 42)

main :: IO ()
main = do
    ref <- newIORef Nothing
    magic ref
    readIORef ref >>= print


func :: [(String, String)]
func = [("add", "+")
	   ,("minus", "-")
	   ,("multi", "*")
	   ,("divide", "/")
	   ,("mod", "%")
	   ]

persons :: [Person]	   
persons = [
			Person { firstName = "Just", lastName = "Wu", age = 20, height = 1.73, phoneNumber = "12346578901", flavor = "programming" }
		   ,Person { firstName = "Maybe", lastName = "Wu", age = 21, height = 1.74, phoneNumber = "12346578901", flavor = "programming" }
		   ,Person { firstName = "Either", lastName = "Wu", age = 22, height = 1.75, phoneNumber = "12346578901", flavor = "programming" }
		   ,Person { firstName = "Nothing", lastName = "Wu", age = 23, height = 1.76, phoneNumber = "12346578901", flavor = "programming" }
		   ,Person { firstName = "If", lastName = "Wu", age = 24, height = 1.77, phoneNumber = "12346578901", flavor = "programming" }
		   ,Person { firstName = "Else", lastName = "Wu", age = 25, height = 1.78, phoneNumber = "12346578901", flavor = "programming" }
		   ,Person { firstName = "Map", lastName = "Wu", age = 26, height = 1.79, phoneNumber = "12346578901", flavor = "programming" }
		   ,Person { firstName = "Filter", lastName = "Wu", age = 27, height = 1.80, phoneNumber = "12346578901", flavor = "programming" }
		  ]



car1 = Car { company = "Ford", model = "Mustang", year = 1967}

person1 = Person {firstName = "Youxi", lastName = "Wu", age = 28, height = 1.81, phoneNumber = "98765432109", flavor = "love"}

type Env = IORef [(String, IORef Person)]

nullEnv :: IO Env
nullEnv = newIORef []

personsBinding :: IO Env
personsBinding = mapM tuple persons >>= newIORef
    where tuple x = do
            ref <- (newIORef x)
            return (firstName x, ref)

getVar :: String -> IO Person
getVar name = do
	env <- personsBinding
	ps <- (readIORef env)
	case (lookup name ps) of
		Just x -> readIORef x
		_ -> return (persons!!0)

setVar :: String -> Person -> IO ()
setVar name person = do
	env <- personsBinding
	ps <- (readIORef env)
	case (lookup name ps) of 
		Just x -> do print "GOT YOU"; writeIORef x person
		_ -> return ()
        

newCounter :: IO (IO Int)
newCounter = do {
  	counter <- newIORef (0::Int);
  	return $ do {
    	i <- readIORef counter;
    	writeIORef counter (i+1);
    	return i
 		}
  	}

test1 :: IO ()
test1 = do {
	c1 <- newCounter;
	x1 <- c1;
	x2 <- c1;
	x3 <- c1;
	c2 <- newCounter;
	y1 <- c2;
	print x1;
	print x2;
	print x3;
	print y1;
	}


makeTest :: IO [IORef Int]
makeTest = sequence [newIORef 0, newIORef 1, newIORef 2]

test2 = do
  	test <- makeTest
  	--test <- test ++ [newIORef 3]
  	--modifyIORef makeTest (++[newIORef 3])
  	mapM_ (\x -> (readIORef x) >>= print) test
  	--readIORef (test !! 1) >>= print
  	mapM_ (\x -> (modifyIORef x (+1))) test
  	--modifyIORef (test !! 1) (+1) -- Doesn't copy list
  	--readIORef (test !! 1) >>= print
  	mapM_ (\x -> (modifyIORef x (*2))) test
  	mapM_ (\x -> (modifyIORef x ((-)1))) test
  	mapM_ (\x -> (readIORef x) >>= print) test	
  	mapM_ (\x -> (writeIORef x 22)) test
  	mapM_ (\x -> (readIORef x) >>= print) test	

test3 = do
	test <- makeTest
	mapM_ (\x -> (readIORef x) >>= print) test	
