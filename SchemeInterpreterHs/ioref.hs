

import Data.IORef

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