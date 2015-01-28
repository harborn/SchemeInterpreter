
import Data.List (group)
import System.Random


-- Problem. 1
myLast = last

-- Problem. 2
myButLast = last . init

-- Problem. 3
elementAt :: [a] -> Int -> a
--elementAt (x:_) 1 = x
--elementAt (_:xs) k = elementAt xs (k-1)
--elementAt _ _ = errro "Index out of bounds"
elementAt xs k = xs!!(k-1)

-- Problem. 4
myLength :: [a] -> Int
--myLength = length
myLength [] = 0
myLength (x:xs) = 1+myLength xs

-- Problem. 5
myReverse :: [a] -> [a]
myReverse = reverse

-- Problem. 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == (reverse x)
--isPalindrome [] = True
--isPalindrome [_] = True
--isPalindrome x = if head x == last x then isPalindrome (init(tail(x))) else False

-- Problem. 7
data NestedList a = Elem a | List [NestedList a] deriving Show
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x)++(flatten (List xs))

-- Problem. 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x:compress(compressH xs x)

compressH :: Eq a => [a] -> a -> [a]
compressH [] _ = []
compressH (x:xs) n = if x == n then compressH xs n else x:xs

-- Problem. 9
pack :: Eq a => [a] -> [[a]]
--pack = group
pack [] = []
pack (x:xs) = (x:takeWhile (== x) xs) : pack (dropWhile (== x) xs)


-- Problem. 10
encode :: Eq a => [a] -> [(Int,a)]
-- encode [] = []
-- encode x = encodeH(pack(x))

-- encodeH :: Eq a => [[a]] -> [(Int,a)]
-- encodeH [] = []
-- encodeH (x:xs) = (length x, head x):encodeH xs

encode x = map (\x -> (length x, head x)) $ group x


-- Problem. 11
data EncodeData a = Multiple Int a | Single a deriving Show
encodeModified [] = []
encodeModified x = map (\x -> (Multiple (length x) (head x))) $ group x

-- Problem. 12
decodeModified :: Eq a => [EncodeData a] -> [a]
decodeModified [] = []
decodeModified x = concat $ map (\x -> case x of 
                                        Multiple l d -> (replicate l d) 
                                        Single d -> [d]) x
                                        
-- Problem. 13
encodeDirect :: Eq a => [a] -> [EncodeData a]
encodeDirect [] = []
encodeDirect x = map (\x -> if length x == 1 then (Single (head x)) else (Multiple (length x) (head x))) $ group x


-- Problem. 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs


-- Problem. 15
repli :: [a] -> Int -> [a]
--repli [] _ = []
--repli (x:xs) k = (replicate k x)++repli xs k

-- another beatiful implementation
--repli = flip $ concatMap . replicate

--repli xs n = xs >>= replicate n

repli [] _ = []
repli (x:xs) n = foldr (const (x:)) (repli xs n) [1..n]

-- Problem. 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery x n = dropEveryH x 1 n

dropEveryH :: [a] -> Int -> Int -> [a]
dropEveryH [] _ _ = []
dropEveryH (x:xs) i k = if (mod i k) == 0 then (dropEveryH xs (i+1) k) else x:(dropEveryH xs (i+1) k)

-- Problem. 17
split :: [a] -> Int -> ([a], [a])
split x n = (take n x, drop n x)


-- Problem. 18
slice :: [a] -> Int -> Int -> [a]
slice x i1 i2 = take (i2 - i1 + 1) $ drop (i1 - 1) x

-- Problem. 19
rotate :: [a] -> Int -> [a]
rotate x n = drop n x ++ take n x

-- Problem. 20
removeAt :: Int -> [a] -> (a, [a])
removeAt k x = (last $ take k x, (init $ take k x) ++ drop k x)

-- Problem. 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = (take (n-1) xs)++[x]++(drop (n-1) xs)

-- Problem. 22
range :: Int -> Int -> [Int]
range a b = [a..b]

-- Problem. 23
-- rnd_select :: [a] -> Int -> IO [a]
-- rnd_select _ 0 = return []
-- rnd_select [] _ = return []
-- rnd_select xs count = do
                    -- r <- randomRIO (0, (length xs)-1)
                    -- rest <- rnd_select (removeAt (r+1) xs) (count-1)
                    -- return (xs!!r):rest
                    
  




  
-- Problem. 26
combinations :: Int -> [a] -> [[a]]
-- combinations _ [] = []
-- combinations 0 _ = [[]]
-- combinations n (x:xs) = (map (x:) (combinations (n-1) xs) ++ (combinations n xs))

combinations 0 _ = [[]]
combinations n xs = [xs !! i : x | i <- [0..(length xs)-1]
								  ,x <- combinations (n-1) (drop (i+1) xs)]







-- Problem. 28
lfsort :: [[a]] -> [[a]]
lfsort [] = []
lfsort (x:xs) = 
	let 
		left = lfsort [l | l <- xs, length l < length x]
		right = lfsort [r | r <- xs, length r >= length x]
	in  left++[x]++right


---------------------------------------------------------------------------------------------
---- Arithmetic

-- Problem. 31
isPrime :: Integer -> Bool
isPrime x = all (==True) $ map (\i -> if mod x i == 0 then False else True) [2..floor $ sqrt $ fromIntegral x]




-- Problem. 32
myGCD :: Integer -> Integer -> Integer
--myGCD x y 
--    | y == 0 = abs x
--    | otherwise = myGCD y (mod x y)

myGCD x y = if x == 0 then y
            else if y == 0 then x
                 else if x > y then myGCD (abs y) (x-y)
                      else myGCD (abs x) (y-x)


-- Problem. 33
coprime :: Integer -> Integer -> Bool
coprime x y = if myGCD x y == 1 then True else False


-- Problem. 34
totient :: Int -> Int
totient m = m













---------------------------------------------------------------------------------------------
---- Logic and Codes

-- Problem. 46







-- Problem. 49
gray :: Int -> [String]
gray 0 = [""]
--gray n = (map ('0':) (gray (n-1))) ++ (reverse (map ('1':) (gray (n-1))))
gray n = let xs = gray (n-1) in map ('0':) xs ++ map ('1':) (reverse xs)


-- Problem. 50
huffman :: [(Char, Int)] -> [(Char, String)]
huffman [] = []

thesort :: [(Char, Int)] -> [(Char, Int)]
thesort [] = []
thesort (x:xs) = 
    let left = [l | l <- xs, snd l < snd x]
        right = [r | r <- xs, snd r >= snd x]
    in  right++[x]++left





---------------------------------------------------------------------------------------------
---- Binary trees

-- Problem. 54A

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)



