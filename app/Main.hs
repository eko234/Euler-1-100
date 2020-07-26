{-# LANGUAGE BangPatterns #-}

module Main where
import Data.List
import Data.Ord
import Control.Monad
import Control.Monad.Loops
import Control.Exception
import Data.Function
import Data.List.Ordered (minus, union, unionAll)

main = print "hi"


-- 1
-- Multiples of 3 and 5

euler1 lim = foldr (+) 0 $ [x | x <- [1,2..lim-1] , rem x 3 == 0 || rem x 5 == 0] 

-- 2
-- Even Fibonacci numbers

euler2 lim = go 0 (0,1) 
  where
    go !s (!x,!y) | next >= lim = s
                  | rem next 2 == 0 = go (s+next)  (y,next) 
                  | otherwise       = go (s)       (y,next) 
      where next = (x+y)

-- 3
-- Largest prime factor


primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

euler3 1 = [1]
euler3 on = go primes 1 on [] 
  where
    go (x:xs) !c !n r
      | c * x  == on = (x:r)
      | rem n x == 0 = go (x:xs) (x * c) (quot n x) (x:r) 
      | otherwise   =  go (xs) (c) (n) r

-- 4
-- Largest palindrome product


euler4Numbers ::  [   Integer  ]
euler4Numbers =   [100,101..999]

e4'1
  = last
  $ sort
  $ filter isPalindrome 
  $ fmap product 
  $ (combinations 2 euler4Numbers) ++ fmap (\e -> [e,e]) euler4Numbers 
 

e4'2
  = head 
  $ dropWhile (not . isPalindrome)
  $ reverse
  $ sort
  $ fmap product 
  $ (combinations 2 euler4Numbers) ++ fmap (\e -> [e,e]) euler4Numbers 


e4'3
  = head 
  $ dropWhile (not . isPalindrome)
  $ sortDesc
  $ fmap product  
  $ (combinations 2 euler4Numbers) ++ fmap (\e -> [e,e]) euler4Numbers
  where
    sortDesc :: (Ord a, Num a) => [a] -> [a]
    sortDesc = sortBy (flip compare)


euler4
  = head 
  $ dropWhile (not . isPalindrome)
  $ sortDesc
  $ fmap product 
  $ sequence [euler4Numbers,euler4Numbers]
  where
    sortDesc :: (Ord a, Num a) => [a] -> [a]
    sortDesc = sortBy (flip compare)


isPalindrome n
  = let srn = show n in
      case rem (length srn) 2 of
        0 -> (drop (div l 2) s) == (reverse $ take (div l 2) s)  
        _ -> (drop ((div l 2) + 1) s) == (reverse $ take ((div l 2) ) s)
  where l = length s
        s = show n


combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']


-- 5
-- Smallest multiple



-- horrible solution
e5'1 top = head $ dropWhile (\e -> not $ e `divByEvery`  [2,3..top]) [2,3..top]
  where 
    divByEvery n ar =  all (0==) $ (fmap (rem n) ar)


euler5 top = go $ sortByLengthDesc $ (head . group . euler3) <$> [2,3..top]
  where
    go [    ] = 1
    go (x:xs) = product x * (go $ filterCurrentPrime  xs)
      where filterCurrentPrime = filter (not . elem (x!!0))



sortByLengthDesc :: (Ord a , Num a) => [[a]] -> [[a]]
sortByLengthDesc = sortBy (flip $ comparing length)


-- 6
-- Sum square difference





















{-

-- 333
-- soon


s  :: (Ord a , Num a) => [[a]] -> [[a]]
s = sortOn length    



isPrime 1 = False
isPrime n = go 2 (div n 2) 
  where
    go !i !l | rem n i == 0 = False
             | i >= l       = True   -- ?
             | otherwise    = go (i+1) l 


partitions :: Float -> [[(Integer,Integer)]]
partitions n =
  fmap
  (\e -> zip (left e) (right e))
  [base,base-1..0]
  where
    left     = cycle  . flip (:) []   
    right    = zeroTo . toPart
    zeroTo x = [0,1..x]   
    toPart x = floor $ logBase 2 $ n/3^x
    base     = floor $ logBase 3 n    

toNumbers :: [[(Integer,Integer)]] -> [[Integer]]
toNumbers = fmap (\l -> 0 : fmap (\(a,b) -> 3^a * 2^b )  l) 

filterHighNumbers :: Integer -> [[Integer]] -> [[Integer]]
filterHighNumbers n = fmap (filter (n >=))
         
divisible [0,_] = False
divisible [_,0] = False
divisible l = rem b a == 0
  where
    [a,b] = sort l

isValid :: Integer -> [[Integer]] -> Bool
isValid n x = go n x 0
  where
    go :: Integer -> [[Integer]] -> Integer -> Bool 
    go _ [    ] !c = c == 1
    go _  _     !c | c > 1 = False
    go i (y:ys) !c
 --     | foldr (+) 0 y == i  = go i ys (c+1)
      | foldr (+) 0 y == i  && ( divisibleFree y ) = go i ys (c+1)
      | otherwise = go i ys c
      
{-# Inline isValid #-}

divisibleFree l
  | elem 1 l = False
  | otherwise = not . elem True $  divisible <$> (combinations 2 $ filter (0/=) l)


solve n
  = isValid n
  $ sequence
  $ filterHighNumbers n candidates 


main :: IO ()
main = do
  f <- readFilerino
  r <- forkMapM (\e -> if (solve e == True) then (pure e)  else (pure 0) ) f
  print $ sum $ fmap fromR r
  --print $ filter (solve) f
  print "done"

readFilerino = do
  f <- readFile "/home/juicyjouissance/Descargas/primes-to-100k.txt"
  let primis = lines f
  let primiparos = fmap (filter (\c -> c/= '\r' )) primis
  let primiparoides = take 1000 $ fmap (\e -> read e :: Integer) primiparos
  return primiparoides

candidates = fmap (filter (1/=)) $ toNumbers $ partitions 10000

fromR e = case e of
  Right a -> a
  _       -> 0

z = toNumbers $ partitions 10


sense = go [] 
  where
    go lu [    ] = []    
    go lu (x:xs) = x : go [] xs 

--sow :: [[Integer]] -> [[Integer]]
sow l = go pe ze 
  where
    ze = fmap (fillZ ( fromInteger (getTop l) )) l
    pe = perms (head ze)
    go [    ] _ = []
    go (x:xs) y = zipWith (\a b -> b!!a) x y : go xs y

filterino :: [[Integer]] -> [[Integer]]
filterino = filter (\e ->  not (elem 1 e)) 

perms l = permutations [0,1..len-1]
  where len = length l
  
getTop :: [[Integer]] -> Integer
getTop = toInteger . length . last


fillZ f l = l ++ (replicate (f - len) 0) 
  where len =  length l


combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

{-
Zero is like taking none so no options are discarded and just skips a list
so we need a structure that rememeber which palces are taken and is also able to read  and sicard places of each
successive list

i.e.  if i took 1 then i cant take a 1 postition anymore,



-}

{-
sense :: [[Integer]] -> [[Integer]]
sense l = go l []
  where
    go (x:xs) a =  undefined -- go xs (  : a)

sense1 = go 0
  where
    go c (x:xs) = ((take 1 x) ++ (drop (c+1) x)) : (go (c+1) xs )
    go _ []  = []

mutilate :: [a] -> Int -> [a]
mutilate l n = take 1 l ++ drop (n+1) l
sense2' n (x:y:ys)  = do
  let sas = mutilate y n
  a <- x
  b <- sas
  c <- ys 
sense3 [] = []
sense3 (x:xs) = do
  a <- x
  let z = sense3 xs
  [b] <- z
  return $ [a, b] 
  return $  [a,b, sense2' (n+1) c ]   -- (sense2' (n+1) ys)
-}


--mt [] _ = []


solve' n
  = length
  -- expresion
  $ filter (==n)       -- take while
  $ fmap (foldl (+) 0) -- take While
  $ fmap (\l -> 0 : l)
--  $ filter (\e -> not $ elem True $ fmap divisible $ combinations 2 e)
  $ sequence
  $ filterHighNumbers n candidates 


    --  | scanTill n y == i && ( not $ elem True $ fmap divisible  $ combinations 2 y ) = go i ys (c+1)
    --  | scanTill' i y && ( not $ elem True $ fmap divisible  $ combinations 2 y ) = go i ys (c+1)
    --  | foldl (+) 0 y == i = go i ys (c+1)




scanTill n l = last $ takeWhile (<=n) $ scanl (+) 0 l  

scanTill' :: Integer -> [Integer] -> Bool
scanTill' n l = go n l 0
  where
    go n' [] c = n' == c
    go n' (x:xs) c
      | x + c > n' = False
      | otherwise  = go n' xs (x + c)
      

-}

