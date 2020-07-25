{-# LANGUAGE BangPatterns #-}

module Main where
import Data.List
import Control.Monad
import Control.Monad.Loops
import Control.Exception

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



-- stack --profile run myexec --rts-options -p


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

{-

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

