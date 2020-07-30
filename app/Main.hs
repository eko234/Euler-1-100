{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ParallelListComp #-}


module Main where
import Data.List
import Data.List.Split
import Data.Ord
import Control.Monad
import Control.Monad.Loops
import Control.Exception
import Data.Function
import Data.List.Ordered (minus, union, unionAll)
import Data.List.Ordered (minus, union, unionAll)


a' = do
  x <- [1,2]
  z <- [3,4]
  return [x,z]


b' = do
  z <- [3,4]
  x <- [1,2]
  return [x,z]


main = do
  print a'
  print b'

-- 1
-- Multiples of 3 and 5

euler1 lim = foldr (+) 0 [x | x <- [1,2..lim-1] , rem x 3 == 0 || rem x 5 == 0] 

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

euler4
  = head 
  $ dropWhile (not . isPalindrome)
  $ sortDesc
  $ fmap product 
  $ sequence [euler4Numbers,euler4Numbers]
  where
    sortDesc :: (Ord a, Num a) => [a] -> [a]
    sortDesc = sortBy (flip compare)

isPalindrome :: Integer -> Bool
isPalindrome n = show n == (reverse $ show n)

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']


-- 5
-- Smallest multiple

euler5 top = go $ sortByLengthDesc $ (head . group . euler3) <$> [2,3..top]
  where
    go [    ] = 1
    go (x:xs) = product x * (go $ filterCurrentPrime  xs)
      where filterCurrentPrime = filter (not . elem (x!!0))

sortByLengthDesc :: (Ord a , Num a) => [[a]] -> [[a]]
sortByLengthDesc = sortBy (flip $ comparing length)


-- 6
-- Sum square difference

euler6 :: Int -> Int
euler6  n = ((n * (n+1)`div`2)^2) - (n * (n+1) * ( 2 * n+1 ) `div` 6 ) 

-- 7
-- 10001st prime
euler7 ix = primes!!(ix-1)


-- 8
-- Largest product in a series

e8Numbers =  "73167176531330624919225\
             \11967442657474235534919\
             \49349698352031277450632\
             \62395783180169848018694\
             \78851843858615607891129\
             \49495459501737958331952\
             \85320880551112540698747\
             \15852386305071569329096\
             \32952274430435576689664\
             \89504452445231617318564\
             \03098711121722383113622\
             \29893423380308135336276\
             \61428280644448664523874\
             \93035890729629049156044\
             \07723907138105158593079\
             \60866701724271218839987\
             \97908792274921901699720\
             \88809377665727333001053\
             \36788122023542180975125\
             \45405947522435258490771\
             \16705560136048395864467\
             \06324415722155397536978\
             \17977846174064955149290\
             \86256932197846862248283\
             \97224137565705605749026\
             \14079729686524145351004\
             \74821663704844031998900\
             \08895243450658541227588\
             \66688116427171479924442\
             \92823086346567481391912\
             \31628245861786645835912\
             \45665294765456828489128\
             \83142607690042242190226\
             \71055626321111109370544\
             \21750694165896040807198\
             \40385096245544436298123\
             \09878799272442849091888\
             \45801561660979191338754\
             \99200524063689912560717\
             \60605886116467109405077\
             \54100225698315520005593\
             \57297257163626956188267\
             \04282524836008232575304\
             \20752963450"


euler8 :: Int 
euler8 = go (fmap (\e -> read [e] :: Int) e8Numbers) 0
  where
    go :: [Int] -> Int -> Int
    go scan@(a:b:c:d:e:f:g:h:i:j:k:l:m:ms) last
      | next > last = go (tail scan) next
      | otherwise   = go (tail scan) last
      where next = product [a,b,c,d,e,f,g,h,i,j,k,l,m]
    go _ x = x 




-- 9
-- Special Pythagorean triplet

euler9 :: Int -> Int
euler9 n 
  =  (product . head) [ [x,y,z] 
        | x <- [1,  2..div n 3] 
        , y <- [x,x+1..div n 2]
        , let z = n -x-y
        , (x*x) + (y*y) == z * z ]
 
-- 10
-- Sumation of primes

euler10 :: Int -> Int
euler10 lim = sum $ takeWhile (<lim) primes


-- 11
-- Largest product in a grid

scan4 :: [a] -> [[a]]
scan4 (a:b:c:d:ds)  = [a,b,c,d] : scan4 (b:c:d:ds)  
scan4 _             = []

diagonalify4 :: (Num a, Ord a) => [[a]] -> [[a]]
diagonalify4 = filter ((4>=) . length) . transpose . go 0
  where go n (x:xs) = (drop n x) : go (n+1) xs 
        go n _      = []   

greatestH  :: (Num a, Ord a) => [[a]] -> a
greatestH  =  maximum . fmap (maximum . fmap product . scan4)
greatestV  :: (Num a, Ord a) => [[a]] -> a
greatestV  =  greatestH . transpose
greatestD1 :: (Num a, Ord a) => [[a]] -> a
greatestD1 =  maximum . fmap (maximum . fmap product . diagonalify4) . scan4
greatestD2 :: (Num a, Ord a) => [[a]] -> a
greatestD2 =  greatestD1 . reverse

euler11 matrix
  = maximum 
    [ greatestH matrix
    , greatestV matrix
    , greatestD1 matrix
    , greatestD2 matrix ] 

euler11Numbers 
  =   [[08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08]
      ,[49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00]
      ,[81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65]
      ,[52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91]
      ,[22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80]
      ,[24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50]
      ,[32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70]
      ,[67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21]
      ,[24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72]
      ,[21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95]
      ,[78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92]
      ,[16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57]
      ,[86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58]
      ,[19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40]
      ,[04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66]
      ,[88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69]
      ,[04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36]
      ,[20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16]
      ,[20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54]
      ,[01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48]]

-- 12
-- Highly divisible triangular number
floorSqrt :: (Integral a) =>  a -> a
floorSqrt = floor . sqrt . fromIntegral

triangleNumbers = 1 : [ x + y | x <- [2..]
                              | y <- triangleNumbers ]


euler12
  = head
  $ dropWhile
  ( (500 >=) . product . map ((+1).length) . group . euler3 )
  triangleNumbers



-- 13
-- Large sum

stringToHuge s =fmap (\n -> read n :: Int) $ chunksOf 10 s 

adjust :: [Int] -> [Int]
adjust l =  reverse $ go (reverse l) 0 
  where
    go (x:xs) n =  (currFix+n) : go xs nextFix
      where
        length' = (length $ show x) - 10
        currFix = read (drop (length') $ show x) :: Int
        nextFix = case (take (length') $ show x)  of
                    "" -> 0
                    st -> read  st :: Int
    go [] 0 = []
    go [] n = [n]


pairHuge x y | lx > ly   = (x, r0 (lx - ly) ++ y)
          | lx < ly   = (r0 (ly-lx) ++ x   ,y)
          | otherwise = (x,y)
          where
            lx = length x
            ly = length y
            r0 n = take n $ cycle [0] 

sumHuge :: (Num a) => [a] -> [a] -> [a]

sumHuge x y =  [ (a+b) | a <- x' | b <- y']
  where (x',y') = pairHuge x y
  

euler13
  = id
  $ foldl (\ a b -> adjust $ sumHuge a b) []
  $ fmap stringToHuge
  $ chunksOf 50 euler13Numbers

-- with mighty integers
euler13'
  = id
  $ take 10
  $ show
  $ sum
  $ fmap (\n -> read n :: Integer)
  $ chunksOf 50
  $ euler13Numbers

euler13Numbers = "37107287533902102798797998220837590246510135740250463769376774900097126481248969700780504170182605387432498619952474105947423330951305812372661730962991942213363574161572522430563301811072406154908250230675882075393461711719803104210475137780632466768926167069662363382013637841838368417873436172675728112879812849979408065481931592621691275889832738442742289174325203219235894228767964876702721893184745144573600130643909116721685684458871160315327670386486105843025439939619828917593665686757934951621764571418565606295021572231965867550793241933316490635246274190492910143244581382266334794475817892575867718337217661963751590579239728245598838407582035653253593990084026335689488301894586282278288018119938482628201427819413994056758715117009439035398664372827112653829987240784473053190104293586865155060062958648615320752733719591914205172558297169388870771546649911559348760353292171497005693854370070576826684624621495650076471787294438377604532826541087568284431911906346940378552177792951453612327252500029607107508256381565671088525835072145876576172410976447339110607218265236877223636045174237069058518606604482076212098132878607339694128114266041808683061932846081119106155694051268969251934325451728388641918047049293215058642563049483624672216484350762017279180399446930047329563406911573244438690812579451408905770622942919710792820955037687525678773091862540744969844508330393682126183363848253301546861961243487676812975343759465158038628759287849020152168555482871720121925776695478182833757993103614740356856449095527097864797581167263201004368978425535399209318374414978068609844840309812907779179908821879532736447567559084803087086987551392711854517078544161852424320693150332599594068957565367821070749269665376763262354472106979395067965269474259770973916669376304263398708541052684708299085211399427365734116182760315001271653786073615010808570091499395125570281987460043753582903531743471732693212357815498262974255273730794953759765105305946966067683156574377167401875275889028025717332296191766687138199318110487701902712526768027607800301367868099252546340106163286652636270218540497705585629946580636237993140746255962240744869082311749777923654662572469233228109171419143028819710328859780666976089293863828502533340334413065578016127815921815005561868836468420090470230530811728164304876237919698424872550366387845831148769693215490281042402013833512446218144177347063783299490636259666498587618221225225512486764533677201869716985443124195724099139590089523100588229554825530026352078153229679624948164195386821877476085327132285723110424803456124867697064507995236377742425354112916842768655389262050249103265729672370191327572567528565324825826546309220705859652229798860272258331913126375147341994889534765745501184957014548792889848568277260777137214037988797153829820378303147352772158034814451349137322665138134829543829199918180278916522431027392251122869539409579530664052326325380441000596549391598795936352974615218550237130764225512118369380358038858490341698116222072977186158236678424689157993532961922624679571944012690438771072750481023908955235974572318970677254791506150550495392297953090112996751986188088225875314529584099251203829009407770775672113067397083047244838165338735023408456470580773088295917476714036319800818712901187549131054712658197623331044818386269515456334926366572897563400500428462801835170705278318394258821455212272512503275512160354698120058176216521282765275169129689778932238195734329339946437501907836945765883352399886755061649651847751807381688378610915273579297013376217784275219262340194239963916804498399317331273132924185707147349566916674687634660915035914677504995186714302352196288948901024233251169136196266227326746080059154747183079839286853520694694454072476841822524674417161514036427982273348055556214818971426179103425986472045168939894221798260880768528778364618279934631376775430780936333301898264209010848802521674670883215120185883543223812876952786713296124747824645386369930090493103636197638780396218407357239979422340623539380833965132740801111666627891981488087797941876876144230030984490851411606618262936828367647447792391803351109890697907148578694408955299065364044742557608365997664579509666024396409905389607120198219976047599490197230297649139826800329731560371200413779037855660850892521673093931987275027546890690370753941304265231501194809377245048795150954100921645863754710598436791786391670211874924319957006419179697775990283006991536871371193661495281130587638027841075444973307840789923115535562561142322423255033685442488917353448899115014406480203690680639606723221932041495354150312888033953605329934036800697771065056663195481234880673210146739058568557934581403627822703280826165707739483275922328459417065250945123252306082291880205877731971983945018088807242966198081119777158542502016545090413245809786882778948721859617721078384350691861554356628840622574736922845095162084960398013400172393067166682355524525280460972253503534226472524250874054075591789781264330331690"


-- 14
-- Longest Collatz sequence

-- triangularNumbers!!0




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

