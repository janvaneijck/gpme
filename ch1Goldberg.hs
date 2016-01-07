module Ch1Goldberg where 
import Control.Monad
import Data.List
import Data.Ord
import System.Random

-- Exercise A

testrandom :: Int -> [Int]
testrandom n = take 20 ( randoms $ mkStdGen n :: [Int])

testrandom' :: (Random a, Num a) => Int -> [a]
testrandom' n = take 20 ( randoms $ mkStdGen n )

listdigits :: Integral t => t -> [t]
listdigits n = listdigits' (abs n) where
    listdigits' 0 = []
    listdigits' x = listdigits (x `div` 10) ++ [x `mod` 10]

countdigits :: Integral a => [a] -> [Int]    
countdigits xs = map (length . listdigits) xs

randomrange :: Random a => Int -> Int -> (a,a) -> [a]
randomrange seed m (a,b) = take m ( randomRs (a,b) (mkStdGen seed) )

countPartitions :: (Num t1, Ord a) => [a] -> [((a, a), t)] -> [(t, t1)]
countPartitions _ [] = []
countPartitions list [((a,b),label)] = [(label, countPartitions' (a,b) list)] where
    countPartitions' _ [] = 0
    countPartitions' (a,b) (n:rest) = if n <= b && a <= n
        then 1 + (countPartitions' (a,b) rest)
        else countPartitions' (a,b) rest
countPartitions list (((a,b),label):rest) = ((label, countPartitions' (a,b) list): countPartitions list rest) where
    countPartitions' _ [] = 0
    countPartitions' (a,b) (n:rest) = if n < b && a <= n
        then 1 + (countPartitions' (a,b) rest)
        else countPartitions' (a,b) rest

{-
Example use: countPartitions (randomrange 0 1000 (0.0,1.0)) (zip [(0.0,0.25),(0.25,0.5),(0.5,0.75),(0.75,1.0)] [1..4])
                              

-- Exercise B:
sum [0.1, 0.2, 0.05, 0.15, 0.0, 0.11, 0.07, 0.04, 0.00, 0.12, 0.16] === 1, so these
probabilities are consistent.

-}

probB :: [Float]
probB = [0.1, 0.2, 0.05, 0.15, 0.0, 0.11, 0.07, 0.04, 0.00, 0.12, 0.16]

cumList :: Fractional t => [t] -> [(t, t)]
cumList [] = []
cumList (p:rest) = ((0.0,p): cumList' p rest) where
    cumList' _ [] = []
    cumList' p (q:rest) = ( (p,p+q) : cumList' (p+q) rest )

roulette :: (Fractional a, Num b, Ord a, Random a) => Int -> [a] -> Int -> [(Int, b)]
roulette seed probs k = let
                    zipped = zip (cumList probs) [1..length probs]
                    cleanZipped = filter (\((a,b),_) -> b-a /= 0) zipped
                    cleanCounts = countPartitions (randomrange seed k (0.0,1.0)) cleanZipped
                    dirtyCounts = cleanCounts ++ [ (x+1,0) | x <- [0..length probs - 1], probs !! x ==0]
                        in sortBy (comparing fst) dirtyCounts
                        
{- 
Example use: roulette 0 probB 1000, this gives:
[(1,87),(2,212),(3,59),(4,141),(5,0),(6,119),(7,70),(8,46),(9,0),(10,102),(11,164)]



-- Exercise D:
-}

crossover :: [a] -> [a] -> Int -> [[a]]
crossover str1 str2 csv = let
        (str11, str12) = splitAt csv str1
        (str21, str22) = splitAt csv str2
        in [str11 ++ str22, str21 ++ str12]

{-
Example use: crossover [1,1,1,1] [1,0,1,0] 2, this gives ([1,1,1,0],[1,0,1,1])
             crossover "1111" "1010" 2, this gives ["1110", "1011"]


-- Exercise E:
-}

mutationBinary seed p string = let
            coinflips = randomrange seed (length string) (0.0,1.0)
            in mutation' coinflips string where
                mutation' _ [] = []
                mutation' (coinflip:restflips) ('0':restbits) = if coinflip < p
                    then '1':mutation' restflips restbits
                    else '0':mutation' restflips restbits
                mutation' (coinflip:restflips) ('1':restbits) = if coinflip < p
                    then '0':mutation' restflips restbits
                    else '1':mutation' restflips restbits
                mutation' (coinflip:restflips) (b:restbits) = error "no binary string as argument"

                {-
Example use: mutationBinary 0 0.5 "111111111", this gives "100111110"

-- Exercise E:
-}

-- Create a list of tuples from a list
currify :: [a] -> [(a,a)]
currify [] = []
currify [_] = error "odd number of elements"
currify (x:y:xs) = (x,y):currify xs

-- Create a list of triples from a list
currifytrip :: [a] -> [(a,a,a)]
currifytrip [] = []
currifytrip [_] = error "cardinality modulo 3 is not 0"
currifytrip [_,_] = error "cardinality modulo 3 is not 0"
currifytrip (x:y:z:xs) = (x,y,z):currifytrip xs

population :: [String]
population = (replicate 100 "11100") ++ (replicate 100 "00011")

smallpopulation :: [String]
smallpopulation = ["11111", "00000"]

matingpool :: Int -> [a] -> Int -> [(a, a)]
matingpool seed pop size = let
        coinflips = randomrange seed size (0, length pop - 1)
    in currify $ extractMates pop coinflips where
        extractMates pop [] = []
        extractMates pop (i:rest) = pop !! i : extractMates pop rest
 
evolve :: (Fractional a, Ord a, Random a) => Int -> [[Char]] -> Int -> Int -> a -> Int -> [[Char]]
evolve seed pop gensize gennum mutation orgsize = let
            seedsgen = currifytrip $ map (`mod` 10000) (take (3*gennum) (randoms $ mkStdGen seed :: [Int]))
        in evolve' seedsgen pop gennum where
            evolve' _ pop 0 = pop
            evolve' ((s1,s2,s3):seeds) pop k = let
                    pool = matingpool s1 pop gensize
                    seedscross = randomrange s2 gensize (1,orgsize)
                    seedsmut   = map (`mod` 10000) (take gensize (randoms $ mkStdGen s3 :: [Int]))
                in evolve' seeds (mutate seedsmut (reproduction seedscross pool)) (k-1) where
                    reproduction [] _ = []
                    reproduction _ [] = []
                    reproduction (s:srest) ((a,b):prest) = (crossover a b s) ++ (reproduction srest prest)
                    mutate _ [] = []
                    mutate (s:srest) (o:orest) = mutationBinary s mutation o : (mutate srest orest)

{-
evolve 0 smallpopulation 10 4 0.7 5, this gives:
["00111","01000","01111","01110","11001","00110","00000","01101","10100","01010"]

-}