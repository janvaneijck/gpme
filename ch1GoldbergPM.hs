module Ch1Goldberg where 
import Control.Monad
import Data.List
import Data.Ord
import System.Random

import Data.Array.ST
import Control.Monad.ST
import Data.STRef

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

mediumpopulation :: [String]
mediumpopulation = (replicate 10 "11111") ++ (replicate 10 "11100") ++ (replicate 10 "00011") ++ (replicate 10 "00000") ++ (replicate 10 "00110")

bitstringtofloat :: [Char] -> Float
bitstringtofloat xs = bitstringtofloat' xs (length xs) where
   bitstringtofloat' [] _ = 0
   bitstringtofloat' (x:xs) count = (bittofloat x)*2^(count-1) + (bitstringtofloat' xs (count-1))
   bittofloat x = if x == '1' then 1.0 else 0.0

fitnessfunctionneutral :: Num b => [a] -> [(a, b)]
fitnessfunctionneutral xs = zip xs (replicate (length xs) 1)

fitnessfunction1 :: [[Char]] -> [([Char], Float)]
fitnessfunction1 xs = zip xs (fitnessfunction1' xs) where
    fitnessfunction1' [] = []
    fitnessfunction1' (x:rest) = [bitstringtofloat x] ++ fitnessfunction1' rest
    
-- This function was found at: https://wiki.haskell.org/Random_shuffle
shuffle :: Int -> [b] -> [b]
shuffle seed xs = let
    gen = mkStdGen seed
    in runST (do
            g <- newSTRef gen
            let randomRST lohi = do
                (a,s') <- liftM (randomR lohi) (readSTRef g)
                writeSTRef g s'
                return a
            ar <- newArray n xs
            xs' <- forM [1..n] $ \i -> do
                    j <- randomRST (i,n)
                    vi <- readArray ar i
                    vj <- readArray ar j
                    writeArray ar j vi
                    return vj
            gen' <- readSTRef g
            return xs')
    where
        n = length xs
        newArray :: Int -> [a] -> ST s (STArray s Int a)
        newArray n xs =  newListArray (1,n) xs

matingpool :: (Fractional a, Ord a, Random a) => Int -> [a1] -> Int -> ([a1] -> [(t, a)]) -> [(a1, a1)]
matingpool seed pop gensize fitnessfunction = let
        popfit = fitnessfunction pop -- the fitnessfunction used should return a list [(Organism, Fittnessvalue)]
        totfit = sum (map snd popfit)
        popprob = populationprobs popfit totfit where
            populationprobs [] _ = []
            populationprobs ((a,b):rest) totfit = (a, (b/totfit) ):(populationprobs rest totfit)
        fortunescores = zip pop (roulette' seed popprob gensize) -- gives a list [(string, frequency)]
        mates = mates' fortunescores where
            mates' [] = []
            mates' ((a,b):rest) = (replicate b a) ++ (mates' rest)
        in currify (shuffle seed mates)

roulette' :: (Fractional b, Num b1, Ord b, Random b) => Int -> [(a, b)] -> Int -> [b1]
roulette' seed popprob k = let
                    probs = map snd popprob
                    zipped = zip (cumList probs) [1..length probs]
                    cleanZipped = filter (\((a,b),_) -> b-a /= 0) zipped
                    cleanCounts = countPartitions (randomrange seed k (0.0,1.0)) cleanZipped
                    dirtyCounts = cleanCounts ++ [ (x+1,0) | x <- [0..length probs - 1], probs !! x ==0]
                    orderedlist = sortBy (comparing fst) dirtyCounts
                        in map snd orderedlist


evolve :: (Fractional a, Fractional a1, Ord a, Ord a1, Random a, Random a1) => 
 Int -> [[Char]] -> Int -> Int -> a -> Int -> ([[Char]] -> [(t, a1)]) -> [[Char]]
evolve seed pop gensize gennum mutation orgsize fitnessfunction = let
            seedsgen = currifytrip $ map (`mod` 10000) (take (3*gennum) (randoms $ mkStdGen seed :: [Int]))
        in evolve' seedsgen pop gennum where
            evolve' _ pop 0 = pop
            evolve' ((s1,s2,s3):seeds) pop k = let
                    pool = matingpool s1 pop gensize fitnessfunction
                    seedscross = randomrange s2 gensize (1,orgsize)
                    seedsmut   = map (`mod` 10000) (take gensize (randoms $ mkStdGen s3 :: [Int]))
                in evolve' seeds (mutate seedsmut (reproduction seedscross pool)) (k-1) where
                    reproduction [] _ = []
                    reproduction _ [] = []
                    reproduction (s:srest) ((a,b):prest) = (crossover a b s) ++ (reproduction srest prest)
                    mutate _ [] = []
                    mutate (s:srest) (o:orest) = mutationBinary s mutation o : (mutate srest orest)
 
    
-- Old matingpool function
matingpool' :: Int -> [a] -> Int -> [(a, a)]
matingpool' seed pop size = let
        coinflips = randomrange seed size (0, length pop - 1)
    in currify $ extractMates pop coinflips where
        extractMates pop [] = []
        extractMates pop (i:rest) = pop !! i : extractMates pop rest
    


