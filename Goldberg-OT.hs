module Chapter1Goldberg where
import Data.List
import System.Random
import Data.Ord

-- this is a test function, giving 10 random integers for a given seed
testRandom :: (Random a, Num a) => Int -> [a]
testRandom n = take 10 (randoms $ mkStdGen n)

-- this function takes an integer and will return the individual digits of this integer in a list in the same order
listDigits :: Int -> [Int]
listDigits n = listDigits' (abs n) where
    listDigits' 0 = []
    listDigits' n = let k = n `mod` 10
        in (listDigits' (n `div` 10)) ++ [k]

-- randomRange takes a seed, an interval and a quantity n and returns n values uniformly picked from the interval using the seed.
randomRange :: Random a => Int -> (a, a) -> Int -> [a]
randomRange seed (a,b) k = take k $ randomRs (a,b) (mkStdGen seed)


------------------------------------------------------- CHAPTER 1 -------------------------------------------------------------

-- EXERCISE A

-- countPartitions is given a list of values and a list of intervals with labels and counts how many values in the first occur in
-- each interval, using the labels of the intervals in the eventual representation of the answer.
-- IMPORTANT: the list of intervals should be an ordered list of intervals, ordered from small to large!
-- (more precisely: the last interval should also be the highest actual interval, the rest can be in any order)
countPartitions :: (Num a, Ord a) => [a] -> [(b,(a,a))] -> [(b,Int)]
countPartitions _ [] = []
countPartitions list [(label,(a,b))] = [(label, countPartitions' (a,b) list)] where
    countPartitions' _ [] = 0
    countPartitions' (a,b) (n:rest) = if n <= b && a <= n
        then 1 + (countPartitions' (a,b) rest)
        else countPartitions' (a,b) rest
countPartitions list ((label,(a,b)):rest) = ((label, countPartitions' (a,b) list) : countPartitions list rest) where
    countPartitions' _ [] = 0
    countPartitions' (a,b) (n:rest) = if n < b && a <= n
        then 1 + (countPartitions' (a,b) rest)
        else countPartitions' (a,b) rest

{-
for random generator 0: 258, 223, 265, 254
    code used: countPartitions (randomRange 0 (0.0,1.0) 1000) [(0.0,0.25),(0.25,0.5),(0.5,0.75),(0.75,1.0)]
    -}


-- EXERCISE B

-- the list used in the exercise
listProb :: [Float]
listProb = [0.10, 0.20, 0.05, 0.15, 0.00, 0.11, 0.07, 0.04, 0.00, 0.12, 0.16]

-- cumList converts a list of probabilities (summing up to 1) into a list of intervals corresponding to the probabilities
cumList :: Fractional t => [t] -> [(t, t)]
cumList [] = []
cumList (p:rest) = (0.0,p) : cumList' p rest where
    cumList' _ [] = []
    cumList' p (q:rest) = (p,p+q) : cumList' (p+q) rest

-- roulette takes a seed, a list of of probabilities and a number k and picks k elements from the list of probabilities randomly with
-- the respective probabilities, using the seed given as input. It outputs the frequency of each element in the list of probabilities.
roulette :: (Fractional a, Ord a, Random a) => Int -> [a] -> Int -> [(Int, Int)]
roulette seed list k = let
        zipped = zip [1..length list] (cumList list)
        cleanZipped = filter (\(_,(a,b)) -> b-a /= 0) zipped
        cleanCounts = countPartitions (randomRange seed (0.0, 1.0) k) cleanZipped
        dirtyCounts = cleanCounts ++ [ (x+1,0) | x <- [0..length list - 1], list !! x == 0]
    in sortBy (comparing fst) dirtyCounts

{- 
for a random generator 0: (1,87), (2,212), (3,59), (4,141), (5,0), (6,119), (7,70), (8,46), (9,0), (10,102), (11,164)
with code used: roulette 0 listProb 1000
-}

-- EXERCISE D

-- crossover takes two lists and a place where to cut and returns the results of performing crossover into a list.
crossover :: [a] -> [a] -> Int -> [[a]]
crossover p1 p2 cut = let
        (p11, p12) = splitAt cut p1
        (p21, p22) = splitAt cut p2
    in [p11 ++ p22, p21 ++ p12]


-- EXERCISE E

-- mutationBinary takes a seed, a mutation parameter p and a binary string and flips each bit in the string randomly with 
-- probability p where the randomness is determined by the seed in the input.
mutationBinary :: (Fractional a, Ord a, Random a) => Int -> a -> [Char] -> [Char]
mutationBinary seed p string = let
        coinflips = randomRange seed (0.0, 1.0) (length string)
    in mutation' coinflips string where
        mutation' _ [] = []
        mutation' (coinflip:restflips) ('0':restbits) = if coinflip < p
            then '1': mutation' restflips restbits
            else '0': mutation' restflips restbits
        mutation' (coinflip:restflips) ('1':restbits) = if coinflip < p
            then '0': mutation' restflips restbits
            else '1': mutation' restflips restbits
        mutation' (coinflip:restflips) (b:restbits) = error "mutationBinary ERROR: no binary string as input"


-- EXERCISE F

-- currify takes a list and pairs up every two elements.
currify :: [a] -> [(a,a)]
currify [] = []
currify [_] = error "currify ERROR: not an even number in population!"
currify (x:y:xs) = (x,y) : currify xs

-- currifyTriple takes a list and pairs up every three elements
currifyTriple :: [a] -> [(a,a,a)]
currifyTriple [] = []
currifyTriple [_] = error "currifyTriple ERROR: not a multiple of three in population"
currifyTriple [_,_] = error "currifyTriple ERROR: not a multiple of three in population"
currifyTriple (x:y:z:xs) = (x,y,z) : currifyTriple xs

-- currifySkipThrd takes a list and pairs up every two elements while forgetting the third
currifySkipThrd :: [a] -> [(a,a)]
currifySkipThrd [] = []
currifySkipThrd [_] = error "currifyTriple ERROR: not a multiple of three in population"
currifySkipThrd [_,_] = error "currifyTriple ERROR: not a multiple of three in population"
currifySkipThrd (x:y:z:xs) = (x,y) : currifySkipThrd xs

-- some populations for experimenting
population, smallpopulation :: [String]
smallpopulation = ["11111","00000"]
population = replicate 100 "11100" ++ replicate 100 "00011"

{--- matingpool takes as input a seed, a population and a population size k and creates a mating pool of size k out of
-- the population uniformly, where the randomness comes from the given seed.
matingpool :: Int -> [a] -> Int  -> [(a,a)]
matingpool seed pop size = let
        coinflips = randomRange seed (0, length pop - 1) size
    in currify $ extractMates pop coinflips where
        extractMates pop [] = []
        extractMates pop (i:rest) = pop !! i : extractMates pop rest
-}

-- evolveNoMut takes a seed, a population, a population size, the maximum nr of generations and the size of the bitstrings in
-- the populations. Using the given seed, for each generation, it creates a mating pool of the population size and then performs
-- crossover on the members of this mating pool. It does this for every generation, until it has done it for the maximum number
-- of generations.
evolveNoMut :: Int -> [[Char]] -> Int -> Int -> Int -> [[Char]]
evolveNoMut seed pop gensize nrgen orgsize = let
        seedsgen = currifySkipThrd $ map (`mod` 10000) (take (3*nrgen) (randoms $ mkStdGen seed :: [Int]))
    in evolve' seedsgen pop nrgen where
        evolve' _ pop 0 = pop
        evolve' ((s1,s2):seeds) pop k = let
                pool = matingpool s1 pop gensize
                seedscross = randomRange s2 (1,orgsize) gensize
            in evolve' seeds (reproduction seedscross pool) (k-1) where
                reproduction _ [] = []
                reproduction (s:srest) ((a,b):prest)  = (crossover a b s) ++ (reproduction srest prest)

-- evolve does exactly the same as evolveNoMut, except that it also takes a mutation parameter as input with which, after crossover,
-- it performs mutation with the given parameter on each member of the population.
evolve :: (Fractional a, Ord a, Random a) => 
                Int -> [[Char]] -> Int -> Int -> a -> Int -> [[Char]]
evolve seed pop gensize nrgen mpar orgsize = let
        seedsgen = currifyTriple $ map (`mod` 10000) (take (3*nrgen) (randoms $ mkStdGen seed :: [Int]))
    in evolve' seedsgen pop nrgen where
        evolve' _ pop 0 = pop
        evolve' ((s1,s2,s3):seeds) pop k = let
                pool = matingpool s1 pop gensize
                seedscross = randomRange s2 (1,orgsize) gensize
                seedsmut = map (`mod` 10000) (take gensize (randoms $ mkStdGen s3 :: [Int]))
            in evolve' seeds (mutate seedsmut (reproduction seedscross pool)) (k-1) where
                reproduction _ [] = []
                reproduction (s:srest) ((a,b):prest)  = (crossover a b s) ++ (reproduction srest prest)
                mutate _ [] = []
                mutate (s:srest) (o:orest) = mutationBinary s mpar o : mutate srest orest


--------------------------------------------------------- CHAPTER 2 -----------------------------------------------------------------

-- To be able to do some of the exercises from chapter 2, we will adapt our matingpool function to cope with a fitness function.

fitness :: String -> Float
fitness ('0':_) = 1
fitness ('1':_) = 10

-- this version of matingpool picks a mating pool of a given size from a given population using the given seed.
matingpool :: Int -> [String] -> Int  -> [(String,String)]
matingpool seed pop size = let
        fitnesspop  = map fitness pop                           -- the fitness for each member of the population
        totalfit    = sum fitnesspop                            -- the total fitness of the population
        fitProb     = map (\f -> 1 - f/totalfit) fitnesspop     -- the probabilities of each member when closer to 0 is better
--      fitProb     = map (\f -> f/totalfit) fitnesspop         -- the probabilities of each member when further away from 0 is better
        popInterval = zip pop (cumList fitProb)                 -- the population zipped with its corresponding interval
        coinflips   = randomRange seed (0.0,1.0) size           -- coinflips to determine the mating pool
    in currify $ findStrings coinflips popInterval where
        findStrings [] _ = []
        findStrings (f:rest) popInterval = findStrings' f popInterval : findStrings rest popInterval where
            findStrings' f [] = []
            findStrings' f ((string,(a,b)):rest) = if b == 1
                then if a <= f && f <= b
                    then string
                    else findStrings' f rest
                else if a <= f && f < b
                    then string
                    else findStrings' f rest