-- | A functioning Genetic Algorithm
-- | Daan van Stigt 17/01/2016


import System.Random
import Data.List (sort,maximumBy,group)
import Data.Ord (comparing)
import Data.List.Split (chunksOf)


--
-- SOME GENERAL FUNCTIONS
--

-- returns a list of n random floats in the unit interval. Here and in the rest of the code 'seed' is the required integer for
-- production of random numbers.
randomNums :: Int -> Int -> [Float]
randomNums seed n = take n $ randomRs (0.0,1.0)(mkStdGen seed)

-- returns a list of n random integers in the interval [1,k]
randomInts :: Int -> Int -> Int -> [Int]
randomInts seed n k = take n $ randomRs (1,k)(mkStdGen seed)

-- e.g. converts [1,0,1] to 5
binaryToBaseTen :: [Int] -> Int
binaryToBaseTen binaryList = sum $ zipWith (*) binaryList' [2^x | x <- [0,1..]]
    where binaryList' = reverse binaryList

-- takes representation [[string]] of the population and returns representation [(string, occurences)]
package :: [[Int]] -> [([Int],Int)]
package list = map (\l@(x:xs) -> (x,length l)) . group . sort $ list

-- reverses package
unpack :: [([Int],Int)] -> [[Int]]
unpack list = concat . map (\(x,y) -> replicate y x) $ list


-- from a list of probabilities make a partition of the unit interval consisting of intervals (a,b)
-- NB: you must add 0 to the beginning of your probability list! sorry
makeIntervals :: [Float] -> [(Float,Float)]
makeIntervals [a,b] = [(a,a+b)]
makeIntervals (a:b:rest) = (a,a+b) : makeIntervals (a+b:rest)
-- Example when list of probabilities is [0.3,0.5,0.2]:
--  > makeIntervals [0,0.3,0.5,0.2]
--  > [(0.0,0.3),(0.3,0.8),(0.8,1.0)]

-- converts the population from [(binary string, occurrences)] tot [(decimal number, occurrences)]
-- used for ease of reading output in real function optimization (see example fitnessFunction)
convertPopulation :: [([Int],Int)] -> [(Int,Int)]
convertPopulation l = map (\(x,y)-> (binaryToBaseTen x,y)) l

-- picks out the entity with most occurences
bestEntity :: [(a,Int)] -> (a,Int)
bestEntity population = maximumBy (comparing snd) population



--
-- HElPER FUNCTIONS FOR EVOLUTION
--

-- matingPool takes a population (repr. [(string, # of occurences)]) and a fitness function f (f :: [Int] -> Int) and a list of random numbers (using randomNums!),
-- the length of which is the desired size of the next generation. Note: the length of this list must be even!
-- matingPool returns a list of random orde with strings selected w.r.t to fitness for reproduction. 
matingPool :: [Float] -> [([Int],Int)] -> ([Int] -> Int) -> [[Int]]
matingPool [] _ _ = []
matingPool (r:rs) population f = (findString r) : matingPool rs population f 
    where populationFitness = map (\(x,y) -> y * (f x)) population
          totalFitness = sum $ populationFitness
          populationProbs = map (\x -> (fromIntegral x / fromIntegral totalFitness)) populationFitness
          populationWithIntervals = zip population (makeIntervals $ (0 : populationProbs))
          findString r = fst $ fst $ head [(x,(a,b)) | (x,(a,b)) <- populationWithIntervals, a <= r, r < b]

--Straighforward: given two strings and a cutting site, crossover produces a list with the two crossed strings.
stringCrossover :: Int -> [Int] -> [Int] -> [[Int]]
stringCrossover site string1 string2 =
    let (a,b) = splitAt site string1 
        (c,d) = splitAt site string2
    in [a++d,c++b]

-- takes a list of random integers between 1 and length.of.strings-1. The length of the list is number.of.strings/2
crossover :: [Int] -> [[Int]] -> [[Int]]
crossover _ [] = []
crossover (site:restOfSites) (string1:string2:restOfStrings) = (stringCrossover site string1 string2) ++ crossover restOfSites restOfStrings

-- mutation performs independent 'flip' mutation on binary strings with flip probability 'prob'. 
-- Representation for binary strings is a list of 0's and 1's, i.e. [1,0,0,1] represents 1001
stringMutation :: Int -> [Int] -> Float -> [Int]
stringMutation seed string prob = zipWith (\x y -> (x+y) `mod` 2) string indices
    where randomnums = randomNums seed (length string)
          indices = map (\x -> if x<prob then 1 else 0) randomnums

-- takes a list of seeds (random integers) with length same as size of population
mutation :: [Int] -> [[Int]] -> Float -> [[Int]]
mutation _ [] _ = []
mutation (seed:restOfSeeds) (string:restOfStrings) prob = (stringMutation seed string prob) : mutation restOfSeeds restOfStrings prob


--
-- CENTRAL FUNCTIONS
--

-- create a random population
population :: Int -> Int -> Int -> [([Int],Int)]
population seed stringLength size = package strings
    where randomnums = randomNums seed (stringLength * size)
          indices = map (\x -> if x<0.5 then 1 else 0) randomnums
          strings = chunksOf stringLength indices

-- maximize the function 100*sin(x/10.0) 
fitnessFunction :: [Int] -> Int
fitnessFunction string = f(x)^3 -- we scale the function for better selection
    where x = fromIntegral $ binaryToBaseTen string
          f(x) = floor $ 100*sin(x/10.0) -- the actual function to optimize

-- perform evolution
evolution :: [Int] -> [([Int],Int)] -> ([Int] -> Int) -> Float -> Int -> [([Int],Int)]
evolution (seed:restOfSeeds) population f p gens
    | gens <= 1 = newPopulation
    | otherwise = evolution restOfSeeds newPopulation f p (gens-1)
    where popSize = sum $ map snd population
          matRands = randomNums seed popSize
          crossRands = randomInts seed (floor(fromIntegral popSize / 2.0)) (popSize-1)
          mutRands = randomInts seed popSize 10000
          pooled = matingPool matRands population f 
          crossed = crossover crossRands pooled
          mutated = mutation mutRands crossed p
          newPopulation = package mutated

-- does not cross and mutate the last population; only does selection.
evolution' :: [Int] -> [([Int],Int)] -> ([Int] -> Int) -> Float -> Int -> [([Int],Int)]
evolution' (seed:restOfSeeds) population f p gens
    | gens <= 1 = package pooled  -- you can see that here
    | otherwise = evolution restOfSeeds newPopulation f p (gens-1)
    where popSize = sum $ map snd population
          matRands = randomNums seed popSize
          crossRands = randomInts seed (floor(fromIntegral popSize / 2.0)) (popSize-1)
          mutRands = randomInts seed popSize 10000
          pooled = matingPool matRands population f 
          crossed = crossover crossRands pooled
          mutated = mutation mutRands crossed p
          newPopulation = package mutated

-- example call:
-- evolution evolutionSeeds population' fitnessFunction 0.01 10


--
-- SOME SETTINGS TO RUN ON
--

-- define your population
-- NB: population size MUST be even (otherwise crossover fails)
-- example population
population' :: [([Int],Int)]
population' = population 0 7 100


evolutionSeeds :: [Int]
evolutionSeeds = randomInts 0 1000 100000



--
-- SOME TRIALS ON A RANDOM POPULATION
--

-- function to optimize is 100*sin(x/10) over integer interval [0,127] 
-- encoded as binary strings of length 7 (1111111 base 2 is 127 base 10)
-- maxima of the function are around 14,15,16, and 77,78,79

-- *Main> bestEntity $ convertPopulation population'
-- (37,6)
-- *Main> bestEntity $ convertPopulation $ evolution evolutionSeeds population' fitnessFunction 0.01 1
-- (10,32)
-- *Main> bestEntity $ convertPopulation $ evolution evolutionSeeds population' fitnessFunction 0.01 2
-- (12,34)
-- *Main> bestEntity $ convertPopulation $ evolution evolutionSeeds population' fitnessFunction 0.01 5
-- (12,50)
-- *Main> bestEntity $ convertPopulation $ evolution evolutionSeeds population' fitnessFunction 0.01 10
-- (14,46)
-- *Main> bestEntity $ convertPopulation $ evolution evolutionSeeds population' fitnessFunction 0.01 30
-- (14,60)
-- *Main> bestEntity $ convertPopulation $ evolution evolutionSeeds population' fitnessFunction 0.01 50
-- (14,56)
-- *Main> bestEntity $ convertPopulation $ evolution evolutionSeeds population' fitnessFunction 0.01 100
-- (14,22)
-- *Main> bestEntity $ convertPopulation $ evolution evolutionSeeds population' fitnessFunction 0.01 200
-- (79,44)
-- *Main> bestEntity $ convertPopulation $ evolution evolutionSeeds population' fitnessFunction 0.01 300
-- (78,50)
-- *Main> bestEntity $ convertPopulation $ evolution evolutionSeeds population' fitnessFunction 0.01 400
-- (15,60)

-- we move initially towards first peak and then towards second peak, before returning to the first again.


