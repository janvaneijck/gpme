-- Daan van Stigt
-- Computer Assignments for Chapter 1 of Goldberg 1989

import System.Random

-- Exercise A

-- Returns a list of n random floats in the unit interval. Here and in the rest of the code 'seed' is the required integer for
-- production of random numbers.
randomNums :: Int -> Int -> [Float]
randomNums seed n = take n $ randomRs (0.0,1.0)(mkStdGen seed)

-- See the function rouletteCount for a more general implementation of this function.
quartileCount :: [Float] -> [(Float,Int)]
quartileCount xs =
    let q1 = [x | x <- xs, x < 0.25]
        q2 = [x | x <- xs, x >= 0.25, x < 0.5]
        q3 = [x | x <- xs, x >= 0.5, x < 0.75]
        q4 = [x | x <- xs, x >= 0.75, x <= 1.0]
    in [(0.25, length q1),(0.5, length q2),(0.75, length q3),(1.0, length q4)]

-- Exercise B

probs :: [Float]
probs = [0.1,0.2,0.05,0.15,0.0,0.11,0.07,0.04,0.0,0.12,0.16]

-- quantileList takes a list of probabilities that sum to 1, and returns a numbered partition of the unit interval.
-- Representation: [(1,0.0),(2,0.3),(3,0.4),(4,1.0)] is the numbered partition of the unit interval based on the 
-- list of probabilities [0.3,0.1,0.6] corresponding to strings 1, 2, and 3.
quantileList :: [Float] -> [(Int,Float)]
quantileList probs = zip [1..] $ scanl (+) 0 probs

-- rouletteCount takes a list of random numbers in the unit interval ('spins' of the roulette wheel) and a numbered partition
-- of the unit interval ('slots'). The numbered partition must use the same representation 
-- as the output of quantileList. rouletteCount counts how many of the spins ended in each slot.
rouletteCount :: [Float] -> [(Int,Float)] -> [(Int,Int)]
rouletteCount spins [a,b] = [(fst a, length [x | x <- spins, x >= snd a, x <= snd b])]
rouletteCount spins quantiles = (fst a, length [x | x <- spins, x >= snd a, x < snd b]) : rouletteCount spins (b:rest) 
    where a:b:rest = quantiles

-- roulette spins the custom wheel, which is based on the list of probabilities, n times and calls rouletteCount to do the counting.  
roulette :: Int -> [Float] -> Int -> [(Int,Int)]
roulette seed probs n = rouletteCount spins quantiles
    where spins = randomNums seed n
          quantiles = quantileList probs

-- Exercise D

--Straighforward: given two strings and a cutting site, crossover produces a list with the two crossed strings.
crossover :: (Integral a) => Int -> [a] -> [a] -> [[a]]
crossover site string1 string2 =
    let (a,b) = splitAt site string1 
        (c,d) = splitAt site string2
    in [a++d,b++c]

-- Exercise E

-- mutation performs independent 'flip' mutation on binary strings with flip probability 'prob'. 
-- Representation for binary strings is a list of 0's and 1's, i.e. [1,0,0,1] represents 1001
mutation :: Int -> [Int] -> Float -> [Int]
mutation seed list prob = zipWith (\x y -> (x+y) `mod` 2) list indices
    where randomnums = randomNums seed (length list)
          indices = map (\x -> if x<prob then 1 else 0) randomnums

-- Exercise F

population :: [[Int]]
population = replicate 100 [1,1,1,0,0] ++ replicate 100 [0,0,0,1,1]

--evolution :: still in the making




