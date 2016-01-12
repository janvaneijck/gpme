module GPME_mfga_v1 where
import System.Random

-- randomRange takes a seed, an interval and a quantity n and returns n values uniformly picked from the interval using the seed.
randomRange :: Random a => Int -> (a, a) -> Int -> [a]
randomRange seed (a,b) k = take k $ randomRs (a,b) (mkStdGen seed)

-- pairUp takes a list and pairs up every two elements.
pairUp :: [a] -> [(a,a)]
pairUp [] = []
pairUp [_] = error "pairUp ERROR: not an even number in population!"
pairUp (x:y:xs) = (x,y) : pairUp xs

-- tripleUp takes a list and pairs up every three elements
tripleUp :: [a] -> [(a,a,a)]
tripleUp [] = []
tripleUp [_] = error "tripleUp ERROR: not a multiple of three in population"
tripleUp [_,_] = error "tripleUp ERROR: not a multiple of three in population"
tripleUp (x:y:z:xs) = (x,y,z) : tripleUp xs

-- cumList converts a list of probabilities (summing up to 1) into a list of intervals corresponding to the probabilities
cumList :: Fractional t => [t] -> [(t, t)]
cumList [] = []
cumList (p:rest) = (0.0,p) : cumList' p rest where
    cumList' _ [] = []
    cumList' p (q:rest) = (p,p+q) : cumList' (p+q) rest

-- crossover takes two lists and a place where to cut and returns the results of performing crossover into a list.
crossover :: [a] -> [a] -> Int -> [[a]]
crossover p1 p2 cut = let
        (p11, p12) = splitAt cut p1
        (p21, p22) = splitAt cut p2
    in [p11 ++ p22, p21 ++ p12]

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


fitness :: a -> Float
fitness xs = 1

-- this version of matingpool picks a mating pool of a given size from a given population using the given seed.
matingpool :: Int -> [a] -> Int  -> [(a,a)]
matingpool seed pop size = let
        fitnesspop  = map fitness pop                           -- the fitness for each member of the population
        totalfit    = sum fitnesspop                            -- the total fitness of the population
--        fitProb     = map (\f -> (1 - f/totalfit)/(fromIntegral size - 1)) fitnesspop     -- the probabilities of each member when closer to 0 is better
        fitProb     = map (\f -> f/totalfit) fitnesspop         -- the probabilities of each member when further away from 0 is better
        popInterval = zip pop (cumList fitProb)                 -- the population zipped with its corresponding interval
        coinflips   = randomRange seed (0.0,1.0) size           -- coinflips to determine the mating pool
    in pairUp $ findStrings coinflips popInterval where
        findStrings [] _ = []
        findStrings (f:rest) popInterval = findStrings' f popInterval : findStrings rest popInterval where
            findStrings' f ((string,(a,b)):rest) = if b == 1
                then if a <= f && f <= b
                    then string
                    else findStrings' f rest
                else if a <= f && f < b
                    then string
                    else findStrings' f rest

evolve :: (Fractional a, Ord a, Random a) => Int -> [[Char]] -> Int -> Int -> a -> Int -> [[Char]]
evolve seed pop gensize nrgen mpar orgsize = let
        seedsgen = tripleUp $ map (`mod` 10000) (take (3*nrgen) (randoms $ mkStdGen seed :: [Int]))
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


-- example:
population, smallpopulation :: [String]
smallpopulation = ["11111","00000"]
population = replicate 100 "11100" ++ replicate 100 "00011"
population' = replicate 15 "11100" ++ replicate 15 "00011"


example seed nrgen = evolve seed population 200 nrgen (0 :: Float) 5
example' seed nrgen = evolve seed population' 30 nrgen (0 :: Float) 5