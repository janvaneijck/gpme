module GPME_mfga_v1 where
import System.Random

-- O.F. Tuyt & P.W.B. Michgelsen


-- My First Genetic Algorithm:
-- Please find below a very simple genetic algorithm to return the fittest bitstring
--  in a population of bitstrings upto a certain length. The fitness of a bitstring
--  is given by its decimal value to the power of two. How to use this algorithm is 
--  given below.



-- Types: ---------------------------------------------------

type Seed = Int
type Bit = Char 
type Bitstring = [Bit]
type Population = [Bitstring]



-- Help Functions: --------------------------------------------------------------------

-- randomRange takes a seed, an interval and a quantity n and returns n value
--  uniformly picked from the interval using the seed.
randomRange :: Random a => Seed -> (a, a) -> Int -> [a]
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

-- cumList converts a list of probabilities (summing up to 1) into a list of 
--  intervals corresponding to the probabilities
cumList :: Fractional t => [t] -> [(t, t)]
cumList [] = []
cumList (p:rest) = (0.0,p) : cumList' p rest where
    cumList' _ [] = []
    cumList' p (q:rest) = (p,p+q) : cumList' (p+q) rest

-- bitstringtofloat converts a bitstring to its corresponding decimal number
bitstringtofloat :: [Char] -> Float
bitstringtofloat xs = bitstringtofloat' xs (length xs) where
   bitstringtofloat' [] _ = 0
   bitstringtofloat' (x:xs) count = (bittofloat x)*2^(count-1) + (bitstringtofloat' xs (count-1))
   bittofloat x = if x == '1' then 1.0 else 0.0



-- Genetic Algorithn: -----------------------------------------------------------------

-- crossover takes a seed and two lists, transforms the seed to a cut off point 
--  and returns the results of performing crossover into a list.
crossover :: Int -> Bitstring -> Bitstring -> [Bitstring]
crossover randomnr p1 p2 = let
        maxSize = max (length p1) (length p2)
        cut = randomnr `mod` (maxSize - 1) + 1
        (p11, p12) = splitAt cut p1
        (p21, p22) = splitAt cut p2
    in [p11 ++ p22, p21 ++ p12]

-- The function mutationBinary performs the mutation of binary strings, s.t. it flips
--  a bit in a string with probability p.
mutationBinary :: (Fractional a, Ord a, Random a) => Seed
                                                  -> a 
                                                  -> Bitstring 
                                                  -> Bitstring
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
        mutation' (coinflip:restflips) (b:restbits) = 
                            error "mutationBinary ERROR: no binary string as input"

-- The function reproduciton performs the reproduction using a roulette wheel
--  reproduction technique.
reproduction :: Seed -> Population -> Int  -> [(Bitstring,Bitstring)]
reproduction seed pop size = let
        fitnesspop  = map fitness pop                           
        totalfit    = sum fitnesspop                            
        fitProb     = map (\f -> f/totalfit) fitnesspop         
        popInterval = zip pop (cumList fitProb)                 
        coinflips   = randomRange seed (0.0,1.0) size           
    in pairUp $ findStrings coinflips popInterval where
        findStrings [] _ = []
        findStrings (f:rest) popInterval = 
            findStrings' f popInterval : findStrings rest popInterval where
                findStrings' f ((string,(a,b)):rest) = if b == 1
                    then if a <= f && f <= b
                        then string
                        else findStrings' f rest
                    else if a <= f && f < b
                        then string
                        else findStrings' f rest

-- The function evolve is the function preforming the genetic algorithm.
evolve :: (Fractional a, Ord a, Random a) => Seed        -- Seed to created randomness
                                          -> Population  -- Sample of search space
                                          -> Int         -- Bound on generation size
                                          -> Int         -- Bound on nr of reproduction
                                          -> a           -- Probability mutation
                                          -> [[Char]]    -- Population after evolution
evolve seed pop gensize nrgen mpar = let
        seedsgen = map (`mod` 10000) (take (3*nrgen) (randoms $ mkStdGen seed ::[Int]))
        seedstriples = tripleUp seedsgen
    in evolve' seedstriples pop nrgen where
        evolve' _ pop 0 = pop
        evolve' ((s1,s2,s3):seeds) pop k = let
                crossoverpool = reproduction s1 pop gensize
                seedscross = take gensize (randoms $ mkStdGen s2)
                seedsmut = map (`mod` 10000) 
                               (take gensize (randoms $ mkStdGen s3 :: [Int]))
            in evolve' seeds 
                       (mutate seedsmut (performcrossover seedscross crossoverpool))
                       (k-1) 
                        where
                        performcrossover _ [] = []
                        performcrossover (s:srest) ((a,b):prest) = 
                            (crossover s a b) ++ (performcrossover srest prest)
                        mutate _ [] = []
                        mutate (s:srest) (o:orest) = 
                            mutationBinary s mpar o : mutate srest orest

                            
                         
-- Using the Genetic Algorithn: -------------------------------------------------------

-- The search space are bitstrings of length 5; population is a sample of this space.                                    
population :: Population
population =    replicate 10 "10101" ++
                replicate 10 "11100" ++
                replicate 10 "00011" ++
                replicate 10 "00000" ++
                replicate 10 "00110"

-- This fittest-high fitness function returns the decimal value of bitstring to the
--  power of two. The latter is done to extend the variation in fitness values among
--  the population, s.t. the genetic algorithm preforms betters. This is called:
--  fitness scaling.
fitness :: Bitstring -> Float
fitness string = (bitstringtofloat string)**2

{- Example of use of the Genetic algorithm:

    Run the function: evolve 0 population 50 700 0.01
    
    This should return:
    ["11100","11101","11110","11110","11111","11110","11111","11111","11111","11110",
     "11101","11101","11101","11110","11110","11111","11100","11110","11111","11101",
     "11100","11111","11100","11101","11111","11110","11111","11100","11111","11111",
     "11111","11101","11101","11111","11101","11111","11111","11111","11111","11110",
     "11111","11101","11110","11111","11111","11101","11101","11101","11111","11110"]

-} 