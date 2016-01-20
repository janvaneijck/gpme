module GPME_mfga_v2clone where
import System.Random

-- O.F. Tuyt & P.W.B. Michgelsen


-- Introduction:
-- Please find below a genetic algorithm according to the principles of the beginning
--  chapters of ``Goldberg, D.E., (1989), “Genetic Algorithms in Search, Optimization,
--  and Machine Learning”, Addison-Wesley Publishing Company".

-- Important Remark:
-- By the order of a fitness function, i.e. '>' or '<', we mean the following
    -- If '>', then further from 0 is fitter;
    -- If '<', then closer to 0 is fitter.

    
    
-- Types: ---------------------------------------------------

type Seed = Int
type Frequency = Int
type Bit = Char 
type Bitstring = [Bit]
type Population = [(Bitstring, Frequency)]



-- Help Functions: --------------------------------------------------------------------

-- randomRange takes a seed, an interval and a quantity n and returns n value
--  uniformly picked from the interval using the seed.
randomRange :: Random a => Seed -> (a, a) -> Int -> [a]
randomRange seed (a,b) k = take k $ randomRs (a,b) (mkStdGen seed)

-- Generalisation of 'div' to any instance of Real (from Data.Fixed)
div' :: (Real a,Integral b) => a -> a -> b
div' n d = floor ((toRational n) / (toRational d))

-- Generalisation of 'mod' to any instance of Real (from Data.Fixed)
mod' :: (Real a) => a -> a -> a
mod' n d = n - (fromInteger f) * d where
    f = div' n d

-- pairUp takes a list and pairs up every two elements.
pairUp :: [a] -> [(a,a)]
pairUp [] = []
pairUp [_] = error "pairUp ERROR: not an even number in population/generation!"
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
bitstringtofloat :: Bitstring -> Float
bitstringtofloat xs = bitstringtofloat' xs (length xs) where
   bitstringtofloat' [] _ = 0
   bitstringtofloat' (x:xs) count = (bittofloat x)*2^(count-1) +
                                    (bitstringtofloat' xs (count-1))
   bittofloat x = if x == '1' then 1.0 else 0.0    
    


-- Genetic Algorithn: -----------------------------------------------------------------

-- crossover takes a seed and two lists, transforms the seed to a cut off point 
--  and returns the results of performing crossover into a list.    
crossover :: Int -> [a] -> [a] -> [[a]]
crossover randomnr p1 p2 = let
        maxSize = max (length p1) (length p2)
        cut = randomnr `mod` (maxSize - 1) + 1
        (p11, p12) = splitAt cut p1
        (p21, p22) = splitAt cut p2
    in [p11 ++ p22, p21 ++ p12]

-- mutationBinary takes a seed, a mutation parameter p and a binary string and flips 
--  each bit in the string randomly with probability p where the randomness is 
--  determined by the seed in the input.
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
        mutation' (coinflip:restflips) (b:restbits) = 
                error "mutationBinary ERROR: no binary string as input"

-- The function reproduciton performs the reproduction using a roulette wheel
--  reproduction technique.
reproduction :: (Eq t, Fractional a2, Integral b,
                 Num a, Num a1, Ord a2, Random a2) => Seed               
                                                      -- seed
                                                   -> [(t, b)]          
                                                      -- population
                                                   -> Int               
                                                      -- bound on generation size
                                                   -> (t -> a2)         
                                                      -- fitness function
                                                   -> (a -> a1 -> Bool) 
                                                      -- order of fitness function*
                                                   -> [t]
reproduction seed pop size fitnessfunction order = let
        fitnesspop  = zip pop (map (fitnessfunction . fst) pop )            
        perfectlist = (filter (\((x,y),z) -> z == 0.0) fitnesspop)     
        in if perfectlist /= [] && order 0 1
            then [fst $ fst $ head perfectlist]
            else let
                    totalfit    = if order 0 1
                                    then sum $ map 
                                        (\((x, freq), fit) -> fromIntegral freq / fit )
                                        fitnesspop           
                                    else sum $ map 
                                        (\((x, freq), fit) -> fromIntegral freq * fit )
                                        fitnesspop           
                    fitProb     = if order 0 1
                                    then map 
                                         (\ ((x, freq),fit) -> 
                                            fromIntegral freq / (fit * totalfit) )
                                         fitnesspop 
                                    else map 
                                         (\ ((x, freq),fit) ->
                                            fromIntegral freq * fit / totalfit )
                                         fitnesspop     
                    popInterval = zip (map fst pop) (cumList fitProb)
                    coinflips   = randomRange seed (0.0,1.0) size
                in findStrings coinflips popInterval where
                    findStrings [] _ = []
                    findStrings (flip:rest) popInterval = 
                        findStrings' flip popInterval : findStrings rest popInterval
                        where
                            findStrings' flip ((string,(a,b)):rest) = if b == 1
                                then if a <= flip && flip <= b
                                        then string
                                        else findStrings' flip rest
                                else if a <= flip && flip < b
                                        then string
                                        else findStrings' flip rest

-- The function evolve prefors the genetic algorithm.
evolve :: (Fractional a, Ord a, Random a) => Seed
                                             -- seed
                                          -> Population
                                             -- sample of search space
                                          -> Int           
                                             -- bound on generation size
                                          -> Int  
                                             -- maximum number of generations
                                          -> Float  
                                             -- crossover parameter
                                          -> Float  
                                             -- mutation parameter
                                          -> (Bitstring -> a)         
                                             -- fitness function
                                          -> (Int -> Int -> Bool) 
                                             -- fitness order
                                          -> Population
                                             -- Population after evolution
evolve seed pop gensize nrgen cpar mpar fitnessfunction order = let
        seedsgen = map (`mod` 10000) (take nrgen (randoms $ mkStdGen seed :: [Int]))
    in evolve' seedsgen pop nrgen where
        evolve' _ pop 0 = pop
        evolve' _ [(perfectentity,0)] k = 
          [("Perfect entity found, " ++
           perfectentity ++
           ", in generation",nrgen - k - 1)]
        evolve' (s:seeds) pop k =
          evolve' seeds (createGen s pop gensize cpar mpar fitnessfunction order) (k-1)
          
-- The function evolveVerbose prefors the genetic algorithm verbosely.
evolveVerbose :: (Fractional a, Integral b,
                  Ord a, Show b, Random a) => Seed
                                              -- seed
                                           -> Population
                                              -- sample of search space
                                           -> Int 
                                              -- bound on generation size
                                           -> Int 
                                              -- maximum number of generations
                                           -> Float 
                                              -- crossover parameter
                                           -> Float
                                              -- mutation paramter
                                           -> (Bitstring -> a) 
                                              -- fitness function
                                           -> (Int -> Int -> Bool)
                                              -- fitness order
                                           -> IO ()
                                              -- population after evolution or perfect
                                              --  entity if found
evolveVerbose seed pop gensize nrgen cpar mpar fitnessfunction order = let 
        seedsgen = map (`mod` 10000) (take nrgen (randoms $ mkStdGen seed :: [Int]))
    in evolve' seedsgen pop nrgen where
        evolve' _ pop 0 = do putStrLn $
                                "Generation " ++ 
                                (show (nrgen)) ++
                                ": " ++
                                (show pop)
        evolve' _ [(perfectentity,0)] k = do putStrLn $
                                                "Perfect entity " ++ 
                                                perfectentity ++ 
                                                " found in generation " ++
                                                (show (nrgen - k - 1))
        evolve' (s:seeds) pop k = do putStrLn $
                                        "Generation " ++
                                        (show (nrgen - k)) ++
                                        ": " ++
                                        (show pop)
                                     let newGen = createGen s pop gensize cpar mpar 
                                                  fitnessfunction order          
                                     evolve' seeds newGen (k-1)

-- The function createGen creates a new generation from a given population, by 
--  performing reproduction, mutation and crossover.
createGen :: (Fractional a, 
              Integral b, Ord a, Random a) => Seed
                                              -- seed
                                           -> Population
                                              -- population (old generation)
                                           -> Int 
                                              -- bound on generation size
                                           -> Float
                                              -- crossover parameter
                                           -> Float
                                              -- mutation parameter
                                           -> (Bitstring -> a)
                                              -- fitness function
                                           -> (Int -> Int -> Bool)
                                              -- fitness order
                                           -> Population
createGen seed pop gensize cpar mpar fitnessfunction order = let
        [(seedPool, seedCross, seedMut)] = tripleUp $ map 
                                           (`mod` 10000)
                                           (take 3 (randoms $ mkStdGen seed :: [Int]))        
        pool = reproduction seedPool pop gensize fitnessfunction order
    in if length pool == 1 && fitnessfunction (head pool) == 0.0 && order 0 1
            then [(head pool,0)]
            else let        
                    sizecrossoverpool = round $ (fromIntegral gensize)*cpar - 
                                                (mod' ((fromIntegral gensize)*cpar) 2)
                    crossoverpool = pairUp $ take sizecrossoverpool pool
                    clonepool = drop sizecrossoverpool pool
                    seedscross = take gensize (randoms $ mkStdGen seedCross)
                    seedsmut = map (`mod` 10000) 
                                   (take gensize (randoms $ mkStdGen seedMut :: [Int]))
                in freqRep $ mutate seedsmut $ 
                        (crossover' seedscross crossoverpool) ++ clonepool 
                        where
                         crossover' _ [] = []
                         crossover' (s:srest) ((a,b):prest) = (crossover s a b) ++
                                                              (crossover' srest prest)
                         mutate _ [] = []
                         mutate (s:srest) (o:orest) = 
                            mutationBinary s (mpar :: Float) o : mutate srest orest

-- The function freqRep represents a population of individual organisms as pairs
-- (organism type, frequency) similar to the type Population.
freqRep :: (Eq a, Num t) => [a] -> [(a, t)]
freqRep [] = []
freqRep (org:rest) = (org, (count org rest) + 1) :
                      freqRep (filter (\x -> x /= org) rest)
                        where
                            count _ [] = 0
                            count x (y:ys)
                                       | x == y = 1 + (count x ys)
                                       | otherwise = count x ys

                                       
                                       
-- Using the Genetic Algorithn: -------------------------------------------------------

-- The search space are bitstrings of length 5; populationx is a sample of this space.     
population1, population2, population3 :: Population
population1 = [("11111", 1),("00000",1)]
population2 = [("10101", 10), ("11100",10), ("00011", 10), ("00000", 10), ("00110",10)]
population3 = [("01111", 10), ("11100",10), ("00011", 10), ("11111", 10), ("00110",10)]

-- The fitness function fitnessneutral returns fitness value 1 for every organism.
fitnessneutral :: a -> Float
fitnessneutral organism = 1

-- The fitness function fitnessratio returns fitness value 1 of the bitstring begins
--  with a 0 or 10 if it begins with a 1.
fitnessratio :: Bitstring -> Float
fitnessratio ('0':_) = 1
fitnessratio ('1':_) = 10


-- The fitness function fitnessbinary returns the decimal value of bitstring to the
--  power of two. The latter is done to extend the variation in fitness values among
--  the population, s.t. the genetic algorithm preforms betters. This is called:
--  fitness scaling.
fitnessbinary :: Bitstring -> Float
fitnessbinary organism = ((bitstringtofloat organism)**2)

{- Example of use of the Genetic algorithm:
    
    Run the function: evolveVerbose 0 population3 40 100 0.5 0.001 fitnessbinary (<)
    
    This should return:
        Generation 0: [("10101",10),("11100",10),("00011",10),("00000",10),("00110",10)]
        Generation 1: [("10100",2),("11101",2),("10101",12),("11100",24)]
        Generation 2: [("11100",32),("10100",3),("11101",3),("10101",2)]
        Generation 3: [("11100",35),("10100",3),("11101",1),("10101",1)]
        Generation 4: [("11100",35),("10100",2),("11101",3)]
        Generation 5: [("11100",31),("11101",7),("10100",2)]
        Generation 6: [("11101",7),("11100",33)]
        Generation 7: [("11100",34),("11101",6)]
        Generation 8: [("11100",28),("11101",12)]
        Generation 9: [("11101",18),("11100",22)]
        Generation 10: [("11101",16),("11100",23),("01100",1)]
        ...
        Generation 95: [("11111",33),("11101",7)]
        Generation 96: [("11101",5),("11111",35)]
        Generation 97: [("11111",36),("11101",4)]
        Generation 98: [("11111",34),("11101",6)]
        Generation 99: [("11111",38),("11101",2)]
        Generation 100: [("11111",38),("11101",2)]

    Run the function: evolveVerbose 0 population3 40 100 0.5 0.001 fitnessbinary (<)
    
    This should return:
        Generation 0: [("01111",10),("11100",10),("00011",10),("11111",10),("00110",10)]
        Generation 1: [("00011",32),("00110",6),("00111",1),("00010",1)]
        Generation 2: [("00011",33),("00010",5),("00111",1),("00110",1)]
        Generation 3: [("00011",33),("00010",6),("00111",1)]
        Generation 4: [("00011",31),("00010",9)]
        Generation 5: [("00011",17),("00010",23)]
        Generation 6: [("00010",34),("00011",6)]
        Generation 7: [("00010",35),("00011",5)]
        Generation 8: [("00010",39),("00011",1)]
        Generation 9: [("00010",39),("00011",1)]
        Generation 10: [("00010",39),("10010",1)]
        Generation 11: [("00010",39),("10010",1)]
        Generation 12: [("00010",40)]
        Generation 13: [("00010",40)]
        Generation 14: [("00010",40)]
        Generation 15: [("00010",40)]
        Generation 16: [("00010",39),("10010",1)]
        Generation 17: [("00010",38),("00110",1),("00000",1)]
        Perfect entity 00000 found in generation 17

-} 