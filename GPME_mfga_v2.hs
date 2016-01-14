module GPME_mfga_v2 where
import System.Random

-- randomRange takes a seed, an interval and a quantity n and returns n values uniformly picked from the interval using the seed.
randomRange :: Random a => Int -> (a, a) -> Int -> [a]
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

-- cumList converts a list of probabilities (summing up to 1) into a list of intervals corresponding to the probabilities
cumList :: Fractional t => [t] -> [(t, t)]
cumList [] = []
cumList (p:rest) = (0.0,p) : cumList' p rest where
    cumList' _ [] = []
    cumList' p (q:rest) = (p,p+q) : cumList' (p+q) rest

-- crossover takes a seed and two lists, transforms the seed to a cut off point and returns the results of performing crossover into a list.
crossover :: Int -> [a] -> [a] -> [[a]]
crossover randomnr p1 p2 = let
        maxSize = max (length p1) (length p2)
        cut = randomnr `mod` (maxSize - 1) + 1
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

-- this version of reproduction picks a reproduction result of a given size from a given population using the given seed.
reproduction :: (Fractional a1, Integral b, Ord a1, Random a1) => Int -> [(a, b)] -> Int -> (a -> a1) -> [a]
reproduction seed pop size fitnessfunction = let
        fitnesspop  = zip pop (map (fitnessfunction . fst) pop )                             -- gives a list [((organismtype, frequency), fitness)]
        totalfit    = sum $ map (\((x, freq), fit) -> fromIntegral freq * fit ) fitnesspop        -- the total fitness of the population
        fitProb     = map (\ ((x, freq),fit) -> fromIntegral freq * fit / totalfit ) fitnesspop   -- gives a list of probabilities for each organismtype, ordered as in pop.        
        popInterval = zip (map fst pop) (cumList fitProb)                            -- gives a list [(organismtype, interval)]
        coinflips   = randomRange seed (0.0,1.0) size                                -- coinflips to determine the reproduction result
    in findStrings coinflips popInterval where
        findStrings [] _ = []
        findStrings (flip:rest) popInterval = findStrings' flip popInterval : findStrings rest popInterval where
            findStrings' flip ((string,(a,b)):rest) = if b == 1
                then if a <= flip && flip <= b
                    then string
                    else findStrings' flip rest
                else if a <= flip && flip < b
                    then string
                    else findStrings' flip rest

-- evolve runs the genetic algorithm
evolve :: (Fractional a, Integral b, Ord a, Random a) => Int                -- seed
                                                        -> [([Char], b)]    -- population
                                                        -> Int              -- generation size
                                                        -> Int              -- maximum number of generations
                                                        -> Float            -- crossover parameter
                                                        -> Float            -- mutation parameter
                                                        -> ([Char] -> a)    -- fitness function
                                                        -> [([Char], b)]    -- population after certain number of generations
evolve seed pop gensize nrgen cpar mpar fitnessfunction = let
        seedsgen = map (`mod` 10000) (take nrgen (randoms $ mkStdGen seed :: [Int]))
    in evolve' seedsgen pop nrgen where
        evolve' _ pop 0 = pop
        evolve' (s:seeds) pop k = evolve' seeds (createGen s pop gensize cpar mpar fitnessfunction) (k-1)

-- evolveVerbose runs the genetic algorithm verbosely
evolveVerbose :: (Fractional a, Integral b, Ord a, Show b, Random a) => Int 
                                                                        -> [([Char], b)] 
                                                                        -> Int 
                                                                        -> Int 
                                                                        -> Float 
                                                                        -> Float
                                                                        -> ([Char] -> a) 
                                                                        -> IO ()
evolveVerbose seed pop gensize nrgen cpar mpar fitnessfunction = let 
        seedsgen = map (`mod` 10000) (take nrgen (randoms $ mkStdGen seed :: [Int]))
    in evolve' seedsgen pop nrgen where
        evolve' _ pop 0 = do putStrLn $ "Generation " ++ (show (nrgen)) ++ ": " ++ (show pop)
        evolve' (s:seeds) pop k = do
            putStrLn $ "Generation " ++ (show (nrgen - k)) ++ ": " ++ (show pop)
            let newGen = createGen s pop gensize cpar mpar fitnessfunction            
            evolve' seeds newGen (k-1)


-- createGen is the heart of the genetic algorithm, creating a new generation from a given population (using the given parameters)
createGen :: (Fractional a, Integral b, Num t, Ord a, Random a) => Int
                                                                -> [([Char], b)] 
                                                                -> Int 
                                                                -> Float 
                                                                -> Float
                                                                -> ([Char] -> a) 
                                                                -> [([Char], t)]
createGen seed pop gensize cpar mpar fitnessfunction = let
        [(seedPool, seedCross, seedMut)] = tripleUp $ map (`mod` 10000) (take 3 (randoms $ mkStdGen seed :: [Int]))        
        pool = reproduction seedPool pop gensize fitnessfunction          -- base for the next generation
        sizecrossoverpool = round $ (fromIntegral gensize)*cpar - (mod' ((fromIntegral gensize)*cpar) 2)
        crossoverpool = pairUp $ take sizecrossoverpool pool
        clonepool = drop sizecrossoverpool pool
        seedscross = take gensize (randoms $ mkStdGen seedCross)
        seedsmut = map (`mod` 10000) (take gensize (randoms $ mkStdGen seedMut :: [Int]))
    in freqRep (mutate seedsmut ((crossover' seedscross crossoverpool)++clonepool)) where
        crossover' _ [] = []
        crossover' (s:srest) ((a,b):prest)  = (crossover s a b) ++ (crossover' srest prest)
        mutate _ [] = []
        mutate (s:srest) (o:orest) = mutationBinary s (mpar :: Float) o : mutate srest orest

-- freqRep represents a population of individual organisms as pairs (organism type, frequency)
freqRep :: (Eq a, Num t) => [a] -> [(a, t)]
freqRep [] = []
freqRep (org:rest) = (org, (count org rest) + 1) : freqRep (filter (\x -> x /= org) rest) where
    count _ [] = 0
    count x (y:ys)
        | x == y = 1 + (count x ys)
        | otherwise = count x ys

        
-- examples to use:
-- Run: evolveVerbose 0 mediumpopulation' 40 100 0.5 0.001 fitnessbinaryscaled

population, smallpopulation, mediumpopulation :: [(String, Int)]
population = [("11100",100), ("00011",100)]
smallpopulation = [("11111", 1),("00000",1)]
mediumpopulation = [("11111", 10), ("11100",10), ("00011", 10), ("00000", 10), ("00110",10)]
mediumpopulation' = [("01111", 10), ("11100",10), ("00011", 10), ("00000", 10), ("00110",10)]

fitnessneutral :: a -> Float
fitnessneutral organism = 1

fitnessratio :: String -> Float
fitnessratio ('0':_) = 1
fitnessratio ('1':_) = 10

fitnessbinary :: String -> Float
fitnessbinary organism = bitstringtofloat organism

fitnessbinaryscaled :: String -> Float
fitnessbinaryscaled organism = (bitstringtofloat organism)**2

bitstringtofloat :: [Char] -> Float
bitstringtofloat xs = bitstringtofloat' xs (length xs) where
   bitstringtofloat' [] _ = 0
   bitstringtofloat' (x:xs) count = (bittofloat x)*2^(count-1) + (bitstringtofloat' xs (count-1))
   bittofloat x = if x == '1' then 1.0 else 0.0
   
