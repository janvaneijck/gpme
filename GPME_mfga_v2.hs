module GPME_mfga_v2 where
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


fitness :: (a,Int) -> Float
fitness (organism, _) = 1

-- this version of reproduction picks a reproduction result of a given size from a given population using the given seed.
--reproduction :: Int -> [(a, Int)] -> Int  -> [(a,a)]
reproduction seed pop size = let
        fitnesspop  = zip pop (map (fitness . fst) pop )                             -- gives a list [((organismtype, frequency), fitness)]
        totalfit    = sum $ map (\((x, freq), fit) -> freq * fit ) fitnesspop        -- the total fitness of the population
        fitProb     = map (\ ((x, freq),fit) -> freq * fit / totalfit ) fitnesspop   -- gives a list of probabilities for each organismtype, ordered as in pop.        
        popInterval = zip (map fst pop) (cumList fitProb)                            -- gives a list [(organismtype, interval)]
        coinflips   = randomRange seed (0.0,1.0) size                                -- coinflips to determine the reproduction result
    in pairUp $ findStrings coinflips popInterval where
        findStrings [] _ = []
        findStrings (flip:rest) popInterval = findStrings' flip popInterval : findStrings rest popInterval where
            findStrings' flip ((string,(a,b)):rest) = if b == 1
                then if a <= flip && flip <= b
                    then string
                    else findStrings' flip rest
                else if a <= flip && flip < b
                    then string
                    else findStrings' flip rest

--evolve :: (Fractional a, Ord a, Random a) => Int -> [([Char],Int)] -> Int -> Int -> a -> Int -> [([Char],Int)]
evolve seed pop gensize nrgen mpar orgsize = let
        seedsgen = tripleUp $ map (`mod` 10000) (take (3*nrgen) (randoms $ mkStdGen seed :: [Int]))
    in evolve' seedsgen pop nrgen where
        evolve' _ pop 0 = pop
        evolve' ((s1,s2,s3):seeds) pop k = let
                pool = reproduction s1 pop gensize
                seedscross = randomRange s2 (1,orgsize) gensize
                seedsmut = map (`mod` 10000) (take gensize (randoms $ mkStdGen s3 :: [Int]))
            in freqRep $ evolve' seeds (mutate seedsmut (crossover' seedscross pool)) (k-1) where
                crossover' _ [] = []
                crossover' (s:srest) ((a,b):prest)  = (crossover a b s) ++ (crossover' srest prest)
                mutate _ [] = []
                mutate (s:srest) (o:orest) = mutationBinary s mpar o : mutate srest orest

--freqRep :: (Eq a, Num t) => [a] -> [(a, t)]
freqRep [] = []
freqRep (org:rest) = (org, (count org rest) + 1) : freqRep (filter (\x -> x /= org) rest) where
    count _ [] = 0
    count x (y:ys)
        | x == y = 1 + (count x ys)
        | otherwise = count x ys

-- example:
population, smallpopulation :: [String]
smallpopulation = ["11111","00000"]
population = replicate 100 "11100" ++ replicate 100 "00011"
population' = replicate 15 "11100" ++ replicate 15 "00011"


example seed nrgen = evolve seed population 200 nrgen (0 :: Float) 5
example' seed nrgen = evolve seed population' 30 nrgen (0 :: Float) 5