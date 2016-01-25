module Geneticmatching where
import PWBM_GAforGeneticMatching
import PWBM_Studentgenerator
import Data.List

-- Help Functions: --------------------------------------------------------------------

-- The function intToBitstring retuns the binary representation of a decimal number
--  as a string of '0' and '1'.
intToBitstring :: Int -> String
intToBitstring 0 = "0"
intToBitstring 1 = "1"
intToBitstring n 
                | n `mod` 2 == 0 = intToBitstring (n `div` 2) ++ "0"
                | otherwise      = intToBitstring (n `div` 2) ++ "1"

-- The function stringstolongest creates from a list of binary numbers a list of
--  bitstring s.t. each bitstring is of equal length:
--   e.g. stringtolongest ["0", "10"] = ["00", "10"]
stringstolongest :: [String] -> [String]
stringstolongest (x:xs) = let
                longest = longeststring (x:xs)
                in cleanup (x:xs) longest where
                    longeststring [] = 0
                    longeststring (x:xs) =  if (length x) > longeststring xs 
                                                then length x
                                                else longeststring xs
                    cleanup [] _ = []
                    cleanup (x:xs) k =      if length x == k
                                                then [x] ++ cleanup xs k
                                                else [cleanup' x k] ++ cleanup xs k
                        where cleanup' x k = (replicate (k - (length x)) '0') ++ x
    
                
-- Using the Genetic Schoom Allocation Algorithn: -------------------------------------

-- The list highschools is a list of pairs (school, capacity).
highschools :: [(Int,Int)]
highschools =  [(1,10),(2,10),(3,12),(4,12),(5,10),(6,15),(7,12),(8,12),(9,12),(10,15)]
       
-- The function highschoolsBinary gives a binary representation of the list of school
highschoolsBinary = zip (stringstolongest $ 
                    map intToBitstring (map fst highschools)) (map snd highschools)
                    
-- Creating the preferences of the students:
generateStudentpreferences seed = studentpreferences seed highschoolsBinary
generateStudentpreferencesNonBinary seed = studentpreferences seed highschools                    

-- Creating the population, i.e. a random sample from all possible allocations
generatePopulation seed size = let
                    seeds = generateSeeds seed size
                    in createallocations seeds where
                        createallocations [] = []
                        createallocations (s:rest) = 
                            [createallocation s] ++ (createallocations rest)

-- The function createallocation creates a single allocation.                       
createallocation seed = concat $ 
                        shuffle seed $ 
                        concat $ 
                        map (\x -> replicate 10 x) (map fst highschoolsBinary)

-- This is a list of random 40 allocations based on seed 0
allocationPopulation seed size = freqRep $ generatePopulation seed size

-- The function allocationfitness is the fitness function that determines the fitness
--  of some allocation of students to highschools based on semi-random generated
--  student preferences, s.t.
--      - higher values of allocationfitness mean fitter allocations
--      - a fitness of zero is given to allocations that do not take the capacity of
--        school into account
allocationfitness :: [Char] -> Float
allocationfitness xs = if schoolCheck xs && capacityCheck (capacityMeasure xs)  
                                                          (map snd highschoolsBinary)                                                                
                            then fromIntegral ((allocationfitness' xs 1)^2)
                            else 0.0
                            where
    allocationfitness' [] _ = 0
    allocationfitness' (b1:b2:b3:b4:rest) counter =
        ((score school prefs stnr) + (allocationfitness' rest (counter+1))) where
                stnr                    = counter
                school                  = [b1]++[b2]++[b3]++[b4]
                schools                 = map fst highschoolsBinary
                prefs                   = generateStudentpreferences 0
                score school prefs stnr = if school `elem` schools
                                            then 10 - (head $ elemIndices school $
                                                      snd ( prefs !! (stnr - 1)))
                                            else 0

-- The function schoolCheck returns True if the allocation only consits of existing
--  schools.
schoolCheck allocation = let
    schoolsA = allocationSplitup allocation
    schoolsO = map fst highschoolsBinary
   in and $ map (\x -> x `elem` schoolsO) schoolsA


-- The function capacityCheck takes as arguments (i) a list of the frequencies (i.e. 
--  number of students) of each school occuring in an allocation, and (ii) a list
--  of the maximum capacity of each highschool, and returns True if no school is over
--  its allowed capacity
capacityCheck _         []       = True
capacityCheck []        _        = True
capacityCheck (f:rest) (s:rest') = if (f <= s) then ((f <= s) && 
                                                    capacityCheck rest rest')
                                                else False

-- The function capacityMeasure returns a list of the frequencies (i.e. number of
-- students) of each school occuring in an allocation    
capacityMeasure [] = []
capacityMeasure allocation = let
    (s:rest) = map fst highschoolsBinary
    allocation' = allocationSplitup allocation
    in capacityMeasure' (s:rest) allocation' where
        capacityMeasure' [] _                 = []
        capacityMeasure' (s:rest) allocation' = [length (elemIndices s allocation')] ++ capacityMeasure' rest allocation'

-- The function readResult gives a list of fitnesses of a list of allocations
readResultoriginal population = map sqrt $ map allocationfitness $ map fst population
readResultmeanpos population = map (\x -> (1000 - x) / 100 ) 
                                   (readResultoriginal population)
readResultalloc [] = []
readResultalloc ((a,b):rest) = ((map bitstringtofloat (allocationSplitup a)),
                 b,((1000 - (sqrt $ allocationfitness a)) / 100))
                : readResultalloc rest

-- List of random allocations
randomAllocations seed number = let
        seeds = generateSeeds seed number
       in map (\x -> createallocation x) seeds