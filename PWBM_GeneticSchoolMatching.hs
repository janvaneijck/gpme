module PWBM_GeneticSchoolMatching where
import System.Random

-- Needed for the Student Generator and Permutation Function --
import Control.Monad
import Data.List
import Data.Array.ST
import Control.Monad.ST
import Data.STRef
import Data.Ord (comparing)



-- Types --
---------------------------------------------------------------------------------------
type Seed           = Int
type Student        = Int
type School         = Int
type Frequency      = Int
type Allocation     = [(School,[Student])]
type Highschools    = [(School,Frequency)]
type Preferences    = [(Student,[School])]
type Population     = [(Allocation, Frequency)]



-- Help Functions --
---------------------------------------------------------------------------------------

-- Generalisation of 'div' to any instance of Real (from Data.Fixed)
div' :: (Real a,Integral b) => a -> a -> b
div' n d = floor ((toRational n) / (toRational d))

-- Generalisation of 'mod' to any instance of Real (from Data.Fixed)
mod' :: (Real a) => a -> a -> a
mod' n d = n - (fromInteger f) * d where
    f = div' n d

-- The function generateseeds generates randomnumbers based on some seed 
generateSeeds :: Seed -> Int -> [Int]
generateSeeds seed k = map (`mod` 10000) (take k (randoms $ mkStdGen seed :: [Int]))

-- randomRange takes a seed, an interval and a quantity n and returns n value
--  uniformly picked from the interval using the seed.
randomRange :: Random a => Seed -> (a, a) -> Int -> [a]
randomRange seed (a,b) k = take k $ randomRs (a,b) (mkStdGen seed)

-- The function shuffle generates a permutation of some given list based on the seed
shuffle :: Seed -> [b] -> [b]
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

-- The function freqRep represents a population of individual organisms as pairs
-- (organism type, frequency) similar to the type Population.
freqRep :: (Eq a) => [a] -> [(a, Int)]
freqRep [] = []
freqRep (org:rest) = (org, (count org rest) + 1) :
                      freqRep (filter (\x -> x /= org) rest)
                        where
                            count _ [] = 0
                            count x (y:ys)
                                       | x == y = 1 + (count x ys)
                                       | otherwise = count x ys
        
-- The function studentRepresentation creates a list of schools, s.t. at index i the 
--  school to which student i+1 is allocated is represented.
studentRepresentation :: Allocation -> [School]             
studentRepresentation allocation = map fst $ sortBy (comparing snd) $
                                    listofpairs allocation
    where
        listofpairs [] = []
        listofpairs ((sch,stds):rest) = listofpairs' sch stds ++ listofpairs rest 
            where
                listofpairs' sch stds = map (\x -> (sch,x)) stds

-- The function allocationRepresentation creates an allocation from a list of schools
--  created in the syle of the function studentRepresentation.
allocationRepresentation :: [School] -> Highschools -> Allocation
allocationRepresentation studentrep highschools = let
    schools = map fst highschools
   in allocationRepresentation' studentrep schools where
    allocationRepresentation' _ [] = []
    allocationRepresentation' studentrep (hsch:hschs) =    
       (hsch, map (\x -> x+1) (elemIndices hsch studentrep)) : 
            allocationRepresentation' studentrep hschs

       
        
-- Student, School and Allocation Generator --
---------------------------------------------------------------------------------------

-- The list highschools is a list of pairs (school, capacity).
highschools :: Highschools
highschools =  [(1,10),(2,10),(3,12),(4,12),(5,10),(6,15),(7,12),(8,12),(9,12),(10,15)]

-- The function studentgroup generates a group of students with specific preferences
studentgroup :: Seed -> Highschools -> [Student] -> [Int] -> [Int] -> Preferences
studentgroup seed schools students toppref lowpref = let
    schoolslist     = map fst schools
    leftoverschools = (schoolslist \\ toppref) \\ lowpref
    seeds           = generateSeeds seed (length students)
    in studentgroup' seeds students leftoverschools toppref lowpref where
        studentgroup' [] _ _ _ _    = []
        studentgroup' _ [] _ _ _    = []
        studentgroup' (seed:seedrest) (student:studentrest) leftoverschools top low =
            [(student,(toppref ++ (shuffle seed leftoverschools) ++ lowpref))] ++
            studentgroup' seedrest studentrest leftoverschools top low

-- The function generateStudentpreferences generates a list of students, given
--  some seed according to the semi-random specified structure below.
generateStudentpreferences :: Seed -> Highschools -> Preferences
generateStudentpreferences seed highschools = let
        s       = highschools
        seeds   = generateSeeds seed 6
        genericstudents1 = studentgroup (seeds !! 0) s [1..15] 
                            [fst (s !! 0), fst (s !! 1), fst (s !! 2)]
                            [fst (s !! 7), fst (s !! 8), fst (s !! 9)]        
        genericstudents2 = studentgroup (seeds !! 1) s [16..30]
                            [fst (s !! 0), fst (s !! 2), fst (s !! 1)]
                            [fst (s !! 7), fst (s !! 8), fst (s !! 9)] 
        genericstudents3 = studentgroup (seeds !! 2) s [31..38]
                            [fst (s !! 1), fst (s !! 0), fst (s !! 2)]
                            []
        genericstudents4 = studentgroup (seeds !! 3) s [39..45]   
                            [fst (s !! 1), fst (s !! 2), fst (s !! 0)]
                            []
        genericstudents5 = studentgroup (seeds !! 4) s [46..60]   
                            []          
                            [fst (s !! 8),fst (s !! 9)]
        genericstudents6 = studentgroup (seeds !! 5) s [61..100]
                            []
                            []
        in genericstudents1 ++ genericstudents2 ++ genericstudents3 ++
           genericstudents4 ++ genericstudents5 ++ genericstudents6

-- The function generateAllocation creates an allocation based on a permutation of the
--  list of student, then creating a list, based on this order, s.t. for each student
--  the highest still available school is picked.
generateAllocation :: Seed -> Highschools -> Preferences -> Allocation
generateAllocation seed highschools prefs = let
    studs           = map fst prefs
    permut          = shuffle seed studs
    startalloc      = map (\(s,c) -> (s,[])) highschools
   in sortBy (comparing fst) $ 
        generateAllocation' permut prefs (highschools,startalloc) 
    where
        generateAllocation' [] _ (_,alloc) = alloc
        generateAllocation' (s:rest) prefs (highschools,alloc) = 
            generateAllocation' rest prefs (pickTopSchool s prefs highschools alloc)
    
-- The function pickTopSchool returns the highest-preferred school that is still
--  available for some given student, given some allocation.
pickTopSchool ::    Student 
                 -> Preferences 
                 -> Highschools 
                 -> Allocation 
                 -> (Highschools,Allocation) 
pickTopSchool student prefs highschools allocation = let
   Just prefstud   = lookup student prefs
  in pickTopSchool' prefstud highschools allocation where
        pickTopSchool' [] _ _ = error "student cannot be placed"
        pickTopSchool' (s:rest) highschools allocation  = let
                                Just capacity = lookup s highschools
                                Just allocated = lookup s allocation
                               in if capacity > 0 
                                    then ((s, capacity-1): 
                                        delete (s,capacity) highschools,
                                                (s,student:allocated): 
                                               delete (s,allocated) allocation)
                                    else pickTopSchool' rest highschools allocation

                                    

-- Genetic Algorithm --
---------------------------------------------------------------------------------------

-- CreateSample
createSample :: Seed -> Int -> Preferences -> Highschools -> [Allocation]
createSample seed size prefs highschools = let
    seeds  = generateSeeds seed size
   in createSample' seeds highschools prefs where
        createSample' [] _ _ = []
        createSample' (s:rest) highschools prefs = 
            generateAllocation s highschools prefs : 
            createSample' rest highschools prefs

-- Fitness Function
-- The function allocationfitness is the fitness function that determines the fitness
--  of some allocation of students to highschools based, s.t.
--      - higher values of allocationfitness mean fitter allocations
--      - a fitness of zero is given to allocations that do not take the capacity of
--        school into account
fitnessalloc :: Allocation -> Preferences -> Highschools -> Float
fitnessalloc xs prefs highschools = if capacityCheck xs
                                    then fromIntegral $ 
                                        (fitnessalloc' xs prefs)^2
                                    else 0.0
    where
    capacityCheck [] = True
    capacityCheck ((s,xs):rest) = let
        Just capacity   = lookup s highschools
        nrallocated     = length xs
       in nrallocated <= capacity && capacityCheck rest
    fitnessalloc' [] _ = 0
    fitnessalloc' ((sch,stds):rest) prefs = (fitnessschool sch stds prefs) + 
                                             fitnessalloc' rest prefs 
        where
            fitnessschool _ [] prefs = 0
            fitnessschool sch (std:rest) prefs = 
                (10 - (head $ elemIndices sch $ snd (prefs !! (std-1)))) + 
                fitnessschool sch rest prefs
     
-- Function Evolve
-- The function evolve preforms the genetic algorithm.
evolve ::   Seed
           -- seed
         -> Highschools
           -- list of schools and their capacity
         -> Int           
           -- bound on generation size
         -> Int
           -- maximum number of generations
         -> Float  
           -- crossover parameter
         -> Float  
           -- mutation parameter
         -> Population
           -- Population after evolution
evolve seed highschools gensize nrgen cpar mpar = let
        primeseeds  = generateSeeds seed 3
        seedpref    = primeseeds !! 0
        seedpop     = primeseeds !! 1
        seedgen     = primeseeds !! 2
        prefs       = generateStudentpreferences seedpref highschools
        pop         = freqRep $ createSample seedpop gensize prefs highschools
        seedsgen    = generateSeeds seedgen nrgen
    in evolve' seedsgen prefs pop nrgen where
        evolve' _ _ pop 0 = pop
        evolve' _ _ [(perfectentity,0)] k = [(perfectentity,0)]
        evolve' (s:seeds) prefs pop k =
          evolve' seeds prefs (createGen s highschools gensize cpar mpar prefs pop) 
                  (k-1)
    
-- The function createGen creates a new generation from a given population, by 
--  performing reproduction, mutation and crossover. The actual ellitism takes
--  place here, in the sense that the best reproduced organism cannot crossover
--  nor mutate.
createGen ::    Seed
               -- seed
             -> Highschools
               -- list of schools and their capacity
             -> Int
               -- bound in generation size
             -> Float
               -- crossover parameter
             -> Float
               -- mutation parameter
             -> Preferences
               -- preferencelists for all students
             -> Population
               -- old generation
             -> Population
               -- new generation
createGen seed highschools gensize cpar mpar prefs pop = let
        [(seedPool, seedCross, seedMut)] = tripleUp $ generateSeeds seed 3       
        reproduced                       = reproduction seedPool pop gensize
                                            highschools prefs
        bestentity                       = head reproduced
        pool                             = tail reproduced
        nrstudents                       = fromIntegral $ length $ map fst prefs
    in if length reproduced == 1 && 
          (fitnessalloc (head reproduced) prefs highschools == nrstudents * 10)
            then [(head reproduced,0)]
            else let        
                    sizecrossoverpool = round $ (fromIntegral gensize)*cpar - 
                                         (mod' ((fromIntegral gensize)*cpar) 2)
                    crossoverpool     = pairUp $ take sizecrossoverpool pool
                    clonepool         = drop sizecrossoverpool pool
                    seedscross        = take gensize (randoms $ mkStdGen seedCross)                  
                    seedsmut          = generateSeeds seedMut gensize
                in freqRep $ bestentity : (mutate seedsmut highschools prefs
                        ((crossover' seedscross highschools prefs crossoverpool) ++ 
                            clonepool))
                        where
                         crossover' _ _ _ []                                  = []
                         crossover' (s:srest) highschools prefs ((a,b):prest) = 
                            (crossover s highschools prefs a b) ++ 
                                (crossover' srest highschools prefs prest)
                         mutate _ _ _ []                              = []
                         mutate (s:srest) highschools prefs (o:orest) = 
                            (mutation s (mpar :: Float) highschools o) : 
                                mutate srest highschools prefs orest 

-- Function Reproduction
-- The function reproduciton performs reproduction using a roulette wheel
--  reproduction technique in combination with elitism.
reproduction ::    Seed               
                  -- seed
                -> Population      
                  -- population
                -> Int               
                  -- bound on generation size
                -> Highschools
                  -- list of schools and their capacity
                -> Preferences
                  -- preferencelists for all students
                -> [Allocation]
                  -- allocations that are reproduced s.t. the first allocation is the
                  --  best entity of the population
reproduction seed pop size highschools prefs = let
        nrstudents  = fromIntegral $ length $ map fst prefs
        pop'        = map fst pop
        fitnesspop  = zip pop (map (\x -> fitnessalloc x prefs highschools) pop')
        bestentity  = fst $ fst $ maximumBy (comparing snd) fitnesspop
        perfectlist = (filter (\((x,y),z) -> z == (nrstudents * 10.0)) fitnesspop)     
        in if perfectlist /= []
            then [fst $ fst $ head perfectlist]
            else let
                    totalfit = sum $ map (\((x, freq), fit) -> fromIntegral freq * fit)
                                         fitnesspop
                    fitProb  = map (\ ((x, freq),fit) -> 
                                    fromIntegral freq * fit / totalfit )
                                   fitnesspop     
                    popInterval = zip (map fst pop) (cumList fitProb)
                    coinflips   = randomRange seed (0.0,1.0) (size-1)
                in bestentity : findStrings coinflips popInterval where
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

-- Function Mutation
-- The function mutation1 flips a coin to determine if some student will be placed at
--  some other randomly picked school.
mutation ::    Seed 
              -- seed
            -> Float
              -- mutation parameter
            -> Highschools
              -- list of schools and their capacity
            -> Allocation
              -- school allocation (organism)
            -> Allocation
mutation seed parameter highschools allocation = let
    allocation' = studentRepresentation allocation
    seeds   = generateSeeds seed (length allocation')
   in allocationRepresentation (mutation' seeds highschools allocation') highschools
   where
    mutation' [] _ _ = []
    mutation' _ [] _ = []
    mutation' (s:rest) highschools (sch:schs) = let
       f = fst $ randomR (0,1) (mkStdGen s)
       in
        if f < parameter 
            then let
                schools = delete sch (map fst highschools)
                randindex = fst $ randomR (0, (length schools)-1) (mkStdGen s)
                sch' = schools !! randindex
                in sch' : mutation' rest highschools schs 
            else sch : mutation' rest highschools schs
                                                                   
-- Function Crossover
-- The function crossover pick a random school, then switches the students allocated to
--  this school between to organisms and corrects the allocations with the function
--  highestLeftover  
crossover :: Seed -> Highschools -> Preferences -> Allocation -> Allocation -> [Allocation]
crossover seed highschools prefs aloc1 aloc2 = let
        schools         = map fst highschools
        randindex       = fst $ randomR (0, (length schools)-1) (mkStdGen seed)
        randomsch1      = schools !! randindex
        leftoverschools = delete randomsch1 schools
        Just studsaloc1 = lookup randomsch1 aloc1
        Just studsaloc2 = lookup randomsch1 aloc2
       in fillupmissing prefs 
          [(randomsch1, studsaloc2) : 
           (removeduplicates studsaloc2 (delete (randomsch1, studsaloc1) aloc1)),
           (randomsch1, studsaloc1) :
           (removeduplicates studsaloc1 (delete (randomsch1, studsaloc2) aloc2))]
        where
            removeduplicates [] alloc   = alloc
            removeduplicates _  []  = []
            removeduplicates candidates alloc = removeduplicates' candidates alloc 
                where
                removeduplicates' candidates [] = []
                removeduplicates' candidates ((sch,stds):rest) =
                    (sch, rmvdpl candidates stds) : removeduplicates' candidates rest
                    where
                        rmvdpl candidates [] = []
                        rmvdpl candidates (std:stds) = 
                            if std `elem` candidates then rmvdpl candidates stds
                                                     else std : rmvdpl candidates stds
            fillupmissing prefs [] = []
            fillupmissing prefs (alloc:allocs) = [fillupmissing' prefs alloc] ++ 
                                               fillupmissing prefs allocs where
                fillupmissing' prefs alloc = let
                    allstuds    = map fst prefs
                    allstudsnow = concat $ map snd alloc
                    missing     = allstuds \\ allstudsnow
                   in generateAllocation' missing prefs highschools alloc where
                        generateAllocation' [] _ _ alloc = alloc
                        generateAllocation' (s:rest) prefs highschools alloc = 
                                generateAllocation' rest prefs highschools
                                    (highestLeftover s prefs highschools alloc)

-- The funciton highestLeftover places a student in the highest-preffered school that
--  is still available in some given allocation.                                    
highestLeftover :: Student -> Preferences -> Highschools -> Allocation -> Allocation
highestLeftover student prefs highschools allocation = let
   Just prefstud   = lookup student prefs
  in highestLeftover' prefstud highschools allocation where
        highestLeftover' [] _ _ = error "student cannot be placed"
        highestLeftover' (s:rest) highschools allocation  = let
                                Just capacity = lookup s highschools
                                Just allocated = lookup s allocation
                               in if (capacity - (length allocated)) > 0 
                                        then ((s,student:allocated):
                                            delete (s,allocated) allocation)
                                        else highestLeftover' rest highschools allocation
                                                     
-- The function calculateMeanPos calculates the mean preference-place of the students
--  in some allocation s.t. the i-th preffered school gets preference place i and the
--  first preffered school is the school the students places number 1 on his preference
--  list 
calculateMeanPos :: Allocation -> Preferences -> Highschools -> Float
calculateMeanPos xs prefs highschools = if capacityCheck xs
                                    then 11 - (fromIntegral $ 
                                        (calculateMeanPos' xs prefs))/100
                                    else -1
    where
    capacityCheck [] = True
    capacityCheck ((s,xs):rest) = let
        Just capacity   = lookup s highschools
        nrallocated     = length xs
       in nrallocated <= capacity && capacityCheck rest
    calculateMeanPos' [] _ = 0
    calculateMeanPos' ((sch,stds):rest) prefs = (fitnessschool sch stds prefs)
                                                + calculateMeanPos' rest prefs
        where
        fitnessschool _ [] prefs = 0
        fitnessschool sch (std:rest) prefs =
            (10 - (head $ elemIndices sch $ snd (prefs !! (std-1)))) +
            fitnessschool sch rest prefs

-- The function readPopulation gives a list of the mean preference-palce in some
--  population
readPopulation :: Seed -> Population -> Highschools -> [Float]
readPopulation seedused population highschools = let
    prefs = generateStudentpreferences ((generateSeeds seedused 3) !! 0) highschools
   in readPopulation' population prefs highschools where
    readPopulation' [] _ _ = []
    readPopulation' ((aloc,freq):rest) prefs highschools = 
        calculateMeanPos aloc prefs highschools : readPopulation' rest prefs highschools

-- The function giveBestAllocation gives a pair consisting of the mean preference-place
--  and some allocation, that was the allocation with the lowest mean preference-place
--  in some population.       
giveBestAllocation :: Seed -> Population -> Highschools -> (Float, Allocation)
giveBestAllocation seedused population highschools = let
    allocations = map fst population
    scores      = zip allocations $ readPopulation seedused population highschools
    cleanscores = filter (\(x,y) -> y >= 1.0) scores
    highscore   = minimumBy (comparing snd) cleanscores
   in (snd highscore, fst highscore)

{-

Run the Genetic Matching Algorithm:

- 40 Pareto efficient allocations:
    evolve 0 highschools 40 0 0.0 0.0
    
    Then: 
        readPopulation 0 it highschools
        [1.8999996,1.9499998,1.9200001,2.0600004,2.0100002,1.9899998,2.04,2.08,
         2.0200005,2.0100002,1.9499998,1.9700003,2.0200005,1.9399996,1.8999996,
         1.8999996,2.0699997,2.13,1.9899998,2.0699997,1.9399996,1.9099998,
         2.0699997,1.9300003,2.0,1.9399996,2.0600004,1.8500004,1.9700003,
         1.9899998,2.0,1.9499998,2.0,1.9200001,1.9799995,1.9799995,2.0900002,
         1.9300003,2.08,2.04]
         
        giveBestAllocation 0 it highschools
        (1.8500004,[(1,[28,13,15,2,25,21,10,3,26,7]),
        (2,[32,43,35,38,91,59,45,33,31,89]),(3,[36,27,23,41,5,18,17,37,8,44,40,39]),
        (4,[34,75,62,65,99,76,50,73,57,71,68,93]),(5,[97,53,11,70,22,30,29,24,79,96]),
        (6,[67,74,94,48,86,61,78,52,82,66,90,56,55,58,80]),
        (7,[12,9,16,1,6,14,19,4,20,88,47]),(8,[46,81,49,84,51,95,87,60,100,54,83,72]),
        (9,[42,77,85]),(10,[98,92,64,63,69])])

- Only reproduction from initial 40 Pareto efficient allocations:
    evolve 0 highschools 40 100 0.0 0.0
    
    Then:
        readPopulation 0 it highschools
        [1.8500004]
        
        giveBestAllocation 0 it highschools
        (1.8500004,[(1,[2,3,7,10,13,15,21,25,26,28]),
        (2,[31,32,33,35,38,43,45,59,89,91]),(3,[5,8,17,18,23,27,36,37,39,40,41,44]),
        (4,[34,50,57,62,65,68,71,73,75,76,93,99]),(5,[11,22,24,29,30,53,70,79,96,97]),
        (6,[48,52,55,56,58,61,66,67,74,78,80,82,86,90,94]),
        (7,[1,4,6,9,12,14,16,19,20,47,88]),(8,[46,49,51,54,60,72,81,83,84,87,95,100]),
        (9,[42,77,85]),(10,[63,64,69,92,98])])
        
        N.B. Comparing this run and the one above, clearly shows that because we have
        ellitism the 1.8500004 allocation survives, because it was the highscore in
        the initial population.
        
- Mutation s.t. 0.5 student expected to change school
    evolve 0 highschools 40 100 0.0 0.005
    
    Then:
        readPopulation 0 it highschools
        [1.8500004,-1.0,-1.0,1.9799995,-1.0,2.0200005,1.8699999,1.9799995,-1.0,
         -1.0,1.8900003,1.9399996,-1.0,1.8999996,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0]
        
        N.B. readPopulation gives -1.0 as the mean preference-position if the
             alocation is not a possible allocation
             
        giveBestAllocation 0 it highschools
        (1.8500004,[...])
        
        N.B. This is again the best allocation from the initial population, which,
        because we use ellitism, never gets mutated and thus survives.
             
- Half of the populations Crosses over
    evolve 0 highschools 40 100 0.5 0.0
    
    Then:
        readPopulation 0 it highschools
        [1.8100004,1.9899998,1.96,1.8999996,1.9200001,1.8699999,1.96,1.8599997,
         1.8900003,1.8999996,1.9399996,1.79,1.9200001,1.9300003,1.8800001,1.8999996,
         1.8999996,1.8999996,1.8800001,1.9099998,1.8500004,1.9399996,1.9399996,
         1.8599997,1.9799995,1.9200001,1.8800001,1.96,1.9099998,1.9700003,1.9300003,
         1.8699999,1.8999996]
         
        giveBestAllocation 0 it highschools
        (1.79,[(1,[2,3,7,13,15,21,25,26,28,34]),(2,[6,31,32,33,35,38,43,45,89,91]),
        (3,[5,8,17,18,23,27,36,37,39,40,42,44]),
        (4,[50,53,57,62,65,68,71,75,76,93,97,99]),(5,[10,11,22,24,29,30,59,79,96]),
        (6,[52,55,56,58,61,66,67,70,74,78,80,82,86,90,94]),
        (7,[1,4,9,12,14,16,19,20,41,47,88,100]),
        (8,[46,48,49,51,54,60,72,81,83,84,87,95]),
        (9,[73,77,85]),(10,[63,64,69,92,98])])
         
         N.B.: The best allocation now clearly has a lower mean preference-position
         then the best allocation in our initial population, which mean that the
         genetic algorithm optimised the school allocation.
         
- Half Crossover; 0.5 student per allocation expected to mutate:
    evolve 0 highschools 40 100 0.5 0.005
    
    Then:
        readPopulation 0 it highschools
        [1.8400002,2.3000002,-1.0,2.13,2.46,2.21,1.9300003,-1.0,-1.0,2.0900002,2.17,
         1.8999996,2.12,2.38,2.1499996,2.13,1.96,2.12,2.29,2.1899996,2.12,2.1400003,
         2.0900002,-1.0,-1.0,2.1000004,1.8599997,2.3199997,2.17,-1.0,2.1999998,
         2.3500004,2.1899996,2.1800003,2.1800003,1.9399996,2.3500004,2.1999998]
        
        giveBestAllocation 0 it highschools
        (1.8400002,[(1,[2,3,5,8,11,13,21,24,25,26]),
        (2,[31,32,33,34,35,38,43,59,89,91]),(3,[10,15,17,18,23,27,36,37,39,40,41,44]),
        (4,[45,50,57,62,65,68,71,73,75,76,93,99]),(5,[7,22,29,30,53,70,79,96,97]),
        (6,[52,55,56,58,60,61,66,67,74,78,80,82,86,90,94]),
        (7,[1,4,6,9,12,14,16,19,20,28,47,88]),
        (8,[46,48,49,51,54,72,81,83,84,87,95,100]),
        (9,[42,77,85]),(10,[63,64,69,92,98])])
        
        N.B. The best allocation is slightly better than the best allocation in the
        initial population, but the difference is very small and could therefor be due
        to the imprecision of Haskell.
         
         
- Half Crossover; 0.1 per allocation student to mutate (of 4 students expected to
    mutate in the population, i.e. all allocations combined): 
    evolve 0 highschools 40 100 0.5 0.001
    
    Then:    
        giveBestAllocation 0 it highschools
        (1.8000002,[(1,[1,2,3,4,9,10,15,21,22,30]),(2,[31,32,33,35,38,42,43,45,59,91]),
        (3,[5,8,17,18,23,27,36,37,39,40,41,44]),
        (4,[50,53,57,65,68,71,73,75,76,93,97,99]),(5,[7,11,24,26,29,34,54,79,96]),
        (6,[52,55,56,58,61,66,67,70,74,78,80,82,86,90,94]),
        (7,[6,12,13,14,16,19,20,25,28,47,88,100]),
        (8,[46,48,49,51,60,72,81,83,84,87,89,95]),(9,[62,77,85]),
        (10,[63,64,69,92,98])])
    
- Half Crossover; 1 student in the generation expected to mutate: 
    evolve 0 highschools 40 100 0.5 0.00025
    
    Then:
        giveBestAllocation 0 it highschools
        (1.8000002,[(1,[2,4,7,9,10,12,13,14,15,26]),
        (2,[31,32,33,34,35,38,43,45,79,89]),(3,[5,8,17,18,21,23,36,37,39,40,41,44]),
        (4,[50,53,57,62,65,68,71,73,75,76,93,99]),(5,[11,22,24,27,29,30,59,84,96,97]),
        (6,[52,55,56,58,61,66,67,70,74,78,80,82,86,90,94]),
        (7,[1,3,6,16,19,20,25,28,47,88]),(8,[42,46,48,49,51,54,60,72,81,83,87,100]),
        (9,[77,85]),(10,[63,64,69,91,92,95,98])])
    
- 80% Crossover; 1 student in the generation expected to mutate: 
    evolve 0 highschools 40 100 0.8 0.00025
    
    Then:
        giveBestAllocation 0 it highschools
        (1.79,[(1,[1,2,3,5,8,10,13,15,26,28]),(2,[31,32,33,34,35,38,41,43,45,91]),
        (3,[7,12,17,18,19,23,24,36,37,39,40,44]),
        (4,[50,53,57,62,65,68,71,73,75,76,93,99]),(5,[11,21,22,27,29,30,59,79,96,97]),
        (6,[52,55,56,58,61,66,67,70,74,78,80,82,86,90,94]),
        (7,[4,6,9,14,16,20,25,47,88,100]),(8,[46,48,49,51,54,60,72,81,83,84,87,95]),
        (9,[42,77,85,89]),(10,[63,64,69,92,98])])
    
- 80% Crossover; 
    evolve 0 highschools 40 100 0.8 0.001
    
    Then:
        giveBestAllocation 0 it highschools
        (1.8100004,[(1,[1,2,3,4,7,8,10,21,28,29]),(2,[31,32,33,35,40,43,45,59,89,91]),
        (3,[5,17,18,19,23,24,27,36,37,38,39,44]),
        (4,[34,50,53,57,62,65,68,71,75,76,93,99]),(5,[11,15,22,26,30,70,79,84,96,97]),
        (6,[52,55,56,58,61,66,67,74,78,80,82,86,90,94]),
        (7,[6,9,12,13,14,16,20,25,41,47,88]),(8,[42,46,48,49,51,54,60,72,81,83,87,100]),
        (9,[73,77,85]),(10,[63,64,69,92,95,98])])

  
 -}