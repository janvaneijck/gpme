module PWBM_Studentgenerator where
import System.Random

import Control.Monad
import Data.List
import Data.Array.ST
import Control.Monad.ST
import Data.STRef

import Data.Char

-- Help Functions: --------------------------------------------------------------------

-- The function shuffle generates a permutation of some given list based on the seed
shuffle :: Int -> [b] -> [b]
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
 
-- The function generateseeds generates randomnumbers based on some seed 
generateSeeds :: Int -> Int -> [Int]
generateSeeds seed k = map (`mod` 10000) (take k (randoms $ mkStdGen seed :: [Int]))
        
-- The function studentgroup generates a group of students with specific preferences 
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

-- The function studentpreferences generates a list of pairs (student,preferencelist)           
studentpreferences seed schools = let
        s       = schools
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
  

 
