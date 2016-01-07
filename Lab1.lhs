Computer Assignments 1
======================

Jan van Eijck

Computer assignments from Chapter 1 of [@Goldberg1989:ga]. 

A.
---

> module Lab1 where

> import System.Random
> import Data.List


Get a random probability (a floating point number in the range 0..1):

> getRandomProb :: IO Float
> getRandomProb = getStdRandom random

Collect n random probability numbers in the range 0..1:

> probs 0 = return []
> probs n = do
>              p <- getRandomProb 
>              ps <- probs (n-1) 
>              return (p:ps)

Collect 10000 probs in the range 0..1 and return their average: 

> prs = do
>          xs <- probs 10000
>          return (sum xs / 10000)

Counting the numbers in the quartiles, for a list of floating point
numbers, supposedly each in the interval `[0..1)`:

> classify :: Float -> Int
> classify p | p < 0.25  = 1
>            | p < 0.50  = 2 
>            | p < 0.75  = 3 
>            | otherwise = 4

Here is some useful code for our toolkit: 

> type Count a     = (a, Integer)

> addCount :: Eq a => [Count a] -> Count a -> [Count a]
> addCount []     y    = [y]
> addCount (x:xs) y
>     | fst x == fst y = (fst x, snd x + snd y):xs
>     | otherwise      = x:addCount xs y

> freqCount :: Ord a => [a] -> [Count a]
> freqCount xs = sortBy (\ (x,_) (y,_) -> compare x y)
>                 (foldl' addCount [] [(x, 1) | x <- xs])

Use this to classify a list of probabilities according
to their quartiles. 
    
> countQuarts :: [Float] -> [Count Int]
> countQuarts = freqCount . map classify

Now we can inspect the quality of our random generator of
probabilities. 
    
> prsInspect = do
>                xs <- probs 10000
>                return (countQuarts xs)

B.
--

> distrib =
>   [0.1, 0.2, 0.05, 0.15, 0.0, 0.11, 0.07, 0.04, 0.0, 0.12, 0.16] :: [Float]

> check = sum distrib == 1.0

We assume that the input of `roulette` is a list of probabilities that
sum to 1.  The output is an integer indicating the element from the
list that is selected.


> roulette :: [Float] -> IO Int
> roulette xs = do
>                 x <- getStdRandom random :: IO Float
>                 return (select x xs)

The selection function keeps track of the sizes of the slots on the
roulette wheel so that the item at position n has the slot (u,v],
where `u = sum [prob[0]..prob[n-1]]` and `v = sum [prob[0]..prob[n]]`.

> select :: Float -> [Float] -> Int
> select x xs = sel 0 0 xs where
>  sel n u [v] = if x <= u+v then n else error "wrong data"
>  sel n u (v:vs) = if  x <= u+v then n else sel (n+1) (u+v) vs

Spin the roulette wheel n times and collect the results. 

> spin :: Integer -> [Float] -> IO [Int]
> spin 0 _ = return []
> spin n xs = do
>               k <- roulette xs
>               ks <- spin (n-1) xs
>               return (k:ks)

Now let's check the results by repeating the spinning procedure
10000 times.

If we compute new probabilities from the frequency count, then the
result should be close to the probabilities that we started out with.

> freq2prob :: [Count a] -> [(a,Float)]
> freq2prob freqs = let
>     total = sum $ map snd freqs
>   in
>     map (\ (x,n) -> (x,(fromInteger n/fromInteger total))) freqs

Glueing the two together: 

> probCount :: Ord a => [a] -> [(a,Float)]
> probCount = freq2prob . freqCount

                         
Use `probCount` to check the roulette implementation: 

> checkFreq ::  [Float] -> IO [(Int,Float)]
> checkFreq xs = do
>                  ks <- spin 10000 xs
>                  return (probCount ks)

Now it is easy to see that the results are very close to the input
probabilities.

C.
---
Generate lists of numbers in a specified interval.
         
> numbersR 0 _ = return []
> numbersR n (lo,hi) = do
>                        x <- getStdRandom $ randomR (lo,hi) 
>                        xs <- numbersR (n-1) (lo,hi)
>                        return (x:xs)

Test for 10000 numbers between 3 and 12.

> testNrsR :: IO [Count Integer]
> testNrsR = do
>              ks <- numbersR 10000 (3,12)
>              return (freqCount ks)               

D.
---

No need to restrict the type to bit lists.

> crossover :: Int -> ([a],[a]) -> ([a],[a])
> crossover k (xs,ys) = (take k xs ++ drop k ys, take k ys ++ drop k xs)

We can also build in some constraints:

> cross :: Int -> ([a],[a]) -> ([a],[a])
> cross k (xs,ys) = if length xs /= length ys then error "different lengths"
>                   else if k < 0 then error "negative index"
>                   else if k >= length xs then error "index too large"
>                   else crossover k (xs,ys)

Testing this with the bitstrings mentioned in the assignment gives the
expected results.

E.
---

> type Bit = Int

> mutation :: [Bit] -> Int -> Float -> IO [Bit]
> mutation bits k prob = let
>                          b = bits !! k
>                          b' = (b + 1) `mod` 2
>                          bits' = take (k-1) bits ++ [b'] ++ drop k bits
>                        in
>                          do                         
>                             x <- getStdRandom random :: IO Float
>                             if x < prob then return bits else return bits'

Do any number of mutations:

> mutations ::  [Bit] -> Int -> Float -> Int -> IO [[Bit]]
> mutations _ _ _ 0 = return []
> mutations bits k prob n = do
>                             bs <- mutation bits k prob
>                             bss <- mutations bits k prob (n-1)
>                             return (bs:bss)

Testing this, for 10000 calls.

> mutationsTest :: [Bit] -> Int -> Float -> IO [([Bit],Float)]
> mutationsTest bits k prob = do
>                              bss <- mutations bits k prob 10000
>                              return (probCount bss)

F.
---

Type of a population:

> type Population = [Count [Bit]]
> 
> popSize :: Population -> Integer
> popSize = sum . (map snd)

Example population from the Goldberg assignment:
        
> population :: Population
> population = [([1,1,1,0,0],100),([0,0,0,1,1],100)]
 
From a population to a probability distribution:

> pop2dist :: Population -> [Float]
> pop2dist pop = let
>    counts = map snd pop
>    total = sum counts
>  in
>    map (\x -> fromInteger x / fromInteger total) counts

Pick an element from a population using a roulette:

> pickRandom :: Population -> IO [Bit]
> pickRandom pop = let
>    dist = pop2dist pop
>  in 
>    do
>      k <- roulette dist
>      return (fst (pop!!k))
 
Pick a list of parents for forming the next generation, using the
roulette:

> pickParents :: Population -> IO [[Bit]]
> pickParents pop = let
>      dist = pop2dist pop
>      n    = popSize pop
>    in 
>      do
>       ks <- spin n dist
>       return (map (fst . (pop!!)) ks)
 
Use a list of parents to create a new generation.

> nextGeneration :: [[Bit]] -> IO Population
> nextGeneration bss = let
>     pairs = pairup bss
>     n     = length (head bss)
>   in
>     do
>       ks <- numbersR (length pairs) (0,n-1)
>       return (freqCount (unpair $ map (uncurry cross) (zip ks pairs)))

This uses auxiliary functions for pairing up and unpairing. 
                 
> pairup :: [a] -> [(a,a)]
> pairup [] = []
> pairup [_] = []
> pairup (x:y:zs) = (x,y) : pairup zs

> unpair :: [(a,a)] ->[a]
> unpair [] = []
> unpair ((x,y):zs) = x:y: unpair zs
                        
Create the next generation:
       
> nextGen :: Population -> IO Population
> nextGen pop = do
>                ps <- pickParents pop
>                newpop <- nextGeneration ps
>                return newpop

Evolve a number of generations:

> evolveGens :: Population -> Integer -> IO Population
> evolveGens pop 0 = return pop
> evolveGens pop n = do
>                      newpop <- nextGen pop
>                      evolveGens newpop (n-1)

---
