{-# LANGUAGE FlexibleInstances #-}

module GA_Graphical-OT where
import Data.List
import Data.Ord (comparing)
import Data.Sequence hiding (splitAt, take, zip, sortBy, filter)
import Codec.Picture
import System.Random
import RefImage (refImage)


class (Eq a) => Organism a where
    mutation :: Seed -> Parameter -> a -> a
    crossover :: Seed -> a -> a -> [a]
    orgToSeq :: a -> Int -> Int -> Seq [Pixel8]
    fitness :: a -> Int -> Int -> Int

type Seed = Int
type Parameter = Float
type Frequency = Int
type Coordinate = Int
type Radius = Int
type ImageGray = [(Coordinate, Coordinate, Radius, Pixel8, Pixel8)]
type ImageRGB = [(Coordinate, Coordinate, Radius, Pixel8, Pixel8, Pixel8)]



---------------------------------- HELPER FUNCTIONS -------------------------------

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
freqRep :: (Eq a, Num t) => [a] -> [(a, t)]
freqRep [] = []
freqRep (org:rest) = (org, (count org rest) + 1) :
                      freqRep (filter (\x -> x /= org) rest)
                        where
                            count _ [] = 0
                            count x (y:ys)
                                       | x == y = 1 + (count x ys)
                                       | otherwise = count x ys

-- initialSequence creates an initial 'empty canvas' to draw the circles on
initialSequence :: Int -> Int -> Int -> Seq [Pixel8]
initialSequence 2 width height = Data.Sequence.replicate (width*height) [255,150]
initialSequence 3 width height = Data.Sequence.replicate (width*height) [255,255,255]
initialSequence _ width height = error "initialSequence ERROR: not the correct number of components"

-- circleCoord takes the dimensions of a picture and the properties for a circle (x and y coordinate 
-- and radius) and returns all pixels of the picture inside the said picture.
circleCoord :: Int -> Int -> Int -> Int -> Int -> [(Int,Int)]
circleCoord width height x y r = filter circle square where
    x' = fromIntegral x
    y' = fromIntegral y
    r' = fromIntegral r
    square = [(a,b) | a <- [0..width-1], b <- [0..height-1], a >= x, b >= y, a <= x + 2*r, b <= y + 2*r]
    circle (a,b) = fromIntegral b       < 
        sqrt (r'*r' - (fromIntegral a - (x' + r'))*(fromIntegral a - (x' + r'))) + (y' + r') 
                && fromIntegral (-b)    < 
        sqrt (r'*r' - (fromIntegral a - (x' + r'))*(fromIntegral a - (x' + r'))) - (y' + r')

---------------------------------- GENETIC ALGORITHM -------------------------------


instance Organism ImageGray where
  mutation seed p organism = let
        coinflips = randomRange seed (0.0, 1.0) (5 * Data.List.length organism)
        differences = randomRange seed (-20,20) (5 * Data.List.length organism) :: [Int]
    in mutation' coinflips differences organism where
        mutation' _ _ [] = []
        mutation' (fx:fy:fr:fl:fa:restflips) (dx:dy:dr:dl:da:restdiff) ((x,y,r,l,a):restbits) = let
                newx = if fx < p then x + dx else x
                newy = if fy < p then y + dy else y
                newr = if fr < p then r else r
                newl = if fl < p then l + (fromIntegral dl :: Pixel8) else l
                newa = if fa < p then a else a
            in (newx,newy,newr,newl,newa) : mutation' restflips restdiff restbits

  crossover randomnr p1 p2 = let
        maxSize = max (Data.List.length p1) (Data.List.length p2)
        cut = randomnr `mod` (maxSize - 1) + 1
        (p11, p12) = splitAt cut p1
        (p21, p22) = splitAt cut p2
    in [p11 ++ p22, p21 ++ p12]

  orgToSeq org width height = orgToSeq' org (initialSequence 2 width height) where
    orgToSeq' [] imSeq = imSeq
    orgToSeq' ((x, y, r, l, a):rest) imSeq = orgToSeq' rest (updateSeq (circleCoord width height x y r) imSeq) where
        updateSeq [] ss = ss
        updateSeq ((a,b):rest) ss = updateSeq rest (adjust (\[x,alpha] -> 
            [(fromIntegral (max ((fromIntegral x :: Int) + (fromIntegral l :: Int) - 255) 0) :: Pixel8),alpha])
                (a + b*width) ss)

  fitness org width height = let
      ref = mapWithIndex (\_ (l,a) -> [l,a]) refImage
      orgSeq = orgToSeq org width height
    in compareSequences ref orgSeq where
      compareSequences seq1 seq2 
        | Data.Sequence.length seq1 == 0 && Data.Sequence.length seq2 == 0  = 0
        | Data.Sequence.length seq1 == Data.Sequence.length seq2            = let
            lum1 = head $ index seq1 0
            lum2 = head $ index seq2 0
          in (abs (fromIntegral lum1 - fromIntegral lum2))^3 + compareSequences (Data.Sequence.drop 1 seq1) (Data.Sequence.drop 1 seq2)
        | otherwise = error "compareSequences ERROR: sequences not of the same length"



instance Organism ImageRGB where
  mutation seed p organism = let
        coinflips = randomRange seed (0.0, 1.0) (6 * Data.List.length organism)
        differences = randomRange seed (-20,20) (6 * Data.List.length organism) :: [Int]
    in mutation' coinflips differences organism where
        mutation' _ _ [] = []
        mutation' (fx:fy:fr:fcr:fcg:fcb:restflips) (dx:dy:dr:dcr:dcg:dcb:restdiff) ((x,y,r,cr,cg,cb):restbits) = let
                newx = if fx < p then x + dx else x
                newy = if fy < p then y + dy else y
                newr = if fr < p then r else r
                newcr = if fcr < p then cr + (fromIntegral dcr :: Pixel8) else cr
                newcg = if fcg < p then cg + (fromIntegral dcg :: Pixel8) else cg
                newcb = if fcb < p then cb + (fromIntegral dcb :: Pixel8) else cb
            in (newx,newy,newr,newcr,newcg,newcb) : mutation' restflips restdiff restbits

  crossover randomnr p1 p2 = let
        maxSize = max (Data.List.length p1) (Data.List.length p2)
        cut = randomnr `mod` (maxSize - 1) + 1
        (p11, p12) = splitAt cut p1
        (p21, p22) = splitAt cut p2
    in [p11 ++ p22, p21 ++ p12]

  orgToSeq org width height = orgToSeq' org (initialSequence 3 width height) where
    orgToSeq' [] imSeq = imSeq
    orgToSeq' ((x, y, r, cr, cg, cb):rest) imSeq = orgToSeq' rest (updateSeq (circleCoord width height x y r) imSeq) where
        updateSeq [] ss = ss
        updateSeq ((a,b):rest) ss = updateSeq rest (adjust (\[cr,cg,cb] -> 
            [cr,cg,cb])
                (a + b*width) ss)

  fitness org width height = let
      ref = Data.Sequence.empty
      orgSeq = orgToSeq org width height
    in compareSequences ref orgSeq where
      compareSequences seq1 seq2 
        | Data.Sequence.length seq1 == 0 && Data.Sequence.length seq2 == 0  = 0
        | Data.Sequence.length seq1 == Data.Sequence.length seq2            = let
            r1 = head $ index seq1 0
            r2 = head $ index seq2 0
          in (abs (fromIntegral r1 - fromIntegral r2))^3 + compareSequences (Data.Sequence.drop 1 seq1) (Data.Sequence.drop 1 seq2)
        | otherwise = error "compareSequences ERROR: sequences not of the same length"


-- The function reproduciton performs the reproduction using a roulette wheel
-- reproduction technique.
reproduction :: Organism a => Seed -> [(a,Frequency)] -> Int -> [a]
reproduction seed pop size = let
        fitnesspop  = zip pop (map ((\o -> fitness o 64 64) . fst) pop)
        totalfit    = sum $ map (\((_,freq),fit) -> fromIntegral freq / fromIntegral fit) fitnesspop
        fitProb     = map (\((_,freq),fit) -> fromIntegral freq / (fromIntegral fit * totalfit) ) fitnesspop
        popInterval = zip (map fst pop) (cumList fitProb)
        coinflips   = randomRange seed (0.0, 1.0) (size-1) :: [Float]

        sortedpop   = sortBy (comparing snd) fitnesspop
        best        = fst $ fst $ head sortedpop
    in (best : findOrganisms coinflips popInterval) where
        findOrganisms [] _ = []
        findOrganisms (coinflip:rest) popInterval = 
            findOrganisms' coinflip popInterval : findOrganisms rest popInterval where
                findOrganisms' coinflip ((org,(a,b)):rest) = if b == 1
                    then if a <= coinflip && coinflip <= b
                            then org
                            else findOrganisms' coinflip rest
                    else if a <= coinflip && coinflip < b
                            then org
                            else findOrganisms' coinflip rest

-- The function createGen creates a new generation from a given population, by performing reproduction, 
-- mutation and crossover.
createGen :: (Organism a) => Seed -> [(a,Frequency)] -> Int -> Parameter -> Parameter -> [(a,Frequency)]
createGen seed pop gensize cpar mpar = let
        [(seedPool, seedCross, seedMut)] = tripleUp $ map 
                                           (`mod` 10000)
                                           (take 3 (randoms $ mkStdGen seed :: [Int]))
        (best:pool) = reproduction seedPool pop gensize
        sizecrossoverpool = round $ (fromIntegral gensize)*cpar - 
                                                (mod' ((fromIntegral gensize)*cpar) 2)
        crossoverpool = take sizecrossoverpool pool
        clonepool = Data.List.drop sizecrossoverpool pool
        seedscross = take gensize (randoms $ mkStdGen seedCross)
        seedsmut = take gensize (randoms $ mkStdGen seedMut :: [Int])
    in freqRep (best : (mutate seedsmut $ (crossover' seedscross crossoverpool) ++ clonepool)) where
        crossover' _ [] = []
        crossover' (s:srest) (a:b:prest) = (crossover s a b) ++ (crossover' srest prest)
        mutate _ [] = []
        mutate (s:srest) (o:orest) = mutation s (mpar :: Float) o : mutate srest orest

-- The function evolve performs the genetic algorithm.
evolve :: Organism a => Seed -> [(a,Frequency)] -> Int -> Int -> Parameter -> Parameter -> [(a,Frequency)]
evolve seed pop gensize nrgen cpar mpar = let
        seedsgen = map (`mod` 10000) (take nrgen (randoms $ mkStdGen seed :: [Int]))    
    in evolve' seedsgen pop nrgen where
        evolve' _ pop 0 = pop
        evolve' (s:seeds) pop k =
          evolve' seeds (createGen s pop gensize cpar mpar) (k-1)

