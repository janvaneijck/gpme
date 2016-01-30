{-# LANGUAGE FlexibleInstances #-}

module GA_Graphical_OT where
import Data.List
import Data.Ord (comparing)
import Data.Sequence hiding (splitAt, take, zip, sortBy, filter)
import Codec.Picture
import System.Random


class (Eq a) => Organism a where
    mutation :: Seed -> Parameter -> a -> a
    crossover :: Seed -> a -> a -> [a]
    orgToSeq :: a -> Width -> Height -> Seq [Pixel8]
    fitness :: a -> Width -> Height -> Seq [Pixel8] -> Int
    randomCircles :: Seed -> Int -> Width -> Height -> Int -> a

type Seed = Int
type Parameter = Float
type Frequency = Int
type Coordinate = Int
type Radius = Int
type Width = Int
type Height = Int
type ImageBW = [(Coordinate, Coordinate, Radius, Bool)] -- True stands for white, False stands for black
type ImageY = [(Coordinate, Coordinate, Radius, Pixel8)]
type ImageYA = [(Coordinate, Coordinate, Radius, Pixel8, Pixel8)]
type ImageRGB = [(Coordinate, Coordinate, Radius, Pixel8, Pixel8, Pixel8)]

-- Y: stands for relative luminance

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
initialSequence :: Int -> Width -> Height -> Seq [Pixel8]
initialSequence 1 width height = Data.Sequence.replicate (width*height) [255]
initialSequence 2 width height = Data.Sequence.replicate (width*height) [255,150]
initialSequence 3 width height = Data.Sequence.replicate (width*height) [255,255,255]
initialSequence _ width height = error "initialSequence ERROR: not the correct number of components"

-- circleCoord takes the dimensions of a picture and the properties for a circle (x and y coordinate 
-- and radius) and returns all pixels of the picture inside the said picture.
circleCoord :: Width -> Height -> Int -> Int -> Int -> [(Int,Int)]
circleCoord width height x y r = filter circle square where
    x' = fromIntegral x
    y' = fromIntegral y
    r' = fromIntegral r
    square = [(a,b) | a <- [0..width-1], b <- [0..height-1], a >= x, b >= y, a <= x + 2*r, b <= y + 2*r]
    circle (a,b) = fromIntegral b       < 
        sqrt (r'*r' - (fromIntegral a - (x' + r'))*(fromIntegral a - (x' + r'))) + (y' + r') 
                && fromIntegral (-b)    < 
        sqrt (r'*r' - (fromIntegral a - (x' + r'))*(fromIntegral a - (x' + r'))) - (y' + r')

-- randomPop gives a random population of a certain organism with the used parameters
randomPop :: Organism a => Seed -> Int -> Width -> Height -> Int -> Int -> [(a,Frequency)]
randomPop seed popsize width height radius nrcircles = let
    seeds = take popsize (randoms $ mkStdGen seed :: [Int])
  in map (\s -> (randomCircles seed width height radius nrcircles,1)) seeds 

---------------------------------- GENETIC ALGORITHM -------------------------------

instance Organism ImageBW where
  mutation seed p organism = let
        coinflips = randomRange seed (0.0, 1.0) (4 * Data.List.length organism)
        differences = randomRange seed (-50,50) (3 * Data.List.length organism) :: [Int]
    in mutation' coinflips differences organism where
        mutation' _ _ [] = []
        mutation' (fx:fy:fr:fc:restflips) (dx:dy:dr:restdiff) ((x,y,r,c):restbits) = let
                newx = if fx < p then x + dx else x
                newy = if fy < p then y + dy else y
                newr = if fr < p then r else r
                newc = if fc < p then not c else c
            in (newx,newy,newr,newc) : mutation' restflips restdiff restbits

  crossover randomnr p1 p2 = let
        maxSize = max (Data.List.length p1) (Data.List.length p2)
        cut = randomnr `mod` (maxSize - 1) + 1
        (p11, p12) = splitAt cut p1
        (p21, p22) = splitAt cut p2
    in [p11 ++ p22, p21 ++ p12]

  orgToSeq org width height = orgToSeq' org (initialSequence 1 width height) where
    orgToSeq' [] imSeq = imSeq
    orgToSeq' ((x, y, r, c):rest) imSeq = orgToSeq' rest (updateSeq (circleCoord width height x y r) imSeq) where
        updateSeq [] ss = ss
        updateSeq ((a,b):rest) ss = updateSeq rest (update (a + b*width) (if c then [0] else [255]) ss)

  fitness org width height refSeq = compareSequences (width*height-1) where
    orgSeq = orgToSeq org width height
    compareSequences 0 = let
        [lum1] = index refSeq 0
        [lum2] = index orgSeq 0
      in (abs (fromIntegral lum1 - fromIntegral lum2))^3
    compareSequences n = let
        [lum1] = index refSeq n
        [lum2] = index orgSeq n
      in (abs (fromIntegral lum1 - fromIntegral lum2))^3 + compareSequences (n-1)

  randomCircles seed width height radius nrcircles = let
        seeds = take (3*nrcircles) (randoms $ mkStdGen seed :: [Int])
    in constructCircles seeds where
        constructCircles [] = []
        constructCircles (x:y:c:rest) = (x `mod` width,y `mod` height,5, c `mod` 2 == 1) : constructCircles rest

instance Organism ImageY where
  mutation seed p organism = let
        coinflips = randomRange seed (0.0, 1.0) (4 * Data.List.length organism)
        differences = randomRange seed (-50,50) (4 * Data.List.length organism) :: [Int]
    in mutation' coinflips differences organism where
        mutation' _ _ [] = []
        mutation' (fx:fy:fr:fc:restflips) (dx:dy:dr:dc:restdiff) ((x,y,r,c):restbits) = let
                newx = if fx < p then x + dx else x
                newy = if fy < p then y + dy else y
                newr = if fr < p then r else r
                newc = if fc < p then c + (fromIntegral dc :: Pixel8) else c
            in (newx,newy,newr,newc) : mutation' restflips restdiff restbits

  crossover randomnr p1 p2 = let
        maxSize = max (Data.List.length p1) (Data.List.length p2)
        cut = randomnr `mod` (maxSize - 1) + 1
        (p11, p12) = splitAt cut p1
        (p21, p22) = splitAt cut p2
    in [p11 ++ p22, p21 ++ p12]

  orgToSeq org width height = orgToSeq' org (initialSequence 2 width height) where
    orgToSeq' [] imSeq = imSeq
    orgToSeq' ((x, y, r, c):rest) imSeq = orgToSeq' rest (updateSeq (circleCoord width height x y r) imSeq) where
        updateSeq [] ss = ss
        updateSeq ((a,b):rest) ss = updateSeq rest (update (a + b*width) [c] ss)

  fitness org width height refSeq = compareSequences (width*height-1) where
    orgSeq = orgToSeq org width height
    compareSequences 0 = let
        [lum1] = index refSeq 0
        [lum2] = index orgSeq 0
      in (abs (fromIntegral lum1 - fromIntegral lum2))^3
    compareSequences n = let
        [lum1] = index refSeq n
        [lum2] = index orgSeq n
      in (abs (fromIntegral lum1 - fromIntegral lum2))^3 + compareSequences (n-1)

  randomCircles seed width height radius nrcircles = let
        seeds = take (3*nrcircles) (randoms $ mkStdGen seed :: [Int])
    in constructCircles seeds where
        constructCircles [] = []
        constructCircles (x:y:c:rest) = (x `mod` width,y `mod` height,5,fromIntegral c :: Pixel8) : constructCircles rest

instance Organism ImageYA where
  mutation seed p organism = let
        coinflips = randomRange seed (0.0, 1.0) (5 * Data.List.length organism)
        differences = randomRange seed (-50,50) (5 * Data.List.length organism) :: [Int]
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

  fitness org width height refSeq = compareSequences (width*height-1) where
    orgSeq = orgToSeq org width height
    compareSequences 0 = let
        [lum1,alpha1] = index refSeq 0
        [lum2,alpha2] = index orgSeq 0
      in (abs (fromIntegral lum1 - fromIntegral lum2))^3
    compareSequences n = let
        [lum1,alpha1] = index refSeq n
        [lum2,alpha2] = index orgSeq n
      in (abs (fromIntegral lum1 - fromIntegral lum2))^3 + compareSequences (n-1)

  randomCircles seed width height radius nrcircles = let
        seeds = take (3*nrcircles) (randoms $ mkStdGen seed :: [Int])
    in constructCircles seeds where
        constructCircles [] = []
        constructCircles (x:y:l:rest) = (x `mod` width,y `mod` height,5,fromIntegral l :: Pixel8,200 :: Pixel8) : constructCircles rest



instance Organism ImageRGB where
  mutation seed p organism = let
        coinflips = randomRange seed (0.0, 1.0) (6 * Data.List.length organism)
        differences = randomRange seed (-50,50) (6 * Data.List.length organism) :: [Int]
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

  fitness org width height refSeq = compareSequences (width*height-1) where
    orgSeq = orgToSeq org width height
    compareSequences 0 = let
        [r1,g1,b1] = index refSeq 0
        [r2,g2,b2] = index orgSeq 0
      in (abs (fromIntegral r1 - fromIntegral r2))^3
    compareSequences n = let
        [r1,g1,b1] = index refSeq n
        [r2,g2,b2] = index orgSeq n
      in (abs (fromIntegral r1 - fromIntegral r2))^3 + compareSequences (n-1)

  randomCircles seed width height radius nrcircles = let
        seeds = take (5*nrcircles) (randoms $ mkStdGen seed :: [Int])
    in constructCircles seeds where
        constructCircles [] = []
        constructCircles (x:y:r:g:b:rest) = (x `mod` width,y `mod` height,5,fromIntegral r :: Pixel8,fromIntegral g :: Pixel8, fromIntegral b :: Pixel8) : constructCircles rest


-- The function reproduciton performs the reproduction using a roulette wheel
-- reproduction technique.
reproduction :: Organism a => Seed              -- seed
                            -> [(a,Frequency)]  -- population of the previous generation
                            -> Int              -- generation size
                            -> Width            -- width of the to be replicated image
                            -> Height           -- height of the to be replicated image
                            -> Seq [Pixel8]     -- pixel information of the to be replicated image
                            -> [a]              -- reproduction pool
reproduction seed pop size width height refSeq = let
        fitnesspop  = zip pop (map ((\o -> fitness o width height refSeq) . fst) pop)
        totalfit    = sum $ map (\((_,freq),fit) -> fromIntegral freq / fromIntegral fit) fitnesspop
        fitProb     = map (\((_,freq),fit) -> fromIntegral freq / (fromIntegral fit * totalfit) ) fitnesspop
        popInterval = zip (map fst pop) (cumList fitProb)
        coinflips   = randomRange seed (0.0, 1.0) (size-1) :: [Float]

        bestPair    = minimumBy (comparing snd) fitnesspop
        best        = fst $ fst bestPair
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
createGen :: Organism a => Seed                 -- seed
                          -> [(a,Frequency)]    -- population of the previous generation
                          -> Int                -- generation size
                          -> Parameter          -- crossover parameter
                          -> Parameter          -- mutation parameter
                          -> Width              -- width of the to be replicated image
                          -> Height             -- height of the to be replicated image
                          -> Seq [Pixel8]       -- pixel information of the to be replicated image
                          -> [(a,Frequency)]    -- population for the next generation
createGen seed pop gensize cpar mpar width height refSeq = let
        [(seedPool, seedCross, seedMut)] = tripleUp $ map 
                                           (`mod` 10000)
                                           (take 3 (randoms $ mkStdGen seed :: [Int]))
        (best:pool) = reproduction seedPool pop gensize width height refSeq
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
evolve :: Organism a => Seed              -- the seed
                      -> [(a,Frequency)]  -- the initial population
                      -> Int              -- the generation size
                      -> Int              -- the number of generations you want the algorithm to run
                      -> Parameter        -- crossover parameter
                      -> Parameter        -- mutation parameter
                      -> Width            -- width of the image to replicate
                      -> Height           -- height of the image to replicate
                      -> Seq [Pixel8]     -- pixel information of the image to replicate
                      -> [(a,Frequency)]  -- population after the given amount of generations of running
evolve seed pop gensize nrgen cpar mpar width height refSeq = let
        seedsgen = map (`mod` 10000) (take nrgen (randoms $ mkStdGen seed :: [Int]))    
    in evolve' seedsgen pop nrgen where
        evolve' _ pop 0 = pop
        evolve' (s:seeds) pop k = evolve' seeds (createGen s pop gensize cpar mpar width height refSeq) (k-1)


------------------------------------------- EXAMPLES ----------------------------------------

writeResult :: FilePath -> FilePath -> IO ()
writeResult outpath refpath = do
    loadedimage <- readPng refpath
    case loadedimage of
        Left errorMsg -> error errorMsg
        Right image ->
            case image of
                ImageY8 sampleImage -> do
                        let width = imageWidth sampleImage
                        let height = imageHeight sampleImage
                        let outcome = evolve 1 
                                          [(randomCircles 16465 width height 4 1000 :: ImageY,1),
                                              (randomCircles 6545 width height 4 1000 :: ImageY,1)] 
                                          10 2 0.2 0.01 width height (imgToSeqY sampleImage)
                        writePng outpath (seqToImageY (orgToSeq (fst $ head outcome) width height) width height)
                ImageYA8 sampleImage -> do
                        let width = imageWidth sampleImage
                        let height = imageHeight sampleImage
                        let outcome = evolve 1 
                                          [(randomCircles 16465 width height 4 1000 :: ImageYA,1),
                                              (randomCircles 6545 width height 4 1000 :: ImageYA,1)] 
                                          10 2 0.2 0.01 width height (imgToSeqYA sampleImage)
                        writePng outpath (seqToImageYA (orgToSeq (fst $ head outcome) width height) width height)
                ImageRGB8 sampleImage -> do
                        let width = imageWidth sampleImage
                        let height = imageHeight sampleImage
                        let outcome = evolve 1 
                                          [(randomCircles 16465 width height 4 1000 :: ImageRGB,1),
                                              (randomCircles 6545 width height 4 1000 :: ImageRGB,1)] 
                                          10 2 0.2 0.01 width height (imgToSeqRGB sampleImage)
                        writePng outpath (seqToImageRGB (orgToSeq (fst $ head outcome) width height) width height)
                otherwise -> error "File not in ImageY8, ImageYA8 or ImageRGB format" 

writeResultBW :: FilePath -> FilePath -> IO ()
writeResultBW outpath refpath = do
    loadedimage <- readPng refpath
    case loadedimage of
        Left errorMsg -> error errorMsg
        Right image ->
            case image of
                ImageY8 sampleImage -> do
                        let width = imageWidth sampleImage
                        let height = imageHeight sampleImage
                        let outcome = evolve 1 
                                          [(randomCircles 16465 width height 4 1000 :: ImageBW,1),
                                              (randomCircles 6545 width height 4 1000 :: ImageBW,1)] 
                                          10 2 0.2 0.01 width height (imgToSeqY sampleImage)
                        writePng outpath (seqToImageY (orgToSeq (fst $ head outcome) width height) width height)
                otherwise -> error "File not in ImageY8 format"

-- the imgToSeq functions are here to convert an image of a certain type to a sequence in which all data of the pixels
-- are stored. One needs different functions for each type, as each pixel type has a different amount of components. There
-- are functions for a grayscale image (Y), a grayscale image with an alpha channel (YA) and a colour image (RGB).
imgToSeqY :: Image Pixel8 -> Seq [Pixel8]
imgToSeqY image = pixelbypixel (width-1) (height-1) where
    width = imageWidth image
    height = imageHeight image
    pixelbypixel 0 0 = let
            lum = pixelAt image 0 0
        in singleton [lum]
    pixelbypixel 0 y = let
            lum = pixelAt image 0 y
        in pixelbypixel (width-1) (y-1) |> [lum]
    pixelbypixel x y = let
            lum = pixelAt image x y
        in pixelbypixel (x-1) y |> [lum]

imgToSeqYA :: Image PixelYA8 -> Seq [Pixel8]
imgToSeqYA image = pixelbypixel (width-1) (height-1) where
    width = imageWidth image
    height = imageHeight image
    pixelbypixel 0 0 = let
            PixelYA8 lum alpha = pixelAt image 0 0
        in singleton [lum,alpha]
    pixelbypixel 0 y = let
            PixelYA8 lum alpha = pixelAt image 0 y
        in pixelbypixel (width-1) (y-1) |> [lum,alpha]
    pixelbypixel x y = let
            PixelYA8 lum alpha = pixelAt image x y
        in pixelbypixel (x-1) y |> [lum,alpha]

imgToSeqRGB :: Image PixelRGB8 -> Seq [Pixel8]
imgToSeqRGB image = pixelbypixel (width-1) (height-1) where
    width = imageWidth image
    height = imageHeight image
    pixelbypixel 0 0 = let
            PixelRGB8 r g b = pixelAt image 0 0
        in singleton [r,g,b]
    pixelbypixel 0 y = let
            PixelRGB8 r g b = pixelAt image 0 y
        in pixelbypixel (width-1) (y-1) |> [r,g,b]
    pixelbypixel x y = let
            PixelRGB8 r g b = pixelAt image x y
        in pixelbypixel (x-1) y |> [r,g,b]

-- the seqToImage functions do exactly the opposite of the imgToSeq functions, i.e. converting a sequence with
-- pixel information to its corresponding image. Again we need one for each type of image.
seqToImageY :: Seq [Pixel8] -> Width -> Height -> Image Pixel8
seqToImageY pixels width height = generateImage extractPixels width height where
    extractPixels x y = grayscale where
      [grayscale] = index pixels (x+y*width)

seqToImageYA :: Seq [Pixel8] -> Width -> Height -> Image PixelYA8
seqToImageYA pixels width height = generateImage extractPixels width height where
    extractPixels x y = PixelYA8 lum alpha where
      [lum,alpha] = index pixels (x+y*width)

seqToImageRGB :: Seq [Pixel8] -> Width -> Height -> Image PixelRGB8
seqToImageRGB pixels width height = generateImage extractPixels width height where
    extractPixels x y = PixelRGB8 r g b where
      [r,g,b] = index pixels (x+y*width)

