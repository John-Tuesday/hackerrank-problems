{-# LANGUAGE Haskell2010, DeriveFoldable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, InstanceSigs #-}

module Main where

import Control.Monad
import Data.Maybe
import Data.Ratio
import qualified Data.Sequence as Seq

data Line = Line {
    slopeOf :: Integer,
    yIntercept :: Integer
    }
    deriving(Show, Eq)

interceptionPoint :: Line -> Line -> Rational
interceptionPoint 
    l1@(Line {slopeOf = a1, yIntercept = b1})
    (Line {slopeOf = a2, yIntercept = b2})
    = (fromIntegral $ b1 - b2) % (fromIntegral $ a2 - a1)

data Building = Building {
    growLine :: Line,
    indexOf :: Int
    }
    deriving(Show, Eq)

buildBuildings :: Int -> [Integer] -> [Integer] -> [Building]
buildBuildings _ [] [] = []
buildBuildings i (h:hs) (m:ms) = b:(buildBuildings (i+1) hs ms)
    where
        b = Building { 
            growLine = Line { slopeOf = m, yIntercept = h },
            indexOf = i
            }

data Tallest = TallestPoint {
        iOf :: Int,
        dayOf :: Rational
    }
    | TallestRegion {
        iOf :: Int,
        startExcl :: Rational,
        endExcl :: Rational
    }
    | TallestEnd {
        iOf :: Int,
        startExcl :: Rational
    }
    deriving(Show, Eq)

query :: Integer -> Seq.Seq Tallest -> Int
query day intervals = iOf $ intervals `Seq.index` i 
    where
        i = fromJust $ Seq.findIndexR f intervals
        f (TallestPoint { dayOf = d }) = (fromInteger day) == d
        f (TallestEnd { startExcl = a }) = (fromInteger day) > a
        f (TallestRegion { startExcl = a, endExcl = b }) = day' > a && day' < b
            where day' = fromInteger day

buildIntervals :: Seq.Seq Building -> Building -> Rational -> Seq.Seq Tallest
buildIntervals
    bs
    bestSpan@(Building {
        growLine = spanLine@(Line { slopeOf = spanSlope }),
        indexOf = spanIndex
    })
    prevDay
    | Seq.length bsFiltered' <= 0 = TallestEnd { iOf = spanIndex, startExcl = prevDay } Seq.<| Seq.Empty
    | otherwise = intervals
    where
        interX = interceptionPoint spanLine
        bsFiltered = Seq.filter (\x -> indexOf x /= spanIndex && (slopeOf . growLine) x /= spanSlope) bs
        bsFiltered' = Seq.filter ((> prevDay) . snd) $
                (\x -> (x, interX (growLine x))) <$> bsFiltered
        bsInters@(
            (_, inter') Seq.:<| _
            ) = Seq.sortOn snd $ bsFiltered'
        nexts = Seq.takeWhileL (\(_, x) -> x == inter') bsInters
        (_ Seq.:|> (nextPoint, nextDay)) = Seq.sortOn (indexOf . fst) nexts 
        (_ Seq.:|> (nextSpan, _)) = Seq.sortOn (slopeOf . growLine . fst) nexts 

        spanRegion = TallestRegion { iOf = spanIndex, startExcl = prevDay, endExcl = nextDay }
        tallPoint = TallestPoint { iOf = max (indexOf nextPoint) spanIndex, dayOf = nextDay }
        intervals = spanRegion Seq.<| tallPoint Seq.<| buildIntervals bs nextSpan nextDay

findBestInit :: Seq.Seq Building -> Building -> (Building, Tallest)
findBestInit Seq.Empty curr = (curr, TallestPoint { iOf = indexOf curr, dayOf = 0 })
findBestInit
    (b@(Building {indexOf = bi, growLine = (Line {yIntercept = by})}) Seq.:<| bs)
    curr@(Building {indexOf = i, growLine = (Line {yIntercept = y})})
    | by > y = findBestInit bs b
    | by < y = findBestInit bs curr
    | by == y && bi > i = findBestInit bs b
    | otherwise = findBestInit bs curr

main = do
    nqLine <- getLine
    initialLine <- getLine
    slopesLine <- getLine
    let queries = (map (\x -> read x ::Integer) (words nqLine)) !! 1
        initialHeights = map (\x -> read x ::Integer) (words initialLine)
        slopes = map (\x -> read x ::Integer) (words slopesLine)
        buildings = Seq.fromList $ buildBuildings 1 initialHeights slopes
        (initialBest, initialTall) = findBestInit buildings (buildings `Seq.index` 0)
        intervals = initialTall Seq.<| buildIntervals buildings initialBest 0

    forM_ [1..queries] (\_ -> do
        tLine <- getLine
        let day = read tLine :: Integer
            ans = query day intervals
            
        print ans
        )

