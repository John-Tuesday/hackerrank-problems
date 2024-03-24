{-# LANGUAGE Haskell2010, DeriveFoldable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, InstanceSigs #-}

module Main where

import Data.Maybe
import qualified Data.Sequence as Seq

data Section = Section
    {
        startIndex :: Int,
        endIndex :: Int,
        minHeightOf :: Int
    }
    deriving(Show, Eq)

widthOf :: Integral a  => Section -> a
widthOf (Section { startIndex = i, endIndex = j }) = fromIntegral (j-i) + 1

areaOf :: Integral a => Section -> a
areaOf x@(Section { minHeightOf = h }) = fromIntegral h * widthOf x

isLeftOf :: Section -> Section -> Bool
isLeftOf l@(Section {endIndex = lj}) r@(Section {startIndex = ri}) = (lj + 1) == ri

isRightOf :: Section -> Section -> Bool
isRightOf r l = l `isLeftOf` r 

isAdjacent :: Section -> Section -> Bool
isAdjacent x y = (x `isLeftOf` y) || (x `isRightOf` y)

isOverlapped :: Section -> Section -> Bool
isOverlapped 
    (Section {startIndex = li, endIndex = lj})
    (Section {startIndex = ri, endIndex = rj})
    = (ri >= li && rj <= li) || (ri >= lj && rj <= lj) 
        || (li >= ri && lj <= ri) || (li >= rj && lj <= rj)

combineUnsafe :: Section -> Section -> Section
combineUnsafe x y = Section { 
    startIndex = min (startIndex x) (startIndex y),
    endIndex = max (endIndex x) (endIndex y),
    minHeightOf = min (minHeightOf x) (minHeightOf y)
}

combine :: Section -> Section -> Maybe Section
combine x y = if isAdjacent x y || isOverlapped x y then Just (combineUnsafe x y) else Nothing

-- Combine if and only if height of the first Section will not decrease and both sections are adjacent
growCombine :: Section -> Section -> Maybe Section
growCombine x y = if h then combine x y else Nothing
    where h = minHeightOf x <= minHeightOf y

maxAreaOf :: Integral a => Seq.Seq Section -> a
maxAreaOf x = foldl f 0 x
    where
        f :: Integral a => a -> Section -> a
        f n sect = max n (areaOf sect)

toSections :: Integral a => Seq.Seq a -> Seq.Seq Section
toSections xs = Seq.mapWithIndex f xs
    where
        f :: Integral a => Int -> a -> Section
        f i h = Section { startIndex = i, endIndex = i, minHeightOf = fromIntegral h }

-- Keep combining from the start of the sequence until height decreases
-- Return the result and the leftovers of the sequence
lextendSection :: Section -> Seq.Seq Section -> (Section, Seq.Seq Section)
lextendSection x (Seq.Empty) = (x, Seq.Empty)
lextendSection x1 x@(x2 Seq.:<| xs) = x'
    where
        x' = maybe (x1, x) (\y -> lextendSection y xs) (growCombine x1 x2)

-- Same as extendSection but in reverse
rextendSection :: Section -> Seq.Seq Section -> (Section, Seq.Seq Section)
rextendSection x xs = lextendSection x (Seq.reverse xs) -- reverse is O(n)

bextendSection :: Section -> Seq.Seq Section -> Section
bextendSection sect sections = sect'
    where
        i = startIndex sect
        (lhs', rhs) = Seq.splitAt i sections
        lhs = Seq.deleteAt i lhs'

        (lsect, _) = rextendSection sect lhs
        (rsect, _) = lextendSection sect rhs
        sect' = combineUnsafe lsect rsect

solve :: Seq.Seq Integer -> Integer
solve hs = l
    where
        sections = toSections hs
        l = maxAreaOf $ fmap (\s -> bextendSection s sections) sections

main :: IO ()
main = do
    nLine <- getLine
    fLine <- getLine

    let fences = Seq.fromList $ map (\x -> read x :: Integer) $ words fLine
        ans = solve fences

    print ans

