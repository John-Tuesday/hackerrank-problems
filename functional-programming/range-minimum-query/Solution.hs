{-# LANGUAGE Haskell2010, DeriveFoldable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, InstanceSigs #-}

module Main where

import Control.Monad
import Data.Maybe
import qualified Data.Sequence as Seq

data SegTree a 
    = SegTree {
        rightTreeOf :: SegTree a,
        leftTreeOf :: SegTree a,
        startIndexOf :: Int,
        endIndexOf :: Int,
        valueOf :: a
        }
    | SegLeaf { 
        startIndexOf :: Int,
        endIndexOf :: Int,
        valueOf :: a 
        }
    deriving(Show, Eq)

mergeNodes :: Ord a => SegTree a -> SegTree a -> SegTree a
mergeNodes x y = SegTree {
        rightTreeOf = x,
        leftTreeOf = y,
        startIndexOf = i,
        endIndexOf = j,
        valueOf = min (valueOf x) (valueOf y)
    }
    where
        i = min (startIndexOf x) (startIndexOf y)
        j = max (endIndexOf x) (endIndexOf y)

buildSegTree :: Integral a => Seq.Seq a -> Int -> SegTree a
buildSegTree (x Seq.:<| Seq.Empty) i = SegLeaf { startIndexOf = i, endIndexOf = i, valueOf = x }
buildSegTree x i = tree
    where
        hlen = Seq.length x `quot` 2
        (xl, xr) = Seq.splitAt hlen x
        tree = mergeNodes (buildSegTree xl i) (buildSegTree xr (hlen + i))

query :: Ord a => Int -> Int -> SegTree a -> Maybe a
query i j (SegLeaf {startIndexOf = li, endIndexOf = lj, valueOf = v})
    | li >= i && li <= j && lj >= i && lj <= j = Just v
    | otherwise = Nothing
query i j 
    t@(SegTree {
        rightTreeOf = rt,
        leftTreeOf = lt,
        startIndexOf = li,
        endIndexOf = lj,
        valueOf = v
        }
    ) 
    | li >= i && li <= j && lj >= i && lj <= j = Just v
    | (li > i && li > j && lj > i && lj > j) || (li < i && li < j && lj < i && lj < j) = Nothing
    | otherwise = l
    where
        l = g (query i j lt) (query i j rt)
        g :: Ord a => Maybe a -> Maybe a -> Maybe a
        g Nothing y = y
        g x Nothing = x
        g x y = min x y

main = do
    nmLine <- getLine

    let nums = map (\x -> read x :: Int) $ words nmLine
        arrayLength = head nums
        querries = nums !! 1

    arrayLine <- getLine

    let array = Seq.fromList $ map (\x -> read x :: Int) $ words arrayLine
        tree = buildSegTree array 0

    forM_ ([1..querries]) (\i -> do
        lrLine <- getLine

        let ns = map (\x -> read x :: Int) $ words lrLine
            l = ns !! 0
            r = ns !! 1
            ans = fromJust $ query l r tree

        print ans
        )

