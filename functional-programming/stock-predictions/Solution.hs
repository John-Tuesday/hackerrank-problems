{-# LANGUAGE Haskell2010, DeriveFoldable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, InstanceSigs #-}

module Main where

import Control.Monad
import Data.Maybe
import qualified Data.Sequence as Seq

data Block = Block {
    minOf :: Integer,
    maxOf :: Integer,
    numsOf :: Seq.Seq Integer
    }
    deriving(Show)

blockSizeOf :: Seq.Seq a -> Int
blockSizeOf xs = ceiling $ sqrt $ (fromIntegral (Seq.length xs) :: Double)

blockify :: Int -> Seq.Seq Integer -> Seq.Seq Block
blockify blockSize nums = blocks
    where
        chunks = Seq.chunksOf blockSize nums
        blocks = (\xs -> Block { minOf = minimum xs, maxOf = maximum xs, numsOf = xs }) <$> chunks

checkNumsB :: Integer -> Integer -> Int -> Seq.Seq Integer -> Int
checkNumsB _ _ count Seq.Empty = count
checkNumsB minValue maxValue count (xs Seq.:|> x) 
    | x < minValue || x > maxValue = count
    | otherwise = checkNumsB minValue maxValue (count + 1) xs

checkNumsF :: Integer -> Integer -> Int -> Seq.Seq Integer -> Int
checkNumsF _ _ count Seq.Empty = count
checkNumsF minValue maxValue count (x Seq.:<| xs) 
    | x < minValue || x > maxValue = count
    | otherwise = checkNumsF minValue maxValue (count + 1) xs

checkBlocksB :: Integer -> Integer -> Int -> Seq.Seq Block -> Int
checkBlocksB _ _ count Seq.Empty = count
checkBlocksB minValue maxValue count blocks@(xs Seq.:|> x) 
    | bMin < minValue || bMax > maxValue = checkNumsB minValue maxValue count bNums
    | otherwise = checkBlocksB minValue maxValue (count + blockSize) xs
    where
        (Block { minOf = bMin, maxOf = bMax, numsOf = bNums }) = x
        blockSize = Seq.length bNums

checkBlocksF :: Integer -> Integer -> Int -> Seq.Seq Block -> Int
checkBlocksF _ _ count Seq.Empty = count
checkBlocksF minValue maxValue count blocks@(x Seq.:<| xs) 
    | bMin < minValue || bMax > maxValue = checkNumsF minValue maxValue count bNums
    | otherwise = checkBlocksF minValue maxValue (count + blockSize) xs
    where
        (Block { minOf = bMin, maxOf = bMax, numsOf = bNums }) = x
        blockSize = Seq.length bNums

query :: Int -> Int -> Integer -> Seq.Seq Block -> Int
query blockSize elemIndex margin blocks = numCount + blockCount
    where
        (blockIndex, numIndex) = elemIndex `quotRem` blockSize
        elemBlock@(
            Block {
                    minOf = elemBlockMin,
                    maxOf = elemBlockMax,
                    numsOf = elemNums
                }
            ) = fromJust $ Seq.lookup blockIndex blocks
        minValue = fromJust $ Seq.lookup numIndex elemNums
        maxValue = minValue + margin

        selfBlockPass = elemBlockMax <= maxValue && elemBlockMin >= minValue
        (lnums, (_ Seq.:<| rnums)) = Seq.splitAt numIndex elemNums
        lnumCount = checkNumsB minValue maxValue 0 lnums
        rnumCount = checkNumsF minValue maxValue 0 rnums
        continueLeft = selfBlockPass || lnumCount == Seq.length lnums
        continueRight = selfBlockPass || rnumCount == Seq.length rnums
        (lhs, (_ Seq.:<| rhs)) = Seq.splitAt blockIndex blocks
        lCount = if continueLeft then checkBlocksB minValue maxValue 0 lhs else 0
        rCount = if continueRight then checkBlocksF minValue maxValue 0 rhs else 0

        numCount = if selfBlockPass then Seq.length elemNums else (lnumCount + rnumCount + 1)
        blockCount = lCount + rCount

main = do
    nLine <- getLine
    let arrayLength = read nLine :: Int

    arrLine <- getLine
    let nums  = Seq.fromList $ map (\x -> read x :: Integer) $ words arrLine
        blockSize = blockSizeOf nums
        blocks = blockify blockSize nums

    qLine <- getLine
    let querries = read qLine :: Integer

    forM_ [1..querries] (\_ -> do
        dmLine <- getLine
        let nums = map (\x -> read x :: Integer) $ words dmLine
            elemIndex = fromIntegral $ nums !! 0
            margin = fromIntegral $ nums !! 1
            ans = query blockSize elemIndex margin blocks
        print ans
        )

