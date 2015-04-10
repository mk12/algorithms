-- Copyright 2015 Mitchell Kember. Subject to the MIT License.

import Data.Char (toLower, chr, ord)
import Data.List (genericLength, minimumBy)
import Data.Ord (comparing)
import qualified Data.Map as M

-- Relative letter frequencies of English (http://norvig.com/mayzner.html).
englishFreqs :: (Fractional a) => [a]
englishFreqs = map (/ 100) $
    [ 8.04, 1.48, 3.34, 3.82, 12.49, 2.40, 1.87, 5.05, 7.57, 0.16, 0.54, 4.07
    , 2.51, 7.23, 7.64, 2.14, 0.12, 6.28,  6.51, 9.28, 2.73, 1.05, 1.68, 0.23
    , 1.66 , 0.09]

-- Calculates the relative letter frequencies of a string, considering only
-- alphabetical characters. Returns a list of 26 frequencies.
relativeFreqs :: (Fractional a) => String -> [a]
relativeFreqs s = freqs
  where
    letters  = filter (`elem` ['a'..'z']) . map toLower $ s
    zeros    = M.fromDistinctAscList $ map (flip (,) 0) ['a'..'z']
    inc m x  = M.adjust (+ 1) x m
    counts   = M.elems $ foldl inc zeros letters
    divide n = fromIntegral n / genericLength letters
    freqs    = map divide counts

-- Computers Pearson's chi-squared test-statistic for the given expected
-- frequencies and observed frequencies.
chiSqr :: (Fractional a) => [a] -> [a] -> a
chiSqr es os = sum $ zipWith term es os
    where term e o = (o - e)^2 / e

-- Shifts a character around the alphabet, wrapping if necessary.
shift :: Int -> Char -> Char
shift n c
    | c `elem` ['a'..'z'] = chr $ ord 'a' + (ord c - ord 'a' + n) `mod` 26
    | c `elem` ['A'..'Z'] = chr $ ord 'A' + (ord c - ord 'A' + n) `mod` 26
    | otherwise           = c

-- Caesar-encrypts a string using the given key.
encrypt :: Int -> String -> String
encrypt = map . shift

-- Caesar-decrypts a string using the given key.
decrypt :: Int -> String -> String
decrypt = map . shift . negate

-- Rotates a list by the given number of positions. Elements move forward
-- through the list and ones that fall off the end return to the beginning.
rotate :: [a] -> Int -> [a]
rotate xs n = back ++ front where (front, back) = splitAt n xs

-- Returns the index of the minimum element in the list.
minIndex :: (Ord a) => [a] -> Int
minIndex = fst . minimumBy (comparing snd) . zip [0..]

-- Cracks a Caesar cipher given the ciphertext. Returns the key.
crack :: String -> Int
crack s = minIndex chis
  where
    freqs = relativeFreqs s
    chis  = map (chiSqr englishFreqs . rotate freqs) [0..25]
