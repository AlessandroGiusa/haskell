-- Module for learning Haskell
-- Alessandro Giusa
-- some useful functions

import Data.Char (digitToInt)
import Data.Char (intToDigit)
       
-- generic function from base to decimal
toNum :: [Char] -> Int -> (Char -> Int) -> Int
toNum [] base map  = 0
toNum s  base  map = base * toNum (init(s)) base map + map(last(s))

-- generic function from decimal to base k
toKBaseNum :: Int -> Int -> (Int -> Char) -> [Char]
toKBaseNum x base map | x < base = [map x]
                  | otherwise = toKBaseNum (x `div` base) base map ++ [map(x `mod` base)]


-- mapping function for hex to decimal
mapHexToDec :: Char -> Int
mapHexToDec x | x == 'A' = 10
              | x == 'B' = 11
              | x == 'C' = 12
              | x == 'D' = 13
              | x == 'E' = 14
              | x == 'F' = 15
              | otherwise = digitToInt(x) :: Int
           
-- map decimal to hex
mapDecToHex :: Int -> Char
mapDecToHex x | x < 10 = intToDigit(x)
              | x == 10 = 'A'
              | x == 11 = 'B'
              | x == 12 = 'C'
              | x == 13 = 'D'
              | x == 14 = 'E'
              | x == 15 = 'F'

-- hex to decimal
hexToDec :: String -> Int
hexToDec [] = 0
hexToDec s = toNum s 16 mapHexToDec

-- binary to decimal
binToDec :: String -> Int
binToDec [] = 0
binToDec s = toNum s 2 (\x -> if x == '0' then 0 else 1)

-- decimal to binary
decToBin :: Int -> String
decToBin x = toKBaseNum x 2 (\x -> if x == 1 then '1' else '0')

-- decimal to hex
decToHex :: Int -> String
decToHex x = toKBaseNum x 16 mapDecToHex

-- decimal to octal
decToOct :: Int -> String
decToOct x = toKBaseNum x 8 (\x -> intToDigit(x))



