import Data.List
import System.IO



----Prime Numbers----

isPrime :: Int -> Bool
isPrime n = aux n 2
    where
        aux n acc
            | n <= 1 = False
            | n <= acc = True
            | n `mod` acc == 0 = False
            | otherwise = aux n (acc+1)


extractPrimes :: [Int] -> [Int]
extractPrimes a = [ x | x <- a, isPrime x]



----Fibonacci and Factorial----

fibonacci :: Int -> [Int]
fibonacci n = aux 1 1 n []
    where
        aux a b n acc
            | length acc <  n = aux b (a+b) n (a : acc)
            | length acc >= n = reverse(acc)


factorial :: Int -> Int
factorial n =
    if n <= 1 then
        1
    else
        n * factorial (n-1)



----Mean----

mean :: [Int] -> Double
mean a = fromIntegral (foldr (+) 0 a) / fromIntegral (length a)



----Combinatorics----

combination :: Int -> Int -> Double
combination n k = fromIntegral (factorial n) / (fromIntegral (factorial k) * fromIntegral (factorial (n-k)))

permutation :: Int -> Int -> Double
permutation n k = (fromIntegral (factorial n)) / (fromIntegral (factorial (n-k)))
