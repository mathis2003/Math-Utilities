import Data.List
import System.IO



----General-----

factorial :: Int -> Int
factorial n =
    if n <= 1 then
        1
    else
        n * factorial (n-1)



----Number Theory----

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

                                                
data DivEquation = DivEquation { eq_a :: Int,
                                 eq_b :: Int,
                                 eq_q :: Int,
                                 eq_r :: Int }
    deriving Show

data SubstEquation = SubstEquation { subst_a :: Int,
                                     subst_x :: Int,
                                     subst_b :: Int,
                                     subst_y :: Int,
                                     subst_c :: Int }
    deriving Show



solve_diophantine_eq :: Int -> Int -> Int -> (Int, Int)
solve_diophantine_eq a b c = substitute_to_dioph_solution( reverse (div_to_subst_eq (init_euclid_alg a b) (1) ([] :: [SubstEquation]))) c


init_euclid_alg :: Int -> Int -> [DivEquation]
init_euclid_alg a b = euclid_algorithm ( (DivEquation { eq_a = a,  eq_b = b, eq_q = new_q, eq_r = new_r}) : [] )
    where
        new_r = a `mod` b
        new_q = round((fromIntegral (a - new_r)) / (fromIntegral b))
        

euclid_algorithm :: [DivEquation] -> [DivEquation]
euclid_algorithm list
    | new_r == 0 = (DivEquation { eq_a = new_a, eq_b = new_b, eq_q = new_q, eq_r = new_r } : list)
    | otherwise = euclid_algorithm (DivEquation { eq_a = new_a, eq_b = new_b, eq_q = new_q, eq_r = new_r } : list)
        where
            new_a = eq_b (list !! 0)
            new_b = eq_r (list !! 0)
            new_r = new_a `mod` new_b
            new_q = round((fromIntegral (new_a - new_r)) / (fromIntegral (new_b)))
            

div_to_subst_eq :: [DivEquation] -> Int -> [SubstEquation] -> [SubstEquation]
div_to_subst_eq div_list acc subst_list
    | acc >= length div_list = subst_list
    | otherwise = div_to_subst_eq div_list (acc+1) (SubstEquation { subst_a = new_a, subst_x = new_x, subst_b = new_b, subst_y = new_y, subst_c = new_c} : subst_list)
        where
            new_a = eq_a (div_list !! acc)
            new_x = 1
            new_b = eq_b (div_list !! acc)
            new_y = (-1) * eq_q (div_list !! acc)
            new_c = eq_r (div_list !! acc)



substitute_to_dioph_solution :: [SubstEquation] -> Int -> (Int, Int)
substitute_to_dioph_solution subst_eq_list c
    | length subst_eq_list == 1 = (subst_x (subst_eq_list !! 0) * mult_int, subst_y (subst_eq_list !! 0) * mult_int)
    | otherwise = substitute_to_dioph_solution (substitute_eq subst_eq_list : tail (tail subst_eq_list)) c
        where
            mult_int = round(fromIntegral(c) / fromIntegral(subst_c (subst_eq_list !! 0)))

substitute_eq :: [SubstEquation] -> SubstEquation
substitute_eq list = SubstEquation {subst_a = new_a, subst_x = new_x, subst_b = new_b, subst_y = new_y, subst_c = new_c}
    where
        new_a = subst_a (list !! 1)
        new_x = subst_y (list !! 0) * subst_x (list !! 1)
        new_b = subst_b (list !! 1)
        new_y = subst_y (list !! 0) * subst_y (list !! 1) + subst_x (list !! 0)
        new_c = subst_c (list !! 0)


fibonacci :: Int -> [Int]
fibonacci n = aux 1 1 n []
    where
        aux a b n acc
            | length acc <  n = aux b (a+b) n (a : acc)
            | length acc >= n = reverse(acc)



----Probability----

mean :: [Int] -> Double
mean a = fromIntegral (foldr (+) 0 a) / fromIntegral (length a)



----Combinatorics----

combination :: Int -> Int -> Int
combination n k = round(fromIntegral (factorial n) / (fromIntegral (factorial k) * fromIntegral (factorial (n-k))))

permutation :: Int -> Int -> Int
permutation n k = round((fromIntegral (factorial n)) / (fromIntegral (factorial (n-k))))
