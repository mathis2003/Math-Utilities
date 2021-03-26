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



data DiophantineEquation = DiophantineEquation { dio_a :: Int,
                                                dio_b :: Int,
                                                dio_c :: Int }
    deriving Show
                                                
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



solve_diophantine_eq :: DiophantineEquation -> (Int, Int)
solve_diophantine_eq eq = substitute_to_dioph_solution( reverse (div_to_subst_eq (init_euclid_alg(eq)) (1) ([] :: [SubstEquation])))


init_euclid_alg :: DiophantineEquation -> [DivEquation]
init_euclid_alg eq = euclid_algorithm ( (DivEquation { eq_a = (dio_a eq),  eq_b = (dio_b eq), eq_q = new_q, eq_r = new_r}) : [] )
    where
        new_r = (dio_a eq) `mod` (dio_b eq)
        new_q = round((fromIntegral ((dio_a eq) - new_r)) / (fromIntegral (dio_b eq)))
        

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



substitute_to_dioph_solution :: [SubstEquation] -> (Int, Int)
substitute_to_dioph_solution subst_eq_list
    | length subst_eq_list == 1 = (subst_x (subst_eq_list !! 0), subst_y (subst_eq_list !! 0))
    | otherwise = substitute_to_dioph_solution (substitute_eq subst_eq_list : tail (tail subst_eq_list))

substitute_eq :: [SubstEquation] -> SubstEquation
substitute_eq list = SubstEquation {subst_a = new_a, subst_x = new_x, subst_b = new_b, subst_y = new_y, subst_c = new_c}
    where
        new_a = subst_a (list !! 1)
        new_x = subst_y (list !! 0) * subst_x (list !! 1)
        new_b = subst_b (list !! 1)
        new_y = subst_y (list !! 0) * subst_y (list !! 1) + subst_x (list !! 0)
        new_c = subst_c (list !! (length list - 1))



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

combination :: Int -> Int -> Int
combination n k = round(fromIntegral (factorial n) / (fromIntegral (factorial k) * fromIntegral (factorial (n-k))))

permutation :: Int -> Int -> Int
permutation n k = round((fromIntegral (factorial n)) / (fromIntegral (factorial (n-k))))
