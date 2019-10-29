main :: IO()
main = do
 print (sumOddSquares [1..10])

{-
  Задача 1.

  Напишете функция sumOddSquares :: [Int] -> Int, която намира сумата

  от квадратите на нечетните цели числа в даден списък. 
  Решете задачата по два начина - с map, filter, foldr и с list comprehension.
-}

sumOddSquares' :: [Int] -> Int
sumOddSquares' xs = sum [x * x | x <- xs, odd x]

sumOddSquares'' xs =
 foldr (+) 0 (map (\ x -> x * x) (filter (\ x -> mod x 2 == 1) xs))
 
sumOddSquares''' xs =
 foldr (+) 0 (map (^ 2) (filter odd xs))

sumOddSquares :: [Int] -> Int
sumOddSquares xs = ((foldr (+) 0) . (map (^ 2)) . (filter odd)) 

{-
  Задача 2.

  Да се дефинира процедура (composition fs), която по даден списък от

  едноаргументни реални функции fs = [f1,f2,…,fn] намира композицията на

  f1, f2, …, fn.
-}
composition' :: [(a -> a)] -> (a -> a)
composition' fs = (\ x -> helper fs x)
 where
  helper :: [(a -> a)] -> a -> a
  helper []     x = x
  helper (f:fs) x = f (helper fs x)
-- fs -> [f1, f2, f3, f4]
-- \ x -> f1 (f2 (f3 (f4 x)))
-- \ x -> (f1 . f2 . f3 . f4) x

composition :: [(a -> a)] -> (a -> a)
composition []     x = x
composition (f:fs) x = f (composition fs x)
--composition (f:fs) = f . (composition fs)
