import Data.List
import Data.Char

main :: IO()
main = do
  --print (group [1,1,2,3,3,3])
  --print (group' "aaaabbbcaaa")
  --print (sum2 10 11)
  print (f1 (f2 (f1 5)))
  print ((f1 . f2 . f1) 5)
  print (f2 10) -- -> 10 * 2 -> 20
  print ((\ x y -> x * 2 + y) 10 20)
  print (((\ x y -> x * 2 + y) 10) 20)
  print (f3 10 20)
  print (f4 15)
  print (((+ 2) . (* 2)) 10)
  print (map ((* 2) . (+ 2)) [1,2,3])
  print (map (\ x -> (x + 2) * 2) [1,2,3])
  print ([(x + 2) * 2 | x <- [1,2,3]])
  print (filter even [1..10])
  print ([x | x <- [1..10], even x])
  print (foldl (+) 0 [1,2,3])
  print (foldl1 (+) [1,2,3])
  print (foldl1 (++) [[1,2],[3,4],[5,6]])
  print (foldr (\ x a -> x ++ a) [] [[1,2],[3,4],[5,6]])
{-
f :: [Int] -> [[Int]] -> [[Int]]
f l1 l2 = map (\x -> (x:l1)) (map head l2)
Напишете оценката на следния израз:
f [10,20,30,40] [[1,2],[3,4],[5,6]]

(map head l2) -> (map head [[1,2],[3,4],[5,6]])
-> [head [1,2], head [3,4], head [5,6]]
-> [1,3,5]
map (\x -> (x:[10,20,30,40])) [1,3,5]
-> [[1,10,20,30,40],[3,10,20,30,40],[5,10,20,30,40]]

(\x -> (x:[10,20,30,40]))
f15 x = x:[10,20,30,40]
map f15 [1,3,5]
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x) : (map' f xs)
-}
f1 :: Int -> Int
f1 = (\ x -> x + 2)

f2 :: Int -> Int
f2 x = x * 2

--f3 :: Int -> Int -> Int
f3 :: Int -> (Int -> Int)
f3 x y = x * 2 + y

f4 :: (Int -> Int)
f4 = f3 10

comp :: (a -> b) -> (c -> a) -> (c -> b)
--comp :: (a -> b) -> (c -> a) -> c -> b
--comp f g = (\ x -> (f (g x)))
comp f g x = f (g x)
--comp f g = f . g
group' :: Eq t => [t] -> [[t]]
group' (x:xs) = helper x 1 xs []
 where
  helper c count []     rs = rs ++ [replicate count c]
  helper c count (x:xs) rs =
    if c == x
    then helper c (count + 1) xs rs
    else helper x 1 xs (rs ++ [replicate count c])

sum2' :: Int -> Int -> Int
sum2' a b = a + b

sum2'' :: Double -> Double -> Double
sum2'' a b = a + b

sum2 :: Num t => t -> t -> t
sum2 a b = a + b
 
length' :: [t] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs
