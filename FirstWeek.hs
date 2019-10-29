main :: IO()
main = do
 print 1
 print 2
 print (fact 10)
 print (fact 10000)
 print (mymin 2 5)
 print (inside 2 1 10)
 print (div 10 2)
 print (10 `div` 2)
 print (10.2 / (fromIntegral n))
 print (myfunc 2 5)

myfunc :: Double -> Double -> Double
myfunc a b = (a * a + b * b) / 2

n :: Int
n = 2

fact :: Integer -> Integer
fact n = if n <= 1 then 1 else n * (fact (n - 1))

mymin :: Int -> Int -> Int
mymin x y = if x < y then x else y

inside :: Int -> Int -> Int -> Bool
inside x a b = (a <= x) && (x <= b)
