main :: IO()
main = do
 print (solveQuadratic 1 4 4)
 print (sumPrimes 1 3)
 print (countPalindromes 731)
 print (truncatablePrime 3797)
 print (truncatablePrime 47)

{- Задача 1. Нека е дадено квадратно уравнение аx2 + bx + c = 0 , където a, b и c са реални числа. 
Дефинирайте функция solveQuadratic :: Double -> Double -> Double -> (Double, Double), която получава като аргументи, 
коефициентите a, b и c и връща двойката решения на уравнението или индикация за грешка, ако дискриминантата на уравнението е отрицателна.

Задача 2. Дефинирайте функция sumPrimes :: Integer -> Integer -> Integer , която приема целите числа n и k и връща 
сумата на първите k прости числа, по-големи или равни на n .

Задача 3. Ще наричаме едно цяло положително число палиндром, ако то е равно на числото, записано със същите цифри, 
но в обратен ред (приемаме, че числата са дефинирани в десетична бройна система).
Дефинирайте функция countPalindromes :: Integer -> Integer -> Integer, която приема аргументи a и b и връща броя на 
палиндромите в целочисления интервал [a, b], a ≤ b.

Задача 4. Дефинирайте предикат truncatablePrime :: Integer -> Bool , който връща стойност True точно когато аргументът 
num притежава едновременно следните свойства:
● числото num е просто;
● всички числа, които се получават чрез премахване на цифри в края на num, също са прости.
Примери:
truncatablePrime 3797 ⇢ True (числата 3797, 379, 37 и 3 са прости)
truncatablePrime 47 ⇢ False -}

solveQuadratic :: Double -> Double -> Double -> (Double, Double)
solveQuadratic a b c = if d < 0 then error "not defined" else (x,y)
                          where
                           x = e + sqrt d / (2 * a)
                           y = e - sqrt d / (2 * a)
                           d = b * b - 4 * a * c
                           e = - b / (2 * a)
                           
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = null [d | d <- [2..(n `div` 2)], n `mod` d == 0]

sumPrimes :: Integer -> Integer -> Integer
sumPrimes n 0 = 0
sumPrimes n k = 
 if (n < 2) then sumPrimes (n + 1) k 
 else if (isPrime n) then n + (sumPrimes (n + 1) (k - 1))
 else (sumPrimes (n + 1) k)
 
reverseNumber :: Integer -> Integer
reverseNumber n = helper n 0
 where
  helper rest acc
   | rest < 10 = rest + acc * 10
   | otherwise = helper (rest `div` 10) ((rest `mod` 10) + acc * 10)
    
 
countPalindromes :: Integer -> Integer -> Integer 
countPalindromes a b 
 |a > b = 0
 |a < 10 = 1 + (countPalindromes (a + 1) b)
 |(reverseNumber a) == a = 1 + (countPalindromes (a + 1) b)
 |otherwise = (countPalindromes (a + 1) b)
 
truncatablePrime :: Integer -> Bool
 truncatablePrime n 
  | ((isPrime n) && (n < 10)) = True
  | (isPrime n) = (truncatablePrime (n `div` 10))
  | otherwise = False
