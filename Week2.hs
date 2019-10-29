main :: IO()
main = do
 print ("123")
 print (fibRec 5)
 print (fibIter 500)
 print (isAscending 7857)
 print (countOccurences 457889 8)
 print (isPerfectNumber 6)

fibRec :: Integer -> Integer
fibRec 0 = 1
fibRec 1 = 1
fibRec n = fibRec (n - 2) + fibRec (n - 1)

fibIter :: Integer -> Integer
fibIter n = fibHelper n 1 0 0

fibHelper :: Integer -> Integer -> Integer -> Integer -> Integer
fibHelper n res pre i =
 if i <= n then fibHelper n (res + pre) res (i + 1) else res
  
{-
  Зад. 1. Да се дефинира функция countDigits, която генерира линейно рекурсивен
  процес и намира броя на цифрите на дадено естествено число.
-}
countDigits :: Int -> Int
countDigits n =
 if n < 10 then 1 else 1 + countDigits (n `div` 10)



{-
  Зад. 2. Да се дефинира функция sumDigits, която генерира линейно рекурсивен 
  процес и намира сумата от цифрите на дадено естествено число.
-}
sumDigits :: Int -> Int
sumDigits n =
 if n < 10 then n else n `mod` 10 + sumDigits (n `div` 10)


{-
  Зад. 3. Да се дефинира функция pow, която генерира линейно рекурсивен процес 
  и намира x на степен n, където x е реално, а n - естествено число.
-}
pow :: Double -> Int -> Double
pow x n = 
 if n == 0 then 1 else x * pow x (n - 1)


{-
  Зад. 4. Да се дефинира функция sumDigitsIterative, която генерира линейно 
  итеративен процес и намира сумата от цифрите на дадено естествено число.
-}
sumDigitsIterative :: Int -> Int
sumDigitsIterative n = sumIter n 0

sumIter :: Int -> Int -> Int
sumIter k sum =
 if k < 10 then sum + k else sumIter (k `div` 10) (sum + (k `mod` 10))


{-
  Зад. 5. Да се дефинира функция reverseNumber, която генерира линейно итеративен
  процес и по дадено естествено число n намира числото, записано със същите цифри,
  но в обратен ред.
-}
reverseNumber :: Int -> Int
reverseNumber _ = undefined


{-
  Зад. 6. Да се дефинира предикат isPrime, който проверява дали дадено естествено
  число е просто.
  Забележка: Числото 1 не е нито просто, нито съставно.
-}
isPrime :: Int -> Bool
isPrime _ = undefined


{-
  Зад. 7. Да се напише предикат isAscending, който връща истина, ако цифрите на
  дадено естествено число са в нарастващ ред от първата към последната.
-}
isAscending :: Int -> Bool
isAscending n
|n < 10 = True
|(n `mod` 10) > ((n `mod` 100) `div` 10) = isAscending (n `div` 10)
|otherwise = False


{-
  Зад. 8. Да се напише функция countOccurences, намираща броя на срещанията на дадена
  цифра d в записа на число n.
-}
countOccurences :: Int -> Int -> Int
countOccurences d n = undefined

countOccurences :: Int -> Int -> Int
countOccurences n d
|n < 10 = if(n == d) then 1 else 0
|n `mod` 10 == d =1 + countOccurences (n `div` 10) d
|otherwise =countOccurences (n `div` 10) d 


{-
  Зад. 9. Да се напише предикат isPerfectNumber, който връща дали едно число е
  съвършено, т.е. равно на сумата от делите си.
-}
isPerfectNumber :: Int -> Bool
isPerfectNumber _ = undefined

isPerfectNumber :: Int -> Bool
isPerfectNumber n =(n == hisPerfectNumber 1 n)
where
hisPerfectNumber :: Int -> Int -> Int
hisPerfectNumber i n
|i == n =0
|n `mod` i ==0 =i + hisPerfectNumber(i + 1) n
|otherwise =hisPerfectNumber(i + 1) n


{-
  Зад. 10. Да се дефинира функция sumPrimeDivisors, която намира сумата на всички
  прости делители на едно число.
-}
sumPrimeDivisors :: Int -> Int
sumPrimeDivisors _ = undefined
