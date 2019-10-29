import Data.Char
import Data.List

{- Задача 1. Да се дефинира функция checkNumber :: Integer -> (Integer, Integer), която намира сумата от цифрите с четни индекси 
и сумата от цифрите с нечетни индекси в дадено естествено число и връща двете суми като двойка, първият елемент на която 
съответства на сумата от цифрите с четни индекси. Приемаме, че цифрите на числото са индексирани от 0 и отляво надясно.
Примери:
checkNumber 2728 → (4,15)
checkNumber 31415 → (12,2)
checkNumber 121 → (2,2)

Задача 2. Според основната теорема на аритметиката, всяко естествено число, което е по-голямо от 2, може да се представи 
като произведение на прости числа. Да се дефинира функция factorize :: Int -> [Int], която приема естествено число, по-голямо 
от 1, и връща сортиран списък от елементите на това представяне.
Примери:
factorize 13 → [13] -- числото е просто и съответният списък съдържа един елемент
factorize 152 → [2, 2, 2,19] -- 2 * 2 * 2 * 19 = 152
factorize 123 → [3, 41] -- 3 * 41 = 123

Задача 3. Да се дeфинира функция myFilter :: Double -> [[Double]] -> [[Double]], която за дадени реално число x и списък xss, 
елементите на който са непразни списъци от числа, връща като резултат списък от тези елементи на xss, средното аритметично на 
чиито елементи е по-малко от x.
Пример:
myFilter 10 [[1,2,3],[100,200],[10]] → [[1.0,2.0,3.0]]

Задача 4. Run-length encoding е прост начин за компресия на текст, при който последователните срещания на един елемент (символ от текста) 
се заменят с <брой на срещания><елемент>, а в случай, че в резултат на тази замяна биха се получили поне толкова символи, колкото се 
съдържат в оригиналния текст, се запазва оригиналният текст. Например, ако имаме само едно срещане на буквата "а" и го заменим с "1а", 
то промененият текст ще има повече символи от оригиналния.
Да се дефинира функция encode :: String -> String, която компресира низ по описания метод. Да се дефинира и обратната функция decode :: String -> String.
Примери:
encode "Haskell" → "Haskell"
encode "aaabccdefff" → "3abccde3f"
encode "aaaaaaaaaaaabbb" → "12a3b"
decode "12a3b" → "aaaaaaaaaaaabbb" -}

checkNumber :: Integer -> (Integer, Integer)
checkNumber n = helper n 0 0 0
  where
    helper 0 fstSum sndSum count =
      if even count 
      then (fstSum, sndSum)
      else (sndSum, fstSum)
    helper k fstSum sndSum count =
      if odd count
      then helper (div k 10) (fstSum + mod k 10) sndSum (count + 1)
      else helper (div k 10) fstSum (sndSum + mod k 10) (count + 1)


factorize :: Int -> [Int]
factorize 1 = []
factorize n = helper 2
  where
    helper i =
      if mod n i == 0
      then i : factorize (div n i)
      else helper (i + 1)


myFilter :: Double -> [[Double]] -> [[Double]]
myFilter x xss = [xs | xs <- xss, sum xs / (fromIntegral (length xs)) < x]


encode :: String -> String
encode cs = concat [compress ss | ss <- group cs]
  where
    compress ss@[_] = ss
    compress ss@[_,_] = ss
    compress ss@(s:_) = show (length ss) ++ [s]

decode :: String -> String
decode cs = helper cs 0
  where
    helper "" _     = ""
    helper (c:cs) k
     | isDigit c = helper cs (k * 10 + digitToInt c)
     | k == 0    = c : helper cs 0
     | otherwise = replicate k c ++ helper cs 0

main :: IO()
main = do
  print "Task 1"
  print (checkNumber 0)         -- (0, 0)
  print (checkNumber 1)         -- (1, 0)
  print (checkNumber 12)        -- (1, 2)
  print (checkNumber 123)       -- (4, 2)
  print (checkNumber 1234)      -- (4, 6)
  print (checkNumber 12345)     -- (9, 6)
  print (checkNumber 123456)    -- (9, 12)
  print (checkNumber 1234567)   -- (16, 12)
  print (checkNumber 12345678)  -- (16, 20)
  print (checkNumber 123456789) -- (25, 20)

  print "Task 2"
  print (factorize 2)          -- [2]
  print (factorize 3)          -- [3]
  print (factorize 6)          -- [2,3]
  print (factorize 13)         -- [13]
  print (factorize 123)        -- [3,41]
  print (factorize 152)        -- [2,2,2,19]
  print (factorize 128)        -- [2,2,2,2,2,2,2]
  print (factorize 1024)       -- [2,2,2,2,2,2,2,2,2,2]
  print (factorize 4096)       -- [2,2,2,2,2,2,2,2,2,2,2,2]
  print (factorize 1134472500) -- [2,2,3,3,3,5,5,5,5,7,7,7,7,7]

  print "Task 3"
  print (myFilter 2 [[5]])                    -- []
  print (myFilter 7 [[5]])                    -- [[5.0]]
  print (myFilter 2 [[5], [7]])               -- []
  print (myFilter 5 [[5], [1, 8]])            -- [[1.0, 8.0]]
  print (myFilter 10 [[1], [5], [10], [11]])  -- [[1.0], [5.0]]
  print (myFilter 1 [[1], [1, 2], [1, 2, 3]]) -- []
  print (myFilter 2 [[1], [1, 2], [1, 2, 3]]) -- [[1.0], [1.0, 2.0]]
  print (myFilter 3 [[1], [1, 2], [1, 2, 3]]) -- [[1.0],[1.0,2.0],[1.0,2.0,3.0]]
  print (myFilter 4 [[1], [1, 2], [1, 2, 3]]) -- [[1.0],[1.0,2.0],[1.0,2.0,3.0]]
  print (myFilter 5 [[1], [1, 2], [1, 2, 3]]) -- [[1.0],[1.0,2.0],[1.0,2.0,3.0]]

  print "Task 4"
  print (encode "a")               -- "a"
  print (encode "Haskell")         -- "Haskell"
  print (encode "aaabccdefff")     -- "3abccde3f"
  print (encode "aaaaaaaaaaaabbb") -- "12a3b"
  print (encode "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabccdddeeee") -- "121abcc3d4e"
  print (decode "a")           -- "a"
  print (decode "Haskell")     -- "Haskell"
  print (decode "3abccde3f")   -- "aaabccdefff"
  print (decode "12a3b")       -- "aaaaaaaaaaaabbb"
  print (decode "121abcc3d4e") -- "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabccdddeeee"
  
  print "Task 1"
  print (checkNumber 0         == (0, 0))
  print (checkNumber 1         == (1, 0))
  print (checkNumber 12        == (1, 2))
  print (checkNumber 123       == (4, 2))
  print (checkNumber 1234      == (4, 6))
  print (checkNumber 12345     == (9, 6))
  print (checkNumber 123456    == (9, 12))
  print (checkNumber 1234567   == (16, 12))
  print (checkNumber 12345678  == (16, 20))
  print (checkNumber 123456789 == (25, 20))

  print "Task 2"
  print (factorize 2          == [2])
  print (factorize 3          == [3])
  print (factorize 6          == [2,3])
  print (factorize 13         == [13])
  print (factorize 123        == [3,41])
  print (factorize 152        == [2,2,2,19])
  print (factorize 128        == [2,2,2,2,2,2,2])
  print (factorize 1024       == [2,2,2,2,2,2,2,2,2,2])
  print (factorize 4096       == [2,2,2,2,2,2,2,2,2,2,2,2])
  print (factorize 1134472500 == [2,2,3,3,3,5,5,5,5,7,7,7,7,7])

  print "Task 3"
  print (myFilter 2 [[5]]                    == [])
  print (myFilter 7 [[5]]                    == [[5.0]])
  print (myFilter 2 [[5], [7]]               == [])
  print (myFilter 5 [[5], [1, 8]]            == [[1.0, 8.0]])
  print (myFilter 10 [[1], [5], [10], [11]]  == [[1.0], [5.0]])
  print (myFilter 1 [[1], [1, 2], [1, 2, 3]] == [])
  print (myFilter 2 [[1], [1, 2], [1, 2, 3]] == [[1.0], [1.0, 2.0]])
  print (myFilter 3 [[1], [1, 2], [1, 2, 3]] == [[1.0],[1.0,2.0],[1.0,2.0,3.0]])
  print (myFilter 4 [[1], [1, 2], [1, 2, 3]] == [[1.0],[1.0,2.0],[1.0,2.0,3.0]])
  print (myFilter 5 [[1], [1, 2], [1, 2, 3]] == [[1.0],[1.0,2.0],[1.0,2.0,3.0]])

  print "Task 4"
  print (encode "a"               == "a")
  print (encode "Haskell"         == "Haskell")
  print (encode "aaabccdefff"     == "3abccde3f")
  print (encode "aaaaaaaaaaaabbb" == "12a3b")
  print (encode "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabccdddeeee" == "121abcc3d4e")
  print (decode "a"           == "a")
  print (decode "Haskell"     == "Haskell")
  print (decode "3abccde3f"   == "aaabccdefff")
  print (decode "12a3b"       == "aaaaaaaaaaaabbb")
  print (decode "121abcc3d4e" == "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabccdddeeee")
