import Data.List

main = do
 print (cheaperAlternative store2)
-- print (sumUnique [[1,2,3,2],[-4,-4],[5]])
{-
  print (hardestSubject 
   [("k", "a", 5.99),
    ("k", "b", 4.99),
    ("l", "a", 3.99),
    ("m", "a", 6.99),
    ("n", "b", 3.99),
    ("m", "b", 4.99),
    ("o", "c", 5.99)])
  print (reverseOrdSuff 327666)
-}
  
 
type Student = String  --име на ученик
type Subject = String  --име на предмет
type Note = Double     --оценка
 
--Запис за ученик, съдържащ име на ученик, учебен предмет и оценката на
--ученика по дадения предмет.
type Record = (Student, Subject, Note)
 
hardestSubject :: [Record] -> Subject
hardestSubject xs =
 findMin [(sbj, average (getNotes sbj)) | sbj <- subjects]
 where 
  subjects = nub [sbj | (_, sbj,_) <- xs]
  getNotes sbj = [note | (_, s, note) <- xs, s == sbj]
  average xs = sum xs / (fromIntegral (length xs))
  
  findMin :: [(String, Double)] -> String
  findMin [] = ""
  findMin (x:xs) = fst (helper xs x)
   where
    helper [] m = m
    helper ((xSbj, xAvg):xs) (mSbj, mAvg) =
     if xAvg < mAvg then helper xs (xSbj, xAvg)
                    else helper xs (mSbj, mAvg)
                    
reverseOrdSuff :: Int -> Int
reverseOrdSuff n = helper n 0
 where
  helper k res =
   if res `mod` 10 >= k `mod` 10 then res
    else helper (k `div` 10) (res * 10 + k `mod` 10)
    
sumUnique :: [[Int]] -> Int
sumUnique xss = sum [sum (getUnique xs) | xs <- xss]
 where
  getUnique xs = [x | x <- xs, length [x | y <- xs, x == y] == 1]
  
type Product = (String, Double)
type StoreAvailability = [Product]

store2 = [("bread",1),
          ("cheese",2.5),
          ("bread",1),
          ("cheese",5),
          ("butter",2.3)]

closestToAverage :: StoreAvailability -> String
closestToAverage ps = findClosest ps
 where
  average xs = sum xs / (fromIntegral (length xs))
  averagePrice = average [snd p | p <- ps]
  
  findClosest :: [Product] -> String
  findClosest [] = ""
  findClosest (p:ps) = helper ps p
   where
    helper [] (oName,_) = oName
    helper ((cName, cPrice):cs) (oName, oPrice) =
     if abs (cPrice - averagePrice) < abs (oPrice - averagePrice)
      then helper cs (cName, cPrice)
      else helper cs (oName, oPrice)

cheaperAlternative :: StoreAvailability -> Int
cheaperAlternative ps = length nonUniqueNames
 where
  pNames = nub [pName | (pName, _) <- ps]
  nonUniqueNames = 
   [pName | pName <- pNames,
     length (nub [pPrice | (pN, pPrice) <- ps, pN == pName]) > 1]
     
minDistance :: [(Double,Double,Double)] -> Double
minDistance ps =
 if length ps > length (nub ps) then 0
 else minimum [d p1 p2 | p1 <- ps, p2 <- ps, p1 /= p2]
  where
   d (x1, y1, z1) (x2, y2, z2) =
    (x1 - x2) * (x1 - x2) + 
    (y1 - y2) * (y1 - y2) + 
    (z1 - z2) * (z1 - z2) 

fstTriple :: (Double, Double, Double) -> Double
fstTriple (x, _, _) = x
