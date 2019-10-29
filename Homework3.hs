main :: IO()
main = 
  let tree = (Node 8 (Node 6 (Node 4 (Node 2 Empty Empty) (Node 2 Empty Empty)) (Node 4 (Node 2 Empty Empty) (Node 2 Empty Empty))) (Node 6 (Node 4 (Node 2 Empty Empty) (Node 2 Empty Empty)) (Node 4 (Node 2 Empty Empty) (Node 2 Empty Empty))))
  in do
 print (numOfNodes [(10,[3,7,12]),(3,[5,8,9]),(7,[11,13]),(12,[6,4]),(8,[1,2])])
 print (find [2.3, 4.5, 7.0, 9.9, 6.5])
 print (closestToAverage [(Temp 1 23.6),(Temp 6 24.2),(Temp 11 24.2),(Temp 16 21.2),(Temp 21 23.8),(Temp 26 26.5),(Temp 31 24.5)])
 print (grandchildrenIncreased tree)
 print (grandchildrenIncreased (Node 17 (Node 14 (Node 18 (Node 16 Empty Empty) (Node 201 Empty Empty)) (Node 201 (Node 19 Empty Empty) (Node 201 Empty Empty))) (Node 20 (Node 171 (Node 21 Empty Empty) (Node 201 Empty Empty)) (Node 204 Empty Empty))))

{- Задача 1.Дадено е дърво tree от цели числа, представено с асоциативен списък, описващ преките наследници (синовете) 
на върховете, които не са листа. Да се дефинира функция (numOfNodes tree), която намира броя на вътрешните върхове node на tree, 
за които сумата на синовете на node е равна по стойност на родителя на node. -}
type Node = (Int, [Int])
n :: Node
n = (10,[3,7,12])

numOfNodes :: [Node] -> Int
numOfNodes [] = 0
numOfNodes tree = length [x | (x, n) <- tree, (sum n) == (helper x)]
 where helper x = if null [k | (k, p) <- tree, elem x p] then 0 else head [k | (k, p) <- tree, elem x p]
 
{- Задача 2.Температурно измерване се описва с типа
data Measuring = Temp Int Float, където стойността от тип Int задава ден от месеца, а стойността от тип Float – 
измерена температура за този ден. Да се дефинира функция closestToAverage :: [Measuring] -> Int, която по списък от 
температурни измервания намира деня, в който измерената температура е най-близо до средната температура през месеца.
data Measuring = Temp Int Float -}

find :: [Float] -> Float
find [] = 0
find temps = (sum temps) / (fromIntegral (length temps))

getDay :: Measuring -> Int
getDay (Temp day temperature) = day

closestToAverage :: [Measuring] -> Int
closestToAverage [] = 0
closestToAverage measuring = 
  getDay (foldl1 (\ currMeasuring@(Temp _ currTemp) bestMeasuring@(Temp _ bestTemp) ->
      if abs (currTemp - avgTemp) < abs (bestTemp - avgTemp) then currMeasuring else bestMeasuring) measuring)
          where avgTemp = find [t | (Temp d t) <- measuring]
 
{- Задача 3.Нека за представянето на двоично дърво от цели числа се използва алгебричен тип със следната дефиниция:
data BTree = Empty | Node Int BTree Btree .
Да се дефинира функция (grandchildrenIncreased tree), която проверява дали всеки връх на двоичното дърво tree е поне с единица 
по-голям от своя дядо (ако има такъв). -}
data BTree a = Empty | Node a (BTree a) (BTree a)

treeDepth :: BTree a -> Int
treeDepth Empty = 0
treeDepth (Node value (tr1) (tr2)) = 1 + max (treeDepth tr1) (treeDepth tr2)

getLeft :: BTree a -> BTree a
getLeft (Node value left right) = left

getRight :: BTree a -> BTree a
getRight (Node value left right) = right

getValue :: BTree a -> a
getValue (Node value left right) = value

treeNodesAtLevel :: (Eq b, Num b) => BTree a -> b -> [a]
treeNodesAtLevel Empty _ = []
treeNodesAtLevel (Node value _ _) 0 = [value]
treeNodesAtLevel (Node _ tr1 tr2) n = (treeNodesAtLevel tr1 (n-1)) ++  (treeNodesAtLevel tr2 (n-1))

grandchildrenIncreased :: (Num a, Ord a) => BTree a -> Bool
grandchildrenIncreased Empty = False
grandchildrenIncreased tree
  | (treeDepth tree == 1) || (treeDepth tree == 2) = True
  | otherwise = (grandchildrenIncreased (getLeft tree)) && (grandchildrenIncreased (getRight tree)) && (all (>= ((getValue tree) - 1)) (treeNodesAtLevel tree 2))
