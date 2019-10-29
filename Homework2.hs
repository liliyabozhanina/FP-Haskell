main :: IO()
main = do
 print ((pairCompose [(\x -> x+1),(\x -> x+2),(\x -> x+3)]) 1)    -- 8
 print ((switchsum (\x -> x + 1) (\x -> x * 2) 1) 2)              -- 3
 print ((switchsum (\x -> x + 1) (\x -> x * 2) 2) 2)              -- 9
 print ((switchsum (\x -> x + 1) (\x -> x * 2) 3) 2)              -- 16
 print ((switchsum (\x -> x + 1) (\x -> x * 2) 4) 2)              -- 30
 print (replaceAssoc [5,4,2,3] [(1,5),(3,7),(5,9),(7,11),(9,13)]) -- 9,4,2,7

{- Задача 1.Да се дефинира функция (pairCompose fs), която получава като аргумент списък [f1,f2,f3, ... ,fn] с 
функции от тип Int -> Int и връща нова едноаргументна числова функция g – такава, че оценката на (g x) е равна на 
сумата (f1.f2) (x) + (f3.f4) (x) + ... + (fn-1.fn) (x), където “.” е операторът за композиция на функции. Ако оригиналният 
списък с функции има нечетен брой елементи, то последната функция от списъка се композира с функцията идентитет (id).
Пример:
(pairCompose [(\x -> x+1),(\x -> x+2),(\x -> x+3)]) 1 →
((1+2)+1)+(1+3) = 8 -}

pairCompose :: [Int -> Int] -> (Int -> Int)
pairCompose [] x = 0
pairCompose (f:g:fs) x = helper + pairCompose fs x
 where 
  helper = (f . g) x
pairCompose (f:fs) x = (f . (\x -> x)) x

{- Задача 2.Ако f и g са числови функции и n е естествено число, да се дефинира функция от по-висок ред switchsum f g n, 
която връща като резултат функция, чиято стойност в дадена точка x е равна на f(x)+g(f(x))+f(g(f(x)))+... (сумата включва n събираеми). -}
func :: (Int -> Int) -> (Int -> Int) -> Int -> (Int -> Int)
func f g 0 x = 0
func f g 1 x = f x
func f g n x = if even n then (g ((func f g (n-1)) x)) else (f ((func f g (n-1)) x))

switchsum :: (Int -> Int) -> (Int -> Int) -> Int -> (Int -> Int)
switchsum f g 0 x = 0
switchsum f g n x = (func f g n x) + (switchsum f g (n-1) x)

{- Задача 3.Да се дефинира функция (replaceAssoc list dict), която получава като аргументи списък list, чийто елементи са цели числа, 
и речник – асоциативен списък dict, чийто елементи са двойки от цели числа. Функцията трябва да върне нов списък, в който всеки елемент 
се получава чрез замяна на съответния елемент на list с асоциираната с него стойност в dict, ако в dict съществува елемент с такъв ключ, 
или е равен на съответния елемент на list – в противен случай. -}
type Pair = (Int, Int)
p :: Pair
p = (1, 5)

replaceAssoc :: [Int] -> [Pair] -> [Int]
replaceAssoc [] _ = []
replaceAssoc list [] = list
replaceAssoc (x:list) dict = help : (replaceAssoc list dict)
 where 
  help = if null [n | (k, n) <- dict, k == x] then x else (head [n | (k, n) <- dict, k == x])

