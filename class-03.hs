{-
Явная рекурсия в решениях хотя и допускается, но не приветствуется. Старайтесь обходиться стандартными
функциями, используя при этом создание функций «на лету». Пытайтесь максимально упростить уже написанные
решения, применяя подходящие функции из модуля Data.List и любых других модулей. Перед выполнением заданий
изучите примеры из лекции по функциям высшего порядка. 
-}

{-
 1. Простейшие задачи на применение функций map и filter.
 1.1 Преобразовать данный список целых чисел следующим образом:
  a) увеличить все его элементы в два раза;
-}
twiceList [] = []
twiceList (x) = map (*2) x

--  b) увеличить все его элементы с четными значениями в два раза;
twiceEven [] = []
twiceEven (x) = map (\x -> if odd  x then x*1 else x*2) x

--  с) обнулить все его элементы с нечетными значениями;

oddToZero [] = []
oddToZero (x) = map (\x -> if odd x then x*0 else x*1) x

--  d) удалить из него элементы, большие заданного числа k;
deleteMoreK k [] = []
deleteMoreK k (x) = filter (<=k) x 

--e) отфильтровать его, оставив в списке только отрицательные числа;

deletePositive [] = []
deletePositive (x) = filter (<0) x 

 -- f) удалить из него все положительные чётные числа.
deletePositiveEven [] = []
deletePositiveEven x = filter (\x -> if x>0 && even x then False else True) x

f11a :: Integral a => [a] -> [a]
f11a = map undefined

{-
 1.2 Дан список декартовых координат точек на плоскости (пар вещественных чисел).
     Преобразовать его следующим образом:
  a) отфильтровать список так, чтобы в нём остались точки из заданной координатной четверти;
  -}
  
deleteWrongCoord [] d = []
deleteWrongCoord x d
	| d == 1 = filter (\(x,y) -> if x > 0 && y > 0 then True else False) x
	| d == 2 = filter (\(x,y) -> if x > 0 && y > 0 then True else False) x
	| d == 3 = filter (\(x,y) -> if x > 0 && y > 0 then True else False) x
	| d == 4 = filter (\(x,y) -> if x > 0 && y > 0 then True else False) x
	|otherwise = undefined


	-- b) преобразовать декартовы координаты в полярные.


{-
 1.3 Дан список слов.
  a) Преобразовать все слова к верхнему регистру.
 -}
  toUpperCase [] = []
  toUpperCase x = map (\x -> map (\x -> toUpper x) x) x


--b) Извлечь из него подсписок слов заданной длины.

wordsWithLength [] l = []
wordsWithLength x l = filter (\x -> if length x == l then True else False) x
--c) Извлечь из него подсписок слов, начинающихся с заданной буквы.


f13a :: [String] -> [String]
f13a = map undefined

{-
2. Формирование числовых последовательностей (iterate).
 a) Список натуральных чисел, начиная с 0.
 -}

 natList = iterate (1+) 0
 
--b) Список чётных чисел.

-- Если без 0, то natList меняем на [1..]
--evenList = filter (\x -> if even x then True else False) natList
evenList = iterate (2+) 0

--c) Список элементов последовательности: a_0=1, a_n=(1+a_{n-1})/2.
 
seqList = iterate gen 1
	where 
		gen x = (1+x)/2

-- d) Список символов английского алфавита.


--engSimbols = ['a'..'z']
engSimbols = take 26 (map chr (filter (\x -> if x > 96 && x < 123 then True else False) (iterate (1+) 0)))
		
-- e) Список строк, представляющих n-значные двоичные числа.


nats :: [Integer]
nats = iterate undefined 0

{-
3. Группировка списков.
  a) Дан список символов. Сгруппировать подряд идущие символы по принципу: цифры — не цифры — ...
  
  
  -- gr x = groupBy (\x y -> (isDigit x) && (isLetter y) || ((isLetter x) && (isDigit y))) x
  
  b) Дан список пар вещественных чисел (координат точек на плоскости). Сгруппировать подряд идущие
     координаты точек, лежащие в одной координатной четверти.
	 
-}	 
	 grP x = groupBy (\(w,x) (y,z) -> (findCoordF w x) == (findCoordF y z)) x
	 
--  c) Дан список и ненулевое натуральное число n. Разбить список на подсписки длиной n каждый.
--     Последний подсписок может содержать менее n элементов.

subList [] d = []
subList x d = [take d x] ++ subList (drop d x) d

--  d) Дан список и ненулевые натуральные числа n и m. Разбить список на перекрывающиеся подсписки
--     длиной n элементов со сдвигом относительно предыдущего подсписка на m элементов.

subList1 [] n m = []
subList1 x n m = [take n x] ++ subList1 (drop m x) n m

--  e) Дан список. Определить длину самого длинного подсписка, содержащего подряд идущие одинаковые элементы.


longestSubList [] = 0
-- maximum от пустого не робит
longestSubList x = maximum (map length (group x))

f3d :: [a] -> Int -> Int -> [[a]]
f3d xs n m = undefined

-- Должно быть True
test_f3d = f3d [1..10] 4 2 == [[1,2,3,4],[3,4,5,6],[5,6,7,8],[7,8,9,10],[9,10]]

{-
4. Разные задачи.
 a) Дан текст в виде строки символов, содержащий среди прочего числовые данные. Посчитать количество
    всех упоминающихся в тексте чисел.
	-}
	
	countDigits x = length (filter (\x -> if isDigit x then True else False) x)
{- b) Найти сумму всех чисел Фибоначчи, удовлетворяющих заданному предикату, в указанном промежутке
    (например: все чётные от 1 до 106).-}
	
	-- Все авторские права на ф-ю fibs принадлежат Брагилевскому Виталию Николаевичу
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibs' f a b = (filter f (filter (\x -> x >= a) (takeWhile (\x -> x <= b) fibs)))
{-
 c) Дана строка текста и число n. Сформировать список, содержащий n самых часто используемых
    в строке символов.
	-}
	
	-- nSimbols x n = sortBy (\x -> (compare snd x)) (map (\(w:ws) -> (w, length ws + 1)) (group(sort x)))
	-- sortBy (max) map length (group(sort x))
	
-- map (\(w:ws) -> (w, length ws + 1)) . group . sort
	-- ((group (sort x)))
	
 {-  d) Дан список чисел. Сформировать список локальных максимумов исходного списка. Локальным максимумом
    называется элемент, больший своих соседей.
 e) Дан список. Продублировать все его элементы.
-}
doubleList x = concat (map (\x -> [x]++[x]) x)
