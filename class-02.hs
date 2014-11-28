-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms = undefined

sec2hms a = (secToH a, secToM (a - (secToH(a)*60*60)), a - (secToH(a)*60*60 + secToM (a - (secToH(a)*60*60))*60))
secToM a = div a 60
secToH a = div (secToM a) 60

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h * 3600 + m * 60 + s 

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' a b c = hms2sec (a, b, c)

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = undefined

length' x1 y1 x2 y2 = sqrt ((y2 - y1)^2 + (x2 - x1)^2)

-- triangle :: ??? -> (Double, Double)

triangle (a, b, c) = (p, sForTr)
	where 
		p = a + b + c		
		sForTr = sqrt ((p * (p - a) * (p - b) * (p - c)) / 2)

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs)  
	| mod x 2 == 0 = 1 + nEven xs
	| otherwise = 0 + nEven xs



-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems = (x:xs) = 2 * x : doubleList xs



-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs)
	| mod x 2 == 0 = fltOdd xs
	| otherwise = x : fltOdd xs 
	
-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;

delNeg [] = []
delNeg (x:xs) 
	| x < 0 = delNeg xs
	| otherwise = x : delNeg xs
-- б) увеличить элементы с чётными значениями в два раза;
doubleEven [] = []
doubleEven (x:xs)
	| mod x 2 == 0 = 2 * x : doubleEven xs
	| otherwise = x : doubleEven xs
-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).

swap2Elem [] = []
swap2Elem (x:y:xs) = y : x : swap2Elem xs
swap2Elem x = []


-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus (x:xs) (y:ys) = undefinedcombine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = (x+y) : combine_plus xs ys


-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
genListRev 0 = []
genListRev n = n : genListRev (n-1)
-- б) в порядке возрастания.
genList 0 = []
--genList n = genList (n-1) ++ [n]

--Кто-то не может в конкатенацию

genList n = iter 1
	where
		iter i
			| i <= n = i : iter (i+1)
			| otherwise = []
-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.

incertElemEv [] a = []
incertElemEv (x:y:xs) a = x:a:y:a:incertElemEv xs a 
incertElemEv (x:xs) a = x:xs

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).
eqStartElems :: Eq a => [a] -> ([a], [a])
eqStartElems [] = ([],[])
eqStartElems (x:y:xs) = (eq (x:y:xs),others (x:y:xs))
	where
		eq :: Eq a => [a] -> [a]
		eq [] = []
		eq (x:y:xs)
			| x == y = x : eq (y:xs)
			|otherwise = [x]
		eq (x:xs) = [x]
		others :: Eq a => [a] -> [a]
		others [] = []
		others (x:y:xs)
			| x == y = others (y:xs)
			|otherwise = y:xs
		others (x:xs) = []

--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a
-- б) Eq a => [a] -> a -> Bool
-- в) [a] -> Int -> [a]
-- г) a -> Int -> [a]
-- д) [a] -> [a] -> [a]
-- е) Eq a => [a] -> [[a]]
-- ж) [a] -> [(Int, a)]
-- з) Eq a => [a] -> [a]
