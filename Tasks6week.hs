--map
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = [f x | x <- xs]

--filter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = [x | x<-xs, f x]

--Задача 1. Да се дефинира функция multiplyAllBy :: [Int] -> Int -> [Int], 
--която получава списък и число и умножава всеки елемент на списъка по числото.
multiplyAllBy :: [Int] -> Int -> [Int]
multiplyAllBy xs n = map (\x -> x*n) xs

--Задача 2. Да се дефинира filterSmallerThan xs n, 
--която получава списък xs и число n и премахва елементите на списъка xs, които са по-малко от числото n.
filterSmallerThan :: [Int] -> Int -> [Int]
filterSmallerThan xs n = myFilter (\x -> x >= n) xs

--Задача 3. Да се дефинира функция isAscending :: Integer -> Bool, която проверява дали цифрите на число
--са във възходящ ред. Функцията да получава число, но да работи със списък от цифрите му.
isAscending :: Integer -> Bool
isAscending n = toList n == qSort (toList n)
                where
                  qSort::[Integer] -> [Integer]
                  qSort [] = []
                  qSort (x:xs) = qSort smaller ++ [x] ++ qSort bigger
                            where
                                smaller = [s | s <- xs, s <= x]
                                bigger = [b | b <- xs, b > x]

toList :: Integer -> [Integer]
toList n = helper n [] 
    
helper :: Integer -> [Integer] -> [Integer]
helper n xs
    |n == 0      = xs
    |otherwise  = helper (n`div`10) ((n `mod` 10):xs)

--Задача 4. Нека as = [a1, a2, … , ak] и bs = [b1, b2, … , bk] са непразни списъци с еднакъв брой числа. Да се дефинира предикат isImage :: [Int] -> [Int] -> Bool,
-- който да връща „истина“ точно когато съществува такова число x, че ai = x + bi за всяко i = 1,..., k.
 

main :: IO()
main = do
    print(multiplyAllBy [1 .. 3] 3)
    print(filterSmallerThan [1, 1, 2, 2, 3, 3, 5 ] 2)
    print(isAscending 123456789) 