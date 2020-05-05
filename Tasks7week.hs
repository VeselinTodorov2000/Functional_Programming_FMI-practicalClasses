-- map
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = [f x | x <-xs]

-- filter
myFilter :: Eq a => (a -> Bool) -> [a] -> [a]
myFilter f xs = [x | x <- xs, f x]

--foldr
myFoldr :: (a -> a -> a) -> [a] -> a
--myFoldr f [] = []
myFoldr f [x] = x
myFoldr f (x:xs) = f x (myFoldr f xs)

--Задача 1. Да се дефинира функция primesInRange :: Integer -> Integer -> [Integer], 
--която конструира списък от простите числа в интервала [a,b].
primesInRange :: Integer -> Integer -> [Integer]
primesInRange a b = myFilter isPrime [a .. b]
                    where 
                        isPrime::Integer -> Bool
                        isPrime n = [1, n] == [k | k <- [1 .. n] , mod n k == 0]

--Задача 2. Да се дефинира функция prodSumDiv :: [Integer] -> Integer -> Integer,
--която намира произведението на естествените числа в даден списък, сумата от делителите на които е кратна на k.
prodSumDiv :: [Integer] -> Integer -> Integer
prodSumDiv xs k = foldr (*) 1 (filter sumDenominators xs)
                  where
                      sumDenominators::Integer -> Bool
                      sumDenominators n = sum[ k | k <- [1 .. n], mod k n == 0] `mod` k == 0

--Задача 3. Да се дефинира функция isSorted :: [Int] -> Bool, която проверява дали списък е сортиран във възходящ ред.
isSorted :: [Int] -> Bool
isSorted xs = xs == (qSort xs)
              where
                  qSort::[Int] -> [Int]
                  qSort []      = []
                  qSort (x:xs)  = qSort smaller ++ [x] ++ qSort bigger
                            where
                                smaller = [s | s <- xs, s <= x]
                                bigger = [b | b <- xs, b > x]

--Задача 4. Да се дефинира функция insert :: Int -> [Int] -> [Int],
--която добавя елемент в сортиран списък, като резултатният списък също е сортиран.
insert :: Int -> [Int] -> [Int]
insert k []     = [k] 
insert k (x:xs) = if(k < x) 
                        then k:insert x xs 
                        else x:insert k xs

--Задача 5. Да се дефинира функция merge :: [Int] -> [Int] -> [Int], която получава два сортирани списъка
--и ги обединява така, че резултатът също да е сортиран.
merge :: [Int] -> [Int] -> [Int]
merge xs ys = qSort(xs ++ ys)
                where
                  qSort::[Int] -> [Int]
                  qSort []      = []
                  qSort (x:xs)  = qSort smaller ++ [x] ++ qSort bigger
                            where
                                smaller = [s | s <- xs, s <= x]
                                bigger = [b | b <- xs, b > x]

--Задача 6. Да се реализира функция insertionSort :: [Int] -> [Int], 
--която реализира сортиране чрез вмъкване върху списък.
insertionSort :: [Int] -> [Int]
insertionSort []      = []
insertionSort (x:xs)  = insertion x (insertionSort xs)
                        where
                            insertion :: Int -> [Int] -> [Int]
                            insertion x []     = [x]
                            insertion x (y:ys) 
                                | x <= y     = x:y:ys
                                | otherwise  = y:insertion x ys

main :: IO()
main = do
    print(primesInRange 1 20)
    print(primesInRange 1 100)
    print(primesInRange 15 30)
    print(' ')
    print(prodSumDiv [1 .. 10] 6)
    print(isSorted [1, 5, 7, 13, 25])
    print(insert 10 [1 .. 15])
    print(merge [1 .. 10] [1 .. 5])
    print(insertionSort [13, 6, 132, 86, 4, 33, 25, 92])
