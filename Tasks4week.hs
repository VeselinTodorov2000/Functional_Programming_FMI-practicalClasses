--Задача 1. Да се дефинира функция, която намира броя на елементите на списък.
lengthList::[a] -> Int
lengthList list = if(length list == 0) then 0 else (1+(lengthList (tail list)))

--Задача 2. Да се дефинира функция, която намира сумата на елементите в списък.
sumElements::[Int] -> Int
sumElements xs =if(length xs == 0) then 0 else head xs + (sumElements (tail xs))

--Задача 3. Да се дефинира функция, която намира дали даден елемент се съдържа в списък.
hasElement::[Int] -> Int -> Bool
hasElement xs x = if(length xs == 0)
                        then False
                        else if(head xs == x)
                                then True
                                else (hasElement (tail xs) x)

--Задача 4. Да се дефинира функция, която генерира списък с простите числа в интервала [a,b].
listPrimeNumbers::Integer -> Integer -> [Integer]
listPrimeNumbers a b = helper a b []

helper::Integer -> Integer -> [Integer] -> [Integer]
helper a b xs = if(a > b)
                    then xs
                    else if(isPrime a)
                            then (helper (a+1) b (xs++[a]))
                            else (helper (a+1) b xs)        

isPrime::Integer -> Bool
isPrime n = (countDels n 1 0) == 2

countDels::Integer -> Integer -> Integer -> Integer
countDels number currentDenominator counterDenominators = if(number == currentDenominator) 
                                                                then (counterDenominators+1)
                                                                else 
                                                                    if((number `mod` currentDenominator) == 0)
                                                                        then (countDels number (currentDenominator+1) (counterDenominators+1))
                                                                        else (countDels number (currentDenominator+1) (counterDenominators)) 

--Задача 5. Да се дефинира функция, която премахва първият елемент равен на x от даден списък.
filterByFirstX::[Int] -> Int -> [Int]
filterByFirstX xs x = helperF xs x 0

helperF::[Int] -> Int -> Int -> [Int]
helperF xs x currentElement = if((xs!!currentElement == x) && (currentElement<(length xs)))
                                then ((take currentElement xs) ++ (tail (drop currentElement xs) ))
                                else (helperF xs x (currentElement+1))

--Задача 6. Да се дефинира функция, която премахва всички елементи, които са равни на x от даден списък.
filterByEveryX::[Int] -> Int -> [Int]
filterByEveryX xs x = helperE xs x 0 []

helperE::[Int] -> Int -> Int -> [Int] -> [Int]
helperE xs x currentElement newList = if(currentElement == length xs)
                                          then newList
                                          else if((xs!!currentElement) == x)
                                                  then (helperE xs x (currentElement+1) (newList)) 
                                                  else (helperE xs x (currentElement+1) (newList ++ [xs!!currentElement]))

main::IO()
main = do
    
    print(filterByEveryX [1,3,2,1,3,5,1] 1)