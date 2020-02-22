--Задача 1.​ Да се дефинира функция ​inside a b x​, която проверява дали числото x принадлежи на интервала [a, b] 
inside::Integer -> Integer -> Integer -> Bool
inside a b x = a <= x && x <= b

--Задача 2.​ Да се дефинира функция ​sumSquares a b​, която намира сумата на квадратите на числата a и b
sumSquares::Integer -> Integer -> Integer
sumSquares a b = (a^2)+(b^2)

--Задача 3.​ Да се дефинира функция ​average a b​, която намира средно аритметичното на a и b 
average::Integer -> Integer -> Double
average a b = fromIntegral(a + b) / 2
--ако ще правим делене с дроби ни трябва fromIntegral

-- Задача 4. Да се дефинира функция squaresAverage a b, която намира средно аритметичното на квадратите на a и b
squaresAverage::Integer -> Integer -> Double
squaresAverage a b = fromIntegral(sumSquares a b) / 2

-- Задача 5. Да се дефинира функция myMin x y, която връща минималния от двата си аргумента
myMin::Integer -> Integer -> Integer
myMin a b = if(a>b) then b else a

-- Задача 6. Да се дефинира функция myFact n, която пресмята факториела на числото n чрез рекурсивен процес
myFact::Integer -> Integer
myFact n = if(n <= 1) then 1 else n*(myFact(n-1))

-- Задача 7. Да се дефинира функция myFactIter n, която пресмята факториела на числото n чрез итеративен процес
myFactIter::Integer -> Integer
myFactIter n = helper n 1
helper k cur = 
    if (k == 1) 
    then cur
    else helper (k-1) (cur*k) 

-- Задача 8.​ Да се дефинира функция ​myFib n​, която връща n-тото число от редицата на Фибоначи (редицата е 1, 1, 2, 3, 5, ... и е индексирана от 0)
myFib::Integer -> Integer
myFib n = if(n <= 2) then 1 else (myFib(n-1) + myFib(n-2)) 

-- Задача 9.​ Да се дефинира функция ​myFibIter n​, която връща n-тото число от редицата на Фибоначи чрез ​итеративен ​ процес 
myFibIter::Integer -> Integer
myFibIter n = helperIter n 1 1
helperIter n prev cur = 
    if(n<=2)
    then cur
    else helperIter (n-1) cur (prev + cur)

-- Задача 10.​ Да се дефинира функция ​myGcd a b​, която връща НОД(a, b) 
myGcd::Integer -> Integer -> Integer
myGcd a b = if (a == b) 
    then a 
    else if(a>b) 
        then myGcd (a-b) b
        else myGcd a (b-a)

-- Сума на числата от 1 до n
sumNums::Integer -> Integer
sumNums n = if(n == 1) then 1 else n+(sumNums(n-1))
main::IO()
main = do
    print(sumNums 100)