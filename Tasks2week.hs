--Задача 1. Да се дефинира функция myGCD a b, която връща най-големия общ делител на числата a и b.
myGCD::Integer->Integer->Integer
myGCD a b = if(a == b) then a
            else if(a>b)
                then myGCD (a-b) b
                else myGCD a (b-a)

--Задача 2. Да се дефинира функция countDigits, която генерира линейно рекурсивен процес и намира броя на цифрите на дадено естествено число.
countDigits::Integer -> Integer
countDigits n = if(n == 0) then 0
                else (1 + countDigits (div n 10))

--Задача 3. Да се дефинира функция sumDigitsRec, която генерира линейно рекурсивен процес и намира сумата от цифрите на дадено естествено число.
sumDigitsRec::Integer -> Integer
sumDigitsRec n = if(n==0) then 0
                else ((mod n 10) + sumDigitsRec (div n 10)) 

--Задача 4. Да се дефинира функция sumDigitsIter, която генерира линейно итеративен процес и намира сумата от цифрите на дадено естествено число.
sumDigitsIter::Integer -> Integer
sumDigitsIter n = helper n 0

helper::Integer -> Integer -> Integer
helper n sum = if(n == 0) then sum
                          else (helper (div n 10) (sum+(mod n 10)))

--Задача 5. Да се дефинира функция reverseNumber, която генерира линейно итеративен процес и по дадено естествено число n намира числото, записано със същите цифри, но в обратен ред.
reverseNumber::Integer -> Integer
reverseNumber n = reverser n 0

reverser::Integer -> Integer -> Integer
reverser n sum = if(n == 0) then sum
                            else reverser (div n 10) ((sum*10)+(mod n 10))

main::IO()
main = do
    print(reverseNumber 1024)