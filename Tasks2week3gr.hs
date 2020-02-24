--Задача 1.​ Да се дефинира функция ​countDigits​, която намира броя на цифрите на дадено естествено число. Да се напише и итеративно решение. 
countDigitsRec::Integer -> Integer
countDigitsRec n = if(n==0) then 0 else (  1+countDigitsRec(div n 10))

countDigitsIter::Integer -> Integer
countDigitsIter n = helperCounter n 0

helperCounter::Integer -> Integer -> Integer
helperCounter n cnt = if(n==0) then cnt else (helperCounter (div n 10) (cnt+1))

--Задача 2.​ Да се дефинира функция ​sumDigits​, която намира сумата на цифрите на дадено естествено число. Да се напише и итеративно решение. 
sumDigits::Integer -> Integer
sumDigits n = if(n == 0) then 0 else ((mod n 10) + sumDigits (div n 10))

sumDigitsIter::Integer -> Integer
sumDigitsIter n = helperSumator n 0

helperSumator::Integer -> Integer -> Integer
helperSumator n sum = if(n==0) then sum else (helperSumator (div n 10) (sum+(mod n 10)))

--Задача 3. ​Да се дефинира функция ​countOccurences n digit​, която връща броят на срещанията на цифрата ​digit ​в записа на числото ​n
countOccurences::Integer -> Integer -> Integer
countOccurences n digit = if(n==0) then 0
                        else 
                            if(digit == (mod n 10)) then (1+(countOccurences (div n 10) digit)) else (0+(countOccurences (div n 10) digit))

--Задача 4.​ Да се дефинира предикат ​isAscending​, който проверява дали цифрите на дадено число са записани във възходящ ред. 
isAscending::Integer -> Bool
isAscending n = if(n < 10) then True 
                           else helperAscender (div n 10) (mod n 10)
helperAscender::Integer -> Integer -> Bool
helperAscender number lastDigit = if(number < 10) 
                                        then number<lastDigit 
                                        else 
                                            if((mod (div number 10) 10) < (mod number 10)) 
                                                then (helperAscender (div number 10) (mod number 10))
                                                else False    

-- Задача 5. Да се дефинира предикат isPrime, който проверява дали дадено естествено число е просто
isPrime::Integer -> Bool
isPrime 1 = False
isPrime n = helperPrime 2
            where
                helperPrime currentNumber
                    mod(number currentNumber) == 0      = False
                    mod(number currentNumber) == 1      = True
                    number == currentNumber             = True

-- Задача 6. Да се дефинира функция isPerfect, която проверява дали дадено число е равно на сумата от делителите си
isPerfect::Integer -> Bool
isPerfect n = n == sumDels n

sumDels::Integer -> Integer
main::IO()
main = do
    print(isAscending 1324)