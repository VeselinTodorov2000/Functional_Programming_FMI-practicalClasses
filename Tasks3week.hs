--Задача 1. Да се дефинира функция pow, която генерира линейно рекурсивен процес и намира x на степен n, където x е реално, а n - естествено число.
pow::Double -> Integer -> Double 
pow x n = if(n == 0)
                then 1
                else (x*(pow x (n-1)))

--Задача 2. Да се дефинира предикат isPrime, който проверява дали дадено естествено число е просто.
--(Заб.: Числото 1 не е нито просто, нито съставно.)
isPrime::Integer -> Bool
isPrime n = (helper n 1 0) == 2

helper::Integer -> Integer -> Integer -> Integer
helper n denominator sumDels = if(denominator == n) 
                                    then (sumDels+1) 
                                    else if(n `mod` denominator == 0) 
                                                then (helper n (denominator+1) (sumDels+1))
                                                else (helper n (denominator+1) sumDels)   

--Задача 3. Да се дефинира предикат isAscending, който връща истина, ако цифрите на дадено естествено число са в нарастващ ред от първата към последната.
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

--Задача 4. Да се дефинира функция countOccurences, намираща броя на срещанията на дадена цифра d в записа на число n.
countOccurences::Integer -> Integer -> Integer
countOccurences n digit = if(n==0) then 0
                        else 
                            if(digit == (mod n 10)) then (1+(countOccurences (div n 10) digit)) else (0+(countOccurences (div n 10) digit))

--Задача 5. Да се дефинира предикат isPerfectNumber, който връща дали едно число е съвършено, т.е. равно на сумата от делителите му.
isPerfect::Integer -> Bool
isPerfect n = (n == sumDels n)

sumDels::Integer -> Integer
sumDels n = helperPerfect n 1 0

helperPerfect::Integer -> Integer -> Integer -> Integer
helperPerfect n denominator sum = if(denominator==n) 
                                        then sum 
                                        else 
                                            if((n `mod` denominator) == 0)
                                                then (helperPerfect n (denominator+1) (sum+denominator))
                                                else (helperPerfect n (denominator+1) sum)                                  

--Задача 6. Да се дефинира функция sumPrimeDivisors, която намира сумата на всички прости делители на едно число.
sumPrimeDivisors::Integer -> Integer
sumPrimeDivisors n = helperSum n 1

helperSum::Integer -> Integer -> Integer 
helperSum n denom = if(n == denom) 
                        then 0 
                        else 
                            if((isPrime denom) && (n `mod` denom == 0)) 
                                then (denom + (helperSum n (denom+1))
                                else (0+(helperSum n (denom+1))) 
main::IO()
main = do
    print(sumPrimeDivisors 2)