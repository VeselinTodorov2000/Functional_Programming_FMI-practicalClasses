--Task 1
findSum::Int -> Int -> Int -> Int
--we sum the last three members of the set
findSum a b n = (helperSum a b (n-1)) + (helperSum a b (n-2)) + (helperSum a b (n-3)) 

--function that calculates the n member of the set
helperSum::Int -> Int -> Int -> Int
helperSum a b n = if(n < 0) 
                            then a
                            else (((2^n)*b) + (helperSum a b (n-1)))
--Task 2
isSquare::Int -> Bool
isSquare n = helperInSquares n 0
--we create a help function in order to check if there is a number, which square is 'n'
helperInSquares::Int -> Int -> Bool
helperInSquares n squareTester = if(n < squareTester*squareTester) --if n becomes smaller than the test case there is no point to check other nums
                                     then False;
                                     else 
                                       if(n == squareTester*squareTester) --if the current test is correct then we return true
                                           then True 
                                           else (helperInSquares n (squareTester+1)) --else we check the next num till we find correct test or recursion stops

--Task 3
isSpecial::Integer -> Int -> Bool
isSpecial n k = if(n < 10^k) --recursion base case
                  then True
                  else 
                    if(isPrime (n `mod` (10^k)))  --check every k-element number is prime
                      then (isSpecial (n `div` 10) k) -- if it is true we continue till we get to the base case
                      else False --if we find number that's not prime, we end the recursion

--function that check if number is prime
isPrime::Integer -> Bool
isPrime n = (countDels n 1 0) == 2

countDels::Integer -> Integer -> Integer -> Integer
countDels number currentDenominator counterDenominators = if(number == currentDenominator) 
                                                                then (counterDenominators+1)
                                                                else 
                                                                    if((number `mod` currentDenominator) == 0)
                                                                        then (countDels number (currentDenominator+1) (counterDenominators+1))
                                                                        else (countDels number (currentDenominator+1) (counterDenominators))
  
main::IO()
main = do
    print(isSpecial 54787 3)
    