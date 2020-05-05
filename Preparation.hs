--task 1
isInteresting:: Int -> Bool
isInteresting n = n `mod` (sumDigits n 0) == 0
                    where
                        sumDigits::Int->Int->Int 
                        sumDigits k sum = if(k == 0) then sum else sumDigits (k `div` 10) (sum+(k `mod` 10))

--task 2
sumBetweenIntervals:: Int -> Int -> Int
sumBetweenIntervals a b = sum[k | k<-[a..b], k `mod` 4 == 1, consistSix1 k]
                                
consistSix::Int -> Bool
consistSix n = if(n == 0)
                    then False
                    else 
                        if(n `mod` 10 == 6)
                            then True
                            else consistSix (n `div` 10)

consistSix1::Int -> Bool
consistSix1 n 
    |n == 0          = False
    |n `mod` 6 == 0  = True 
    |otherwise       = consistSix1 (n `div` 10)

--task 4
sin1:: Int -> Double -> Double
sin1 n x = sum[((-1)^i)*(x**fromIntegral(2*i+1))/fromIntegral(fact(2*i+1))| i <- [0 .. n]]

fact::Int -> Int
fact n = if(n == 0) then 1 else n * fact (n-1)

--task 5
dominates::(Int -> Int) -> (Int -> Int) -> [Int]-> Bool
dominates f g []   = True
dominates f g [x]  = (f x) >= (g x)
dominates f g (x:xs) = if((f x) >= (g x)) then dominates f g xs else False

main :: IO()
main = do
    print (isInteresting 410) --True
    print (isInteresting 35) --False
    print (isInteresting 12) -- True
    print (sumBetweenIntervals 1 100) 
    print (sin1 10 1.57)
    print (dominates (\x -> x+1) (\x -> x+1) [1 .. 10])