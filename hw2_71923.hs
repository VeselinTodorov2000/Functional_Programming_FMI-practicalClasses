--Задача 1
generate::Double -> Integer -> [Double]
generate p n = [formElement p m | m<-[1, 2 .. n]] --we will create use list comprehension to create all elements and put them into list 

formElement :: Double -> Integer -> Double 
formElement p n = sum[1/((fromIntegral m)**p) | m<-[1 .. n]] --this will create every element from its parameters by sum of all parts

--Задача 2
listSquares :: Integer -> Integer -> [(Integer, Integer)]
listSquares a b = [(k, sumDels k) | k <- [a .. b], isSquare k 0] --list comprehension
                        where 
                            sumDels n = sum [d^2 | d<-[1 .. n], n `mod` d == 0] --sum of denominators on square 

isSquare :: Integer -> Integer -> Bool --predicat: is a number a square of other number
isSquare n k 
    |n < k^2    = False
    |n == k^2   = True
    |otherwise  = isSquare n (k+1)

--Задача 3 
type Point = (Double, Double)
splitPoints :: Point -> Double -> [Point] -> ([Point], [Point])
splitPoints center radius ps = (is,os) --tuple of is and os
            where 
                is = [i | i <- ps, isInside center radius i] --all points inside a circle
                os = [o | o <- ps, not(isInside center radius o)] -- all points outside a circle


isInside :: Point -> Double -> Point -> Bool
isInside center radius checkPoint = (radius ^ 2) >= (fst checkPoint - fst center)^2 + (snd checkPoint - snd center)^2 --function that checks is a point inside a circle

--Задача 4
type Account = (Int, Int, Double)
type Person = (Int, String, String) 

ps :: [Person]
ps = [(1, "Ivan", "Sofia"), (2, "Georgi", "Burgas"),
 (3, "Petar", "Plovdiv"), (4, "Petya", "Burgas")]
 
as :: [Account]
as = [(1, 1, 12.5), (2, 1, 123.2), (3, 2, 13.0), (4, 2, 50.2),
 (5, 2, 17.2), (6, 3, 18.3), (7, 4, 19.4)]

getAverageBalance :: ([Account], [Person]) -> (Person -> Bool) -> Double
getAverageBalance ((idAc, idPerson, balance) : as, (idPerson', name, city) : ps) p = sum [balance| (_, idPerson,balance) <- as, elem idPerson (getIds getNeededPeople)] / fromIntegral (currentBalance)
    where
        getNeededPeople = filter p ps -- this will filter people by predicat p
        getIds idsOfPeople= [id | (id, _, _) <- idsOfPeople] -- list of id's
        currentBalance = length [balance | (_, idPerson, balance) <- as, elem idPerson (getIds getNeededPeople)]

--main
main :: IO()
main = do
    print (listSquares 1 30)
    print (listSquares 250 300)

    print() 
    print(generate 1 3)
    print(generate 0.1 5)
    
    print()
    print(splitPoints (1,1) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)])
    print(splitPoints (10,10) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)])
    print()    
    print(getAverageBalance (as,ps) (\ (_,_,city) -> city == "Burgas")) -- 24.95
    print(getAverageBalance (as,ps) (\ (_,(n:_),_) -> n == 'P'))
