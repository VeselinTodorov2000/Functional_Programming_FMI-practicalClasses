--task 1
data Shape = Circle Double | Rectangle Double Double
             | Triangle Double Double Double | Cylinder Double Double deriving Show


show1:: Shape -> String
show1 (Circle r) = "Circle"
show1 (Rectangle a b) = "Rectangle"
show1 (Triangle a b c) = "Triangle"
show1 (Cylinder r h) = "Cylinder"

--task 2
perimeter:: Shape -> Double
perimeter (Circle r) = 3.14 * r * 2
perimeter (Rectangle a b) = 2*(a+b)
perimeter (Triangle a b c) = a+b+c
perimeter (Cylinder r h) = 2*(r*r*3.14) + h*(r*r*3.14)

area:: Shape -> Double
area (Circle r) = 3.14*r*r
area (Rectangle a b) = a*b
area (Triangle a b c) = sqrt(p*(p-a)*(p-b)*(p-c))
                            where p = (a+b+c)/2
area (Cylinder r h) = 2*3.14*r*(r+h)

isRound:: Shape -> Bool
isRound (Circle r) = True
isRound (Rectangle a b) = False
isRound (Triangle a b c) = False
isRound (Cylinder r h) = True

is2D:: Shape -> Bool
is2D (Cylinder r h) = False
is2D (Circle r) = True
is2D (Rectangle a b) = True
is2D (Triangle a b c) = True

--task 3
m::[Shape]
m = [(Rectangle 3 4), (Rectangle 4 5), (Rectangle 2 3)]
sumArea:: [Shape] -> Double
sumArea ss = helper ss 0

helper::[Shape] -> Double -> Double
helper [] sum = sum
helper (x:xs) sum = helper xs (sum+ (area x))

biggestShape::[Shape] -> Double
biggestShape ss = maximum[area(k) | k<-ss]

maximum :: [Double] -> Double
maximum [x]    = x
maximum (x:xs) =
    |(maximum xs) > x = maximum xs
    |otherwise        = x

main :: IO()
main = do
    print(biggestShape m)