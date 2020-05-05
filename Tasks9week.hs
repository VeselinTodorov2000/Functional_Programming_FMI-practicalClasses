import Data.Char
import Data.List

encode :: [Char] -> Char -> Int -> Char
encode alphabet ch offset =
    if offset >= 0
    then offsetSymbol (findSymbol alphabet ch) offset
    else encode (reverse alphabet) ch (-offset)
    where
        findSymbol [] _ = error "unsupported symbol: @"
        findSymbol alphabet@(a:as) ch =
            if a == ch then alphabet else findSymbol as ch
        
        offsetSymbol []     offset = offsetSymbol alphabet offset
        offsetSymbol (a:_)  0      = a
        offsetSymbol (_:as) offset = offsetSymbol as (offset - 1)

encrypt :: [Char] -> Int -> String -> String
encrypt alphabet offset normalized = [encode alphabet ch offset | ch <- normalized]

decrypt :: [Char] -> Int -> String -> String
decrypt alphabet offset = encrypt alphabet (-offset)

crackall :: [Char] -> String -> [[Char]]
crackall alphabet encrypted = [encrypt alphabet o encrypted | o<-[1 .. 25]]

substring :: String -> String -> Bool
substring "" "" = True
substring _ ""  = True


substringHelper::String -> String -> Bool
substringHelper str ""           = True
substringHelper (s:str) (p:sub)  = if(s == p) then substringHelper str sub else False

main :: IO()
main = do
--    print (crackall ['A'..'Z'] "FYY")
    print (substring "HaHelloP" "Hello")