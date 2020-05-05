import Data.Char

normalize :: String -> String
normalize []       = []
normalize (t:ts)   = if('a' <= t && t <= 'z') 
                            then chr(ord t - ord 'a'+ ord 'A' ):(normalize ts)
                            else if('0' <= t && t<= '9') 
                                    then "digits not allowed"
                                    else t:(normalize ts)


--encode :: [Char] -> Char -> Integer -> Char
encode alphabet ch offset = findSymbol alphabet ch
    where 
        findSymbol [] _                = error "Unsupported symbol: @"
        findSymbol alphabet@(a:as) ch  = if(a==ch) then alphabet else findSymbol as ch

        offsetSymbol [] offset = offset alphabet   





main::IO()
main = do
    print (encode ['A'..'Z'] 'C' 3) -- F