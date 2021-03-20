toInt True = 1
toInt False = 0
-- nu stiu daca pot inlocui toInt cu o func anonima
convertTo :: [Bool] -> [Integer] 
convertTo = map toInt 

check :: Bool -> Integer
check False = 0
check _ = 1

matchBool :: [Bool] -> Integer 
matchBool [] = 0
matchBool (x:list)
            |(x == True) = 1 + (matchBool list)
            | otherwise  = (matchBool list)

convertBool :: [Bool] -> [Integer]
convertBool = map toInt

matchBool2 :: [Bool] -> Integer 
matchBool2 list = foldr (+) 0 (convertBool list)