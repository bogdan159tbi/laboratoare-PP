import Data.Char
-- cum si da seama ca adauga unu la fiecare lista
addOne  = map (\x -> x+1) 
--varianta mai lejera
--anonymus function which has only one use
addFunc' :: Integer -> Integer
addFunc' = \x -> x+1
addToList :: [Integer ]->[Integer ]
addToList list = map addFunc' list

--remove head of every list in an array of lists
removeHead :: [[Integer]] -> [[Integer]]
removeHead = map tail 

toString :: String -> [String]
toString  = map (\c -> [c]) 

suma l = foldr (\x y -> x+y) 0 l

suma2 :: [Integer] -> Integer 
suma2 l = foldr (+) 0 l
    
makeLower l = foldr (\x acc ->(toLower x):acc) [] l

reverse2 :: [a] -> [a]
-- se pune \acc x-> x:acc pentru ca la foldl func de la op are signatura
-- (b -> a -> b)
reverse2 list = foldl (\acc x-> x:acc) [] list

f = (+)
g x y = f x  y


