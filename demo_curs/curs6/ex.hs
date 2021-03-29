import Data.Char
-- functie care separa string dupa spatiu 
spaceSep :: String -> [String]
spaceSep = foldr op [] where
            op ' ' acc = []:acc
            op x [] = [[x]]
            op x (y:ys) = (x:y):ys

-- mai general pentru un separator dat

charSep :: Char -> String -> [String]
charSep sep = foldr op [] where
                op c []
                    | (c == sep) = []
                    | otherwise = [[c]]
                op c (x:xs)
                    | (c == sep) = "":x:xs
                    | otherwise  = (c:x):xs
--parsing a matrix 
-- input "1 2 3\n4 5 6\n 7 8 9\n" :: String -> [[Integer]]
-- pas 1 = separ dupa \n
-- => ["1 2 3","4 5 6","7 8 9"]

-- output = [[1,2,3],[4,5,6],[7,8,9]] 
matrix = "1 2 3\n4 5 6\n7 8 9\n"
parseM :: String -> [[Integer]]
parseM = (map (map read)).
         (map (charSep ' ')).
         (charSep '\n')

-- displaying a matrix 
-- [1,2,3] -> ["1","2","3"] -> "1 2 3\n"


displayLine :: [Integer] -> String 
displayLine = (foldr (\x acc -> x++" "++acc) "\n").(map show)

{--
((++) ((++" ") "1") "2")
((++) ("1 ") "2")
"1 2"
--}

concat2Nr :: [Char] -> [Char] -> [Char]
concat2Nr = (++).(++" ")

displayLine2 :: [Integer] -> String 
displayLine2 = (foldr  concat2Nr  "\n").(map show)

bind :: String -> [String] -> String
bind sep = foldr ((++).(++sep)) [] 
--[[1,2],[3,4]] => [["1","2"],["3","4"]] => ["1 2 ","3 4 "] => 1 2 \n 3 4 \n" 
displayMatrix :: [[Integer]] -> String
displayMatrix = (bind "\n").
                (map (bind " ")).
                (map (map show ))

--- Matrix multiplication
--- step 1 = transposition
--- m = [[1,2,3],[4,5,6], [7,8,9]]
--- prima linie = [1,4,7]
--- transpose m => [[1,4,7], [2,5,8], [3,6,9]]
transpose ([]:_) = []
transpose m = (map head m):(transpose (map tail m))


type Matrix = [[Integer]]

matrix1 = [[1,2],[3,4]]
multiplyMatrix :: Matrix -> Matrix -> Matrix
multiplyMatrix m1 m2 = map (\line -> map (value line) m3) m1 -- map (value line) m3 e ca map (+1) [1,2,3] unde value line asteapta cate o linie din m3(care era de fapt coloana in m2)
                       where m3 = transpose m2
                             value line column = foldr (+) 0 (zipWith (*) line column) -- ia linie si coloana si calc valoarea
multiplyMatrix2 m1 m2 = map (\line -> map (\column -> foldr (+) 0 (zipWith (*) line column)) $ transpose m2) m1  