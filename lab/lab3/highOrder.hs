------ LAB 3------
-- test eu

-- folds varianta veche lab
-- 60. Write a function which appends a list of lists of integers: 

-- foldr op acc [] = acc
-- foldr op acc (elem:list) = op elem (foldr op acc list)  
import Data.Char
sumList :: [Integer] -> Integer
sumList = foldr (+) 0

productList :: [Integer] -> Integer 
productList = foldr (*) 1


f1 x y = x + y
--same as
f2 x = \y -> x + y

makeLower :: [Char] -> String 
makeLower l = foldr op [] l
            where op x acc = (toLower x):acc
sumlist :: [Integer] -> Integer 
sumlist list = foldr op 0 list
            where op x y = x + y

-- reverse list using foldl ( de la st la dreapta)
reverseList :: [Char] -> [Char] 
reverseList l = foldl op [] l
          where op acc x = (x:acc)

appendl l1 l2 = foldr (:) l2 l1