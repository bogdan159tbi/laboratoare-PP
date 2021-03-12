-- 21. Extract the fourth element from a list of integers 
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
-- cum fac daca lista are mai putin de 4 elemente ?

checkNoElem :: [Integer] -> Integer 
checkNoElem [] = 0
checkNoElem ( element: list) = 1 + checkNoElem list

fourthElem :: [Integer] -> Integer 
fourthElem [] = -1
fourthElem list 
            | checkNoElem list == 4 = 1
            | otherwise  = 0

--Write a function which checks if a list is sorted *
checkSorted :: [Integer] -> Bool 
checkSorted [] = True 
checkSorted [x] = True 
checkSorted (x : y : xs) 
            | (x < y) == True = checkSorted (y :xs)
            | otherwise = False