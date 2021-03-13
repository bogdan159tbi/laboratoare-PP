reverseList :: [Integer] -> [Integer]
reverseList = copyToList []
            where 
                copyToList acc [] = acc  
                copyToList acc (x:xs) = copyToList (x:acc) xs     

reverseList2 [] = []
reverseList2   (x:list) =  (reverseList2 list) ++ [x]