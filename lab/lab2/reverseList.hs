
-- mai eficient e sa copiam direct in alta lista pe masura ce parcurgem
-- daca folosim varianta a doua ar fi ineficient
-- deoarce se fac apeluri recursive pe subliste
reverseList :: [Integer] -> [Integer]
reverseList = copyToList []
            where 
                copyToList acc [] = acc  
                copyToList acc (x:xs) = copyToList (x:acc) xs     

reverseList2 [] = []
reverseList2   (x:list) =  (reverseList2 list) ++ [x]