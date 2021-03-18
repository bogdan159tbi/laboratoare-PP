addOne list = map plusOne list
                where plusOne x = x + 1

-- list of lists
removeHead list = map tail list

toString list = map charToStr list
                where charToStr c = [c]

{--
sum [] = 0
sum (x:xs) = x + sum(xs)
devine prin fold
--}
--ramas la 01:28
fold :: (a->b->b) -> b -> [a]->b
sum :: [Integer] -> Integer 
sum list = fold op 0 list
            where op x y = x + y