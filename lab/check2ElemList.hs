atLeastTwo :: [Integer] -> Bool
atLeastTwo list
	| list == [] = False
	| otherwise = atLeastOne (tail list)
			where atLeastOne list 
				| list == [] = False
				| otherwise = True
{--
 atLeastTwo [] = False
 atLeastTwo [x] = False  [x] este pattern pt lista cu un singur element
 atLeastTwo (x:y:list) = True (elem1:elem2:list) pattern pt lista cu cel putin 2 -elem
 -- }
