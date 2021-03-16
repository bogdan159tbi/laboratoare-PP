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

--23. pentru liste cu 4 elem sau mai mult return al 4 -lea elem ,altfel return 0
f (x:y:z:w:l) = w
f _ = 0

--24.use patterns to filter out no < 0
{--
checkSorted :: [Integer] -> Bool
checkSorted [] = True 
checkSorted [x] = True 
checkSorted (x : y : xs) 
 | (x < y) == True = checkSorted (y :xs)
 | otherwise = False

--}
filterOutNeg :: [Integer] -> [Integer]
filterOutNeg [] = []
filterOutNeg (elem : list)
            | (elem < 0) =  ( filterOutNeg  list)
            | otherwise = elem : (filterOutNeg list )

insertE :: Integer  -> [Integer] -> [Integer]
insertE x [] = [x]
insertE x (y : ys)
            | x < y = x : y : ys
            | otherwise = y : ( insertE x ys )

{-
daca lista nu e goala => insert x in sublista ordonata
pana cand se ajunge la o lista de un elem si coboara recursiv
-}
insertSort :: [Integer] -> [Integer ]
insertSort [] = []
insertSort (x : xs) = insertE  x $ insertSort xs

--  28. Write a function which returns True 
--     if the third largest element from a list is positive 
--     am folosit insertion sort din lab1
checkThirdPositive :: [Integer] -> Bool
checkThirdPositive [] = True
checkThirdPositive list =  head( tail ( tail (insertSort list))) > 0

-- 27
{--
func "321CB" [("321CB", ["Matei", "Andrei", "Mihai"]), ("322CB",["George, Matei"])] = ["Matei", "Mihai"]
-- return numele care incep cu M din grupa 321cb ??
func group studList
            | studList == [  ] = []
            | otherwise = [  ]

--}

-- 29 f(x) = g(x + 1)= 2x + 2
f29 x = (g.h) x
        where g x = 2*x
              h x = x + 1


{--
asta e gresit
f229 x = ff x
	where gg x = 2*x
	      hh x = x + 1
	      ff = f229.hh
--}

-- nu stiu cum sa fac pentru cazul cand exista mai putin de 3 elemente
checkPositive :: [Integer] -> Bool
checkPositive list = (checkGreater.getElem) list
                        where checkGreater x
                                            | (x > 0 ) = True
                                            | otherwise  = False
                              getElem list =  head ( tail (tail list))

-- s a schimbat notarea ex la lab 2 aparent :-?
-- 2.1.1. Write a function which returns the number of elements from a list. Use patterns. 
noElem [] = 0
noElem (elem : list) = 1 + noElem(list)

-- 2.1.2 concatenate lists

concatList [[]] = []
concatList[list1] = list1
concatList (sublist:list) = sublist++concatList(list)

-- 2.1.3 same as 2.1.2 this time using only the cons (:) operator as well as patterns. 

-- 2.1.4. Write a function which removes duplicates from a list of integers. 
{--
--}

checkElem elem [] = False 
checkElem elem (first:list) 
                  | (elem == first ) = True
                  | otherwise = checkElem elem list

-- am reusit sa o fac
-- daca exista duplicate
-- elimina elem pana cand ajungem la ultimul elem egal cu restul 
-- le am ignorat pe primele egale pana am ajuns la ultimul 
-- pe care l concatenez la noua lista

removeDuplicate []  = []
removeDuplicate list  = remove  (insertSort(list)) 
                          where 
                                remove []  = []
                                remove (first:list) 
                                                | (checkElem first list) == False = first:(remove list)  
                                                |  otherwise  = remove list

                                    
