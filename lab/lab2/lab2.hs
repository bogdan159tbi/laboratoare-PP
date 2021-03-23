-- 21. Extract the fourth element from a list of integers 
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
-- cum fac daca lista are mai putin de 4 elemente ?
import Data.Char
import Data.List
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
noElem :: [a] -> Integer
noElem [] = 0
noElem (elem : list) = 1 + noElem(list)

-- 2.1.2 concatenate lists
concatList :: [[a]] -> [a]
concatList [] = []
concatList (sublist:list) = sublist ++ (concatList list)

-- 2.1.3 same as 2.1.2 this time using only the cons (:) operator as well as patterns. 
concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 ([] : list) = concat2 list
concat2 ( (elem : sublist) : list) = elem : (concat2 (sublist : list))
--func care elimina nr egale cu 2 din lista
remove2 :: [Integer ] -> [Integer ]
remove2 [] = []
remove2 (elem : list) 
                  | (elem == 2) = (remove2 list)
                  | otherwise = elem : (remove2 list)
-- 2.1.4. Write a function which removes duplicates from a list of integers. 
{--
functie care verifica daca exista elem 
si fac cu guards
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

                                    
-- 2.3.1  2.3.1. Write a function which takes a list of words and makes the first letter of each word uppercase. 
toUpperFirst :: [[Char]] -> [ [Char] ]
toUpperFirst [] = []
toUpperFirst ( (letter: word) : words) = (( toUpper letter):word): (toUpperFirst words)

-- SAU

convertChar :: [String] -> [String]
convertChar [] = []
convertChar (word : words) = (makeUpper word) : (convertChar words)
                              where makeUpper [] = []
                                    makeUpper (letter : x) = (toUpper letter):x


-- 2.3.3 (!) Write a function which takes a text and a pattern and returns the number of occurrences of the pattern in the text. Example: 
-----         pattern   sir de cautat este prefix sau nu ?

-- ia prefix pentru ca verificam caracter cu caracter
-- daca pattern ul se regaseste in fiecare sub cuvant
matchFirst :: String -> String -> Bool 
matchFirst [] str = True
matchFirst _ [] = False
matchFirst (letter : word) (letter2 : str) = (letter == letter2)  && (matchFirst word str)

--        sir       pattern   nr aparitii
search :: String -> String -> Integer 
search [] pattern = 0
search (letter: stream ) pattern 
                              | (matchFirst pattern (letter : stream)) == True = 1 + (search stream pattern)
                              | otherwise  = (search stream pattern)
-- 2.3.4 find students in certain group whose names start with M
-- nu stiu sigur daca e ok sa scriu mai multe functii in modul asta
startsWithM :: [Char] -> Bool 
startsWithM [] = False 
startsWithM (letter : word) 
                        | (letter == 'M') = True
                        | otherwise = False 

foundGroup :: [Char] -> [Char] -> Bool 
foundGroup [] [] = False 
foundGroup group gr 
                  | (group == gr) = True
                  | otherwise  = False
getNames :: [String] -> [String]
getNames [] = []
getNames(name:names) 
                | (startsWithM name) = name: (getNames names)
                | otherwise = (getNames names)
findKidM :: String -> [ (String,[String]) ] -> [String]
findKidM group [] = []
findKidM group ((grp,names):serie)
               | (foundGroup group grp) =  getNames(names)
               | otherwise = (findKidM group serie)


-- 2.3.5
-- f["Matei", "Mihai"] ["Popovici","Dumitru"] = [("Matei","Popovici"), ("Mihai","Dumitru")]

matchNames :: [String] -> [String] -> [(String,String)]
matchNames [] [] = []
matchNames (sur : listSur) (name: listNames) = (sur,name): (matchNames listSur listNames)

-- 2.3.6
--   f ["Matei", "Mihai"] ["Popovici","Dumitru"] = ["MPopovici", "MDumitru"]
matchNames2 :: [String] -> [String] -> [String]
matchNames2 [] [] = []
matchNames2 ((letter:sur) : listSur) (name: listNames) = (letter:name): (matchNames2 listSur listNames)

-- 2.3.7
-- remove student with grade < 5
-- grade++ for students with three names

--f4 [("Dan Matei Popovici",9),("Mihai",4),("Andrei Alex",6)] =  	[(["Dan", "Matei", "Popovici"],10),(["Andrei", "Alex"],6)]

-- verifica lista de nume 
has3Names :: [String] -> Integer    
has3Names [] = 0
has3Names (name: names) = 1 + (has3Names names)

--adauga +1 pt elev cu 3 nume
addBonus :: ([String],Integer) -> ([String],Integer)
addBonus (names,grade)
                  | ((has3Names names) == 3) = (names,grade + 1)
                  | otherwise  = (names,grade)

-- varianta curs pt separarea de spatii 
-- 2.3.7
spaceSep :: String -> [String]
spaceSep = foldr op [] 
                where op :: Char -> [String] -> [String]
                      op ' ' acc = []:acc
                      op x [] = [[x]]
                      op x (y:acc) =  (x:y):acc

checkStudents :: [(String,Integer)] -> [([String],Integer)]
checkStudents [] = []
checkStudents ((names,grade) : list) 
                        | (grade > 5) = (addBonus ((spaceSep names),grade)):(checkStudents list) 
                        | otherwise = checkStudents list
-- in loc de spaceSep names putem folosi words name ,fac acelasi lucru


-- recap 2.3.3
isPrefix :: String -> String -> Bool 
isPrefix _ [] = True
isPrefix [] _ = False  
isPrefix (ltr : str) (ltr2 : substr) = (ltr == ltr2) && (isPrefix str substr)

findPattern :: String -> String -> Integer 
findPattern [] _ = 0
findPattern (ltr:word) substr 
                  |(isPrefix (ltr:word) substr) = 1 + (findPattern word substr) 
                  | otherwise  = (findPattern word substr)
-- use splitOn
