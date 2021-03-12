import Data.List


my_and :: Bool -> Bool -> Bool 
my_and True True = True 
my_and _ _ = False

my_if :: Bool -> a -> a -> a
my_if True x y = x
my_if False x y = y

largest :: [Integer] -> Integer
largest list = last (sort list)


rev :: [a] -> [a] -> [a]
rev [] acc = acc
rev list acc = rev (tail list) (head list : acc)

reversal :: [a] -> [a]
reversal list = rev list []


sum_list :: [Integer] -> Integer
sum_list [] = 0
sum_list list = head list + sum_list (tail list)

sum_list2 :: [Integer] -> Integer
sum_list2 list = foldr (+) 0 list

findLargest3 :: Integer -> Integer -> Integer -> Integer
findLargest3 x y z = my_if (x > my_if (y > z) y z) x (my_if (y > z) y z)

f7 x y z = if x then y else z

f8 x y z
          | x = y
          | otherwise = z
f9 :: Ord p => p -> p -> p -> p
f9 x y z = f8 (x > f8 (z > y) z y) x z

getThird :: [Integer] -> Integer
getThird list = head ( tail (tail (reverse list)))

oddNumber :: [Integer] -> Bool
oddNumber list
            | mod (getThird list) 2 == 1 = True
            | otherwise = False


--Implement a function which returns the sum of integers from a list. 
sumList :: [Integer] -> Integer
sumList [] = 0
sumList list = head(list) + sumList (tail list)
-- Implement a function which takes a list of booleans and returns false if at least one boolean from the list is false. 
findFalse :: [Bool] -> Bool
findFalse [] = True
findFalse list
            | head(list) == False = False
            | otherwise = findFalse (tail(list))

-- Implement a function which filters out all odd numbers from a list. 
filterOutOdd :: [Integer] -> [Integer]
filterOutOdd [] = []
filterOutOdd list
            | mod (head (list) ) 2 == 1 = filterOutOdd ( tail (list ))
            | otherwise = (head list) : filterOutOdd ( tail (list)) -- ca sa adaug nr par la sublista corecta
-- Implement a function which takes a list of booleans and returns a list of integers.
-- In the latter, (True becomes 1 and False becomes 0). Example: f [False, True, False] = [0,1,0]
convertToInteger :: [Bool] -> [Integer]
convertToInteger [] = []
convertToInteger list
                | head (list) == True = 1 : convertToInteger ( tail(list))
                | otherwise = 0 : convertToInteger ( tail (list ))


-- mai am de facut probl 18
{-
    f18 :: [[Integer]] -> [Bool]
	f18 [] = []
	f18 l = ( g18 ( head l )) :(f18 (tail l))
		where
			g18 [] = True
			g18 l = h18 (tail l)
			h18 [] = True
			h18 l = False
-}
-- Implement a function which takes a list of booleans and returns the sum of True values from the list.
-- Example f [False,True,False,False,True] = 2. 
sumConvertedBool :: [Bool] -> Integer
sumConvertedBool [] = 0
sumConvertedBool list = head ( convertToInteger (list) ) + sumConvertedBool( tail (list))

--insert element in ordered array
insertE :: Integer  -> [Integer] -> [Integer]
insertE x [] = [x]
insertE x (y : ys)
            | x < y = x : y : ys
            | otherwise = y : ( insertE x ys )

insertSort :: [Integer] -> [Integer ]
insertSort [] = []
{-
daca lista nu e goala => insert x in sublista ordonata
pana cand se ajunge la o lista de un elem si coboara recursiv
-}
insertSort (x : xs) = insertE  x $ insertSort xs  

-- where ex using auxiliary functions visible only to this function scope
whereEx x y = (increment x) + (decrement y)
            where increment v = (g v) + 1
                                where g 0 = 1
                                      g _ = 1
                  decrement v = v - 1


