import Data.Char
import Data.List
input = ["matei@gmail.com", "mihai@gmail.com", "tEst@mail.com", "email@email.com", "short@ax.ro","short@ax.ro"]
--1.1

remUpper :: String -> String
remUpper = map toLower

makeUpper :: [String] -> [String]
makeUpper = map remUpper
--second way
toUpperList :: [String] -> [String]
toUpperList = (map.map) toLower  

------- 1.2
longer :: Int -> [String] -> [String]
longer size = filter (\str -> (length str) < size)

longer2 :: Int -> [String] -> [String]
longer2 size = filter ((<size).length) 

------- 1.3
checkLen :: String ->Integer -> Integer 
checkLen str acc
        | (length str > 12) = 1 + acc
        | otherwise  = acc

howmany :: [String] -> Integer 
howmany = foldr checkLen 0
-- inca nu stiu cum ar tre cu func anonime
howmany2 :: [String] -> Integer 
howmany2 = foldr (\x y -> if (length x) > 12 then 1 + y else y) 0

-- 1.4


splitEmail :: String -> [String]
splitEmail = foldr op [] where
                op :: Char -> [String] -> [String]
                op '@' acc = []:acc
                op x [] = [[x]]
                op x (y:ys) = (x:y):ys

names_emails :: [String] -> [[String]]
names_emails = map splitEmail        


--member :: String -> [[String]] -> Bool
--member domain = 
isUnique :: String -> [[String]] -> Integer
isUnique domain = foldr op  0 where
                        op ::  [String] -> Integer  -> Integer
                        op [] acc  = acc 
                        op list acc 
                                | (domain == head (tail list)) = 1 + acc 
                                | otherwise  = acc
          

remDupl :: [[String]] -> [[String]]
remDupl = foldr (\address acc -> if (isUnique (head (tail address)) acc )  < 1 then address:acc else acc) [] 

concatAddress :: [[String]] -> [String]
concatAddress = foldr op [] where
                op [] acc = acc
                op addr acc = ((head addr)++"@"++(head(tail addr))) : acc

removeDuplicates :: [String] -> [String]
removeDuplicates list = concatAddress (remDupl (names_emails list))

-- 1.5
{--
(\email acc -> if domain == (tail email)  then 1 + acc else acc)
    as avea nevoie de o functie care separa domeniul de restul emailului. Aceasta e definita deja la exercitiul anterior
    
    as folosi aceasta functie pentru a izola domeniile din lista (folosind un map)
    
    apoi, eliminarea duplicatelor este un fold right, in care acumulatorul va tine domeniile unice

    in final, pentru a verifica daca un element este duplicat, trebuie sa implementezi o functie member,
    care poate fi realizata folosind un fold (left sau right, cel mai eficient ar fi left, dar nu ne gandim inca la asta).
--}
getDomain :: String -> String
getDomain name
        |( (head name) == '@') = (tail name)
        | otherwise = (getDomain (tail name))

isDuplicate :: String -> [String] -> Bool 
isDuplicate domain [] = False
isDuplicate domain (addr:list) 
                | (domain == (getDomain addr)) = True
                | otherwise  = isDuplicate domain list

domains :: [String] -> [String]
domains = foldr (\str acc -> if isDuplicate (getDomain str) acc then acc else str:acc ) []

-- 1.6
-- "ion@gmail.com"
-- adaug la sfarsitul fiecarui string din lista
splitl :: String -> [String]
splitl =  foldl op [] where
                op acc '@' = []:acc
                op [] x = [[x]]
                op  (y:ys) x = (y++[x]):ys -- formez o lista dintr un carac si o cancatenez la sf string ului

-- 1.7
