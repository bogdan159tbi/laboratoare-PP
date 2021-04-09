import Data.Char

type Matrix = [[Integer]]

m = [[1,2],[3,4]]
m2 = [[5,6],[7,8]]
--[1,2] => "1 2\n"
lineToStr :: [Integer] -> String 
lineToStr line = foldr op "\n" line where
                 op x "\n" = (show x)++"\n"
                 op x acc = (show x)++" "++acc

toString:: Matrix -> String
toString  = foldr op "" where
            op line acc = (lineToStr line)++acc 

displayMatrix = putStrLn . toString

vjoin :: Matrix -> Matrix -> Matrix
vjoin = \m1 m2 -> foldr (:) m2 m1 --where
                  --op line acc = line:acc
msum :: Matrix -> Matrix -> Matrix
msum = zipWith (zipWith (+))

transpusa :: [[a]] -> [[a]]
transpusa ([]:_) = []
transpusa m = (map head m) : (transpusa (map tail m))
-- 2.6
multiplyLines :: [Integer ] -> [Integer] -> Integer 
multiplyLines l1 l2 = foldr (+) 0 (zipWith (*) l1 l2)

mprod :: Matrix -> Matrix -> Matrix
mprod m1 m2 =  map (\line -> map (multiplyLines line) (transpusa m2)) m1

type Image = [String]
logo :: Image
logo = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]
    where l1 ="        ***** **            ***** **    "
          l2 ="     ******  ****        ******  ****   "
          l3 ="    **   *  *  ***      **   *  *  ***  "
          l4 ="   *    *  *    ***    *    *  *    *** "
          l5 ="       *  *      **        *  *      ** "
          l6 ="      ** **      **       ** **      ** "
          l7 ="      ** **      **       ** **      ** "
          l8 ="    **** **      *      **** **      *  "
          l9 ="   * *** **     *      * *** **     *   "
          l10="      ** *******          ** *******    "
          l11="      ** ******           ** ******     "
          l12="      ** **               ** **         "
          l13="      ** **               ** **         "
          l14="      ** **               ** **         "
          l15=" **   ** **          **   ** **         "
          l16="***   *  *          ***   *  *          "
          l17=" ***    *            ***    *           "
          l18="  ******              ******            "
          l19="    ***                 ***             "
mask :: Image
mask = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]
    where l1 ="                       *****************"
          l2 ="                       *****************"
          l3 ="                       *****************"
          l4 ="                       *****************"
          l5 ="                       *****************"
          l6 ="                       *****************"
          l7 ="                       *****************"
          l8 ="                       *****************"
          l9 ="                       *****************"
          l10="                       *****************"
          l11="                       *****************"
          l12="                       *****************"
          l13="                       *****************"
          l14="                       *****************"
          l15="                       *****************"
          l16="                       *****************"
          l17="                       *****************"
          l18="                       *****************"
          l19="                       *****************"

toStringImg :: Image -> String
toStringImg = foldr op "" where
              op line "" = line++"\n"
              op line acc = line++"\n"++acc
displayImg = putStrLn . toStringImg


revlist :: [Char] -> [Char]
revlist  = foldl op [] where
           op acc elem = elem:acc


--[[1,2],[3,4]] => [[2,1],[4,3]]
-- image = [string] = [[char]]
flipH :: Image -> Image 
flipH  = map revlist

rotate90r :: Image -> Image
rotate90r ([]:_) = []
rotate90r l = (map revlist ) ((map head l):(rotate90r (map tail l)))

invert :: Image -> Image
invert = map (map op ) where
         op x
             | (x == '*') = ' '
             | otherwise  = '*'
maskKeep :: Image -> Image -> Image
maskKeep = zipWith (zipWith op) where
           op x1 x2
                   |(x1 == x2 && x1 == '*') = '*'
                   | otherwise  = ' '

maskDiscard :: Image -> Image -> Image
maskDiscard = zipWith (zipWith op) where
              op x1 x2
                   |(x1 == x2 && x1 == '*') = ' '
                   | otherwise  = '*'
union' :: Image -> Image -> Image
union' = zipWith (zipWith op) where
         op x1 x2
                 |(x1 /= x2 && (x1 == '*' || x2 == '*')) = ' '
                 | otherwise  = '*'
-- warning: nu stiu daca asa tre puse conditiile
transformationSequence :: [Image -> Image] -> Image -> Image
transformationSequence list img = foldr op img list where
                                  op func img = func img 
seq1 :: Image -> Image
seq1 = transformationSequence [invert, union' mask,rotate90r] -- rotate90r nu stiu sa fac rotate
--rotater = flipH transpusa ?