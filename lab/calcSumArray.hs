calcSum sum [] = sum
calcSum sum (elem:list) = calcSum (sum + elem + 1) list
