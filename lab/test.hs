f [] = []
f (x:y) = (f y)++[x]