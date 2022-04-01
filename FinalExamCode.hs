--Name: Ajay Mahendra Mehra 
--Roll No: 21112002

consecutiveJumps :: Int -> [Int] -> Int -> [Int]
consecutiveJumps n [] k = []
consecutiveJumps n ls k | (head ls) == k = cmp k (tail ls)
                        |  otherwise = (consecutiveJumps n (tail ls) k)


cmp :: Int -> [Int] -> [Int] 
cmp k [] = []
cmp k ls | (head ls) > k = (head ls):cmp k (tail ls)
         | otherwise = cmp k (tail ls)

count :: [Int] -> Int
count ls = 1 + (length ls)

isSolution :: Int -> [Int] -> Int -> Int
isSolution n ls k = (count (consecutiveJumps n ls k))   {--isSolution Function gives the output--}
