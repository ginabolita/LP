myLength :: [Int] -> [Int] -> Bool
myLength xs ys
    | length xs == length ys = True
    | otherwise = False

eql :: [Int] -> [Int] -> Bool
eql xs ys = (myLength xs ys) && (and (zipWith (==) xs ys))

prod :: [Int] -> Int
prod xs = foldl (*) 1 xs

prodOfEvens :: [Int] -> Int
prodOfEvens xs = (foldl (*) 1. filter even) xs

powersOf2 :: [Int]
powersOf2 = iterate (*2) 1 

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct xs ys = sum (zipWith (*) xs ys)