flatten :: [[Int]] -> [Int]
flatten xs = foldl (++) [] xs

myLength :: String -> Int
myLength xs = foldl (+) 0 (map (const 1) xs)

myReverse :: [Int] -> [Int] 
myReverse = foldl (flip(:)) []

countIn :: [[Int]] -> Int -> [Int]
countIn xs x = map (length . filter (== x)) xs

firstWord :: String -> String
firstWord = takeWhile (/= ' ') . dropWhile (== ' ')