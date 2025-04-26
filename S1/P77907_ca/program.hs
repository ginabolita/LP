absValue :: Int -> Int
absValue x
  | x < 0 = -x
  | otherwise = x

power :: Int -> Int -> Int
power x p 
  | p == 0 = 1
  | even p = halfPower * halfPower
  | otherwise = x * power x (p-1)
  where
    halfPower = power x (p `div` 2)

isPrime :: Int -> Bool 
isPrime n
  | n < 2 = False
  | otherwise = not (hasDivisors n 2)
  where
    hasDivisors n d
      | d*d > n = False
      | n`mod`d == 0 = True
      | otherwise = hasDivisors n (d+1)

slowFib :: Int -> Int
slowFib x
  | x < 2 = x
  | otherwise = myFib x 2 0 1
  where
    myFib x i n1 n2
      | i == x = (n1 + n2)
      | otherwise = myFib x (i+1) n2 (n1+n2)

quickFib :: Int -> Int
quickFib x
  | x < 2 = x
  | otherwise = myFib x 2 0 1
  where
    myFib x i n1 n2
      | i == x = (n1 + n2)
      | otherwise = myFib x (i+1) n2 (n1+n2)