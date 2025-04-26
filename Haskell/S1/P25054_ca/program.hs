--donada una llista d'enters, calculi la seva llargada
myLength :: [Int] -> Int
myLength xs = countNumbers xs 0
  where
    countNumbers [] i = i
    countNumbers (_:ys) i = countNumbers ys (i+1)


--donada una llista d'enters no buida, calculi el seu maxim
myMaximum :: [Int] -> Int
myMaximum xs = maxValue xs (head xs)
  where 
    maxValue [] i = i 
    maxValue (x:ys) i = maxValue ys (max x i)

--donada una llista d'enters no buida, calculi la seva mitjana
average :: [Int] -> Float
average xs = calculateAverage xs 0 0
  where
    calculateAverage [] suma count = fromIntegral suma/ fromIntegral count
    calculateAverage (y:ys) suma count = calculateAverage ys (suma+y) (count+1)

--donada una llista, retorni el palíndrom que comença amb la llista invertida
--buildPalindrome :: [Int] -> [Int]

