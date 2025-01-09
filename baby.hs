test = stringFromList [trimFloat (show (x / 2)) | x <- [1 .. 500], x `fRem` 7 == 0 || hasSeven x]

doubleMe x = x + x

fRem x y = x - (y * fromIntegral (truncate (x / y)))

matMult x y = []

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

hasSeven :: (Num a, Show a) => a -> Bool
hasSeven xs = [x | x <- show xs, x == '7'] /= []

stringFromList :: [[Char]] -> [Char]
stringFromList [] = []
stringFromList xs = head xs ++ ", " ++ stringFromList (drop 1 xs)

trimFloat :: [Char] -> [Char]
trimFloat x =
  if last x == '0'
    then reverse (drop 2 (reverse x))
    else x
