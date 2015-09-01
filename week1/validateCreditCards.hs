toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x > 0 = x `mod` 10 : toDigitsRev (x `div` 10)
  | otherwise = []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
  | even $ length xs = doubleEveryOtherLeft xs
  | odd $ length xs = head xs : (doubleEveryOtherLeft $ tail xs)

doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft (x:y:xs) = (2 * x) : y : (doubleEveryOther xs)
doubleEveryOtherLeft [x] = [2*x]
doubleEveryOtherLeft [] = []

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map (sum . toDigits) xs

validate :: Integer -> Bool
validate cc = ((sumDigits . doubleEveryOther . toDigits) cc) `mod` 10 == 0
