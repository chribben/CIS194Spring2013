main = do
    number <- readInt
    putStr "toDigits: " 
    print (toDigits(number))
    putStr "toDigitsRev: " 
    print (toDigitsRev(number))
    putStr "doubleEveryOther: " 
    print (doubleEveryOther(toDigits(number)))
    putStr "sumDigits: " 
    print (sumDigits(doubleEveryOther(toDigits(number))))
    putStr "validate: " 
    print (validate number)
readInt :: IO Integer
readInt = readLn
toDigits, toDigitsRev :: Integer -> [Integer]
toDigits i = if i > 0 then 
                let lastDig = rem i 10 in toDigits (i `div` 10) ++ [lastDig]
             else 
                []
toDigitsRev i = if i > 0 then 
                    let lastDig = rem i 10 in lastDig : toDigitsRev (i `div` 10)
                else 
                    [] 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = 
                if l == [] then 
                    []
                else
                    let h:t = l 
                        fac = if rem (length l) 2 == 0 then 2 else 1 
                    in
                        (h*fac):doubleEveryOther(t)

sumDigits :: [Integer] -> Integer
sumDigits l = if l == [] then
                0
              else
                let h:t = l
                    r = rem h 10
                    d = div h 10
                in 
                    r + d + sumDigits t
validate :: Integer -> Bool
validate number = 0 == rem (sumDigits . doubleEveryOther . toDigits $ number) 10