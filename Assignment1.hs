import Data.Char

charCat :: Char -> Int
charCat x
     | isLower x = 1
     | isUpper x = 2
     | isDigit x = 3
     | otherwise = 4 

coneVolume :: Float -> Float -> Float
coneVolume x y 
     | (x < 0) || (y < 0) = 0
     | otherwise = ((pi*x^2*y)/3)

oddDigits :: Integer -> Bool
oddDigits x
     | (x <= 0) = error “Enter a number greater than 0:”
     | (x <= 9) = True
     | otherwise = oddDigits (div x 10) == False 

rpm :: Int -> Int -> Int
rpm x y 
     | (y == 1) = x 
     | (y == -1) = -x
     | (x == 0) || (y == 0) = 0
     | ((mod y 2) == 1) = x + rpm (x*2) (div y 2)
     | ((mod y 2) == 0) = rpm (x*2) (div y 2)


      
     
