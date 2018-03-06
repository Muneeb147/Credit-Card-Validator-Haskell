-- Algorithm

--     Double the value of every second digit beginning from the right. That is, the last digit is unchanged; the second-to-last digit is doubled; the third-to-last digit is unchanged; and so on. For example, [1,3,8,6] becomes [2,3,16,6].

--    Add the digits of the doubled values and the undoubled digits from the original number. For example, [2,3,16,6] becomes 2+3+1+6+6 = 18.

--    If the result equals 0, then the number is valid. 
  --  Calculate the remainder when the sum is divided by 10. For the above example, the remainder would be 8.


import Data.List
import System.IO


  -- FUNCTION for checking the validity of credit cards
gd :: Integer -> [Integer]
gd 0 = []
gd n = (n `mod` 10) : (gd (n `div` 10))

getdigit :: Integer -> [Integer]
getdigit n = reverse (gd n)

toreverse :: Integer -> [Integer]
toreverse n = gd n 

dblscnd :: [Integer] -> Integer -> [Integer]
dblscnd [] _ = []
dblscnd (x:xs) n
    | (n `mod` 2 == 0) =  x : dblscnd xs (n+1)
    | otherwise =  x*2 : dblscnd xs (n+1)


sumllist :: [Integer] -> Integer
sumllist [] = 0
sumllist [x,y] = (sum (getdigit x)) + (sum (getdigit y))
sumllist (x:xs) = (sum (getdigit x)) + sumllist xs 

isvalid :: Integer -> String
isvalid n 
      | (sumllist (dblscnd (toreverse n) 0))  `mod` 10 == 0 = "The cardnumber is valid" 
      | otherwise = " The cardnumber is not valid "
