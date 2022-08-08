module Lib where
-- created a custome type names Alphabet that is of Char list type
type Alphabet = [Char] 

-- creates a variable of type Alphabet, with its value being that of a rang from 'a' to 'z'
lowerCaseAlphabet :: Alphabet
lowerCaseAlphabet = ['a'.. 'z']

-- creates a variable of type Alphabet, with its value being that of a rang from 'a' to 'z'
upperCaseAlphabet :: Alphabet
upperCaseAlphabet = ['A' .. 'Z']

-- same variable functionality as previous two variables 
digits :: Alphabet
digits = ['0' .. '9']
-- isLowerCase is a function that takes one parameter Char and evaluates to Bool

isLowerCase :: Char -> Bool
-- if the char is an element in the lowerCaseAlphabet the return a positive Bool (True)
    -- elem can be either prefix by dropping the backticks of infix with
    -- the backticks
isLowerCase char = char `elem` lowerCaseAlphabet
 
 -- the same functionality as isLowerCase
isUpperCase :: Char -> Bool
isUpperCase char = char `elem` upperCaseAlphabet

 -- the same functionality as isLowerCase
isDigit :: Char -> Bool
isDigit char = char `elem` digits

 -- the same functionality as isLowerCase 
    -- checks to see if the char is not an element in the listed lists 
    -- by concatenating all of the check list 
isMisc :: Char -> Bool
isMisc char = char `notElem` lowerCaseAlphabet ++ upperCaseAlphabet ++ digits

-- this indexOf function takes two arguments 1.) Char & 2.) List of Chars & they evaluate to an Int
indexOf :: Char -> Alphabet -> Int
-- in case of an empty list
indexOf ch [] = undefined
    -- creates a list, x represents the first elementof the list the xs represents the other elements
    -- of the lsit that added recursively
    -- finds the index of the char
indexOf ch (x : xs) = if x == ch then 0 else 1 + indexOf ch xs

-- a function definition that takes two parameters and Int and a Char and evaluates to a Char
upperRotation :: Int -> Char -> Char
    -- an implemetaion of the function that uses the (!!) to find the element position
    -- of a particular element in a list, in this case the (upperCaseAlphabet)
    -- it then uses the indeOf function that takes two params a Char and a list (Alphabet)
    -- the evaluated result of indexOf is then added to the number of the rotation (n)
    -- mod is used as the number of letters of the english alphabet to determine the new Char
upperRotation n ch = upperCaseAlphabet !! ((indexOf ch upperCaseAlphabet + n) `mod` 26)
-- same as above but with lowerCase
lowerRotaion :: Int -> Char -> Char
lowerRotaion n ch = lowerCaseAlphabet !! ((indexOf ch lowerCaseAlphabet + n) `mod` 26)