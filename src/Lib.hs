module Lib where

isLowerCase :: Char -> Bool
isLowerCase char = char `elem` lowerCaseAlphabet

isUpper :: Char -> Bool
isUpper char = char `elem` upperCaseAlphabet

isDigit :: Int -> Bool
isDigit digi = digi `elem` digits


type Alphabet = [Char] 
lowerCaseAlphabet :: Alphabet
lowerCaseAlphabet = ['a'.. 'z']

upperCaseAlphabet :: Alphabet
upperCaseAlphabet = ['A' .. 'Z']

digits :: Alphabet
digits = ['0' .. '9']