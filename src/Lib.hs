module Lib where

type Alphabet = [Char] 
lowerCaseAlphabet :: Alphabet
lowerCaseAlphabet = ['a'.. 'z']

upperCaseAlphabet :: Alphabet
upperCaseAlphabet = ['A' .. 'Z']

digits :: Alphabet
digits = ['0' .. '9']

isLowerCase :: Char -> Bool
isLowerCase char = char `elem` lowerCaseAlphabet

isUpperCase :: Char -> Bool
isUpperCase char = char `elem` upperCaseAlphabet

isDigit :: Char -> Bool
isDigit char = char `elem` digits

isMisc :: Char -> Bool
isMisc char = char `notElem` lowerCaseAlphabet ++ upperCaseAlphabet ++ digits
