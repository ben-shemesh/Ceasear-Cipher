{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Lib where
import Data.Char
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
    -- elem can be either prefix by dropping the backticks or infix with
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
    -- by concatenating all of the check list into one list  
isMisc :: Char -> Bool
isMisc char = char `notElem` lowerCaseAlphabet ++ upperCaseAlphabet ++ digits

-- [ƒ] indexOf takes two arguments 1.) Char & 2.) List of Chars & they evaluate to an Int
indexOf :: Char -> Alphabet -> Int
-- in case of an empty list
indexOf ch [] = undefined
    -- creates a list, x represents the first elementof the list the xs represents the other elements
    -- of the list that added recursively (prepended)
    -- finds the index of the char
    -- add one, because it represents 1 (x) plus the index of prepended elm (xs)
indexOf ch (x : xs) = if x == ch then 0 else 1 + indexOf ch xs

-- [ƒ] a general function that takes any alphabet, Int and Char
-- and returns a new char
alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot alphabet n char =
    alphabet !! ((indexOf char alphabet + n) `mod` length alphabet)

-- [ƒ] a function that takes two parameters and Int and a Char and evaluates to a Char
upperRotation :: Int -- takes a number as an argument, representing the off set 
                     -- to be added to the old character
  -> Char -- represents the original char that is the subject of the rotation
  -> Char -- the evaluated result of the rotation that evals to a Char

upperRotation n ch = alphabetRot upperCaseAlphabet n ch
-- same as above but with lowerCase
lowerRotaion :: Int -> Char -> Char
lowerRotaion n ch =  alphabetRot lowerCaseAlphabet n ch

-- [ƒ] using helper functions to determine case distinctions 
rotationChar :: Int -> Char -> Char
rotationChar n ch
    -- isLower is part of the Data Lib
    -- uses boolean to determine if the char is lowercase in Unicode
    -- if true executes lowerRotaion function
    | isLower ch = lowerRotaion n ch
    --  same functionality
    | isUpper ch = upperRotation n ch
    -- self explanitory
    | otherwise = ch
    
ceaser :: Int -> String -> String
-- map applies a function (rotationChar) to each element of the String /[a List]
    -- each character (ch) is used in the mapping function ( -> rotationChar n ch)
        -- untill the map has exasted each character (ch) in the string [recursive ƒ]
        -- then producing a new string (message)
ceaser n message = map (\ch -> rotationChar n ch) message

rot13 :: String -> String
-- Performs ROT13 encryption by using the Caesars cipher with a fixed offset of 13
rot13 message = ceaser 13 message