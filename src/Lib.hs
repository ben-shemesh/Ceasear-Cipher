module Lib where

type Alphabet = [Char] 
lowerCaseAlphabet :: Alphabet
lowerCaseAlphabet = ['a'.. 'z']

upperCaseAlphabet :: Alphabet
upperCaseAlphabet = ['A' .. 'Z']

digits :: Alphabet
digits = ['0' .. '9']