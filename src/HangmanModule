{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module HangmanModule where

import           Prelude       
import           Data.Char


---- data ----

type HangmanData = GuessData
type GuessData = (GuessWord, GuessTupleList)
type GuessTupleList = [GuessTuple]
type GuessTuple = (GuessChar, IsGuessedBool)

type GuessWord = String
type GuessChar = Char
type IsGuessedBool = Bool

emptyGuessChar :: Char
emptyGuessChar = '_'

--- initialize ---

initializeGuessWord :: GuessWord -> GuessData
initializeGuessWord inputString = (lowerCaseInputString,  ( map initializeCharForTuple (lowerCaseInputString) ))
                                where lowerCaseInputString = stringToLower inputString

initializeCharForTuple :: GuessChar -> GuessTuple
initializeCharForTuple inputChar = (inputChar, False)

----- runtime ----

doGuessForWord :: Char -> GuessData -> GuessData
doGuessForWord inputGuessChar (guessWord, guessTupleList) = (guessWord, fmap (doGuessPerChar inputGuessChar) guessTupleList )    

doGuessPerChar :: Char -> GuessTuple -> GuessTuple
doGuessPerChar inputChar (compareChar, compareBool) 
                                                    | isAlreadyMatched || checkIfInputCharIsInList  = (compareChar, True)
                                                    | otherwise =  (compareChar, False)
                                                    where
                                                      isAlreadyMatched = compareBool
                                                      checkIfInputCharIsInList = inputChar == compareChar
                              

isGuessed :: GuessData -> Bool
isGuessed ( _ , guessTupleList) =  and ( fmap  isTupleTrue guessTupleList ) 

isTupleTrue :: GuessTuple -> Bool
isTupleTrue ( _ , compareBool) = compareBool

getCharForWord :: GuessTuple -> Char
getCharForWord (compareChar, compareBool) 
                                          | compareBool =  compareChar
                                          | otherwise =  emptyGuessChar

--- print functions

printGame :: GuessData -> String
printGame guessData             
                                | isGuessed guessData = printWordState guessData
                                | otherwise =  printWordState guessData

printWordState :: GuessData -> String
printWordState ( _ , guessTupleList) = fmap getCharForWord guessTupleList  

---- utils ----

stringToCaps :: String -> String
stringToCaps inputString = fmap toUpper inputString

stringToLower :: String -> String
stringToLower inputString = fmap toLower inputString
