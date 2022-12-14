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

import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Char8       as BS8
import           Data.Foldable         as Fold
import           Data.Maybe
import           Data.String
import qualified Data.String           as FS
import           Prelude
import           System.Directory
import           System.Environment (getArgs)
import           System.Exit
import           System.Random

import qualified HangmanModule         as Hangman

------------------------------------------------------

main :: IO ()
main = do
  arguments <- getArgs
  printLine $ "\n" ++ const_string_Message_Hangman_Title ++ "\n"
  printMenu

printMenu :: IO ()
printMenu = do

  menuInputOptionCharacter <- promptChar const_string_Message_Hangman_Menu const_string_Error_NoCharacterEntered

  case menuInputOptionCharacter of
    '1' -> initializeHangmanBuiltInDictionary
    '2' -> initializeHangmanUserDictionary
    '3' -> initializeHangmanHotSeat
    '4' -> die $ const_string_Message_Hangman_Exit
    _ -> printMenu

  printMenu

--- hangman ---



initializeHangmanBuiltInDictionary :: IO ()
initializeHangmanBuiltInDictionary = do
                                      randomWord <-getRandomWordFromStringList dictionaryOfWords
                                      playHangManLoop $ Hangman.initializeGuessWord $ randomWord

initializeHangmanUserDictionary :: IO ()
initializeHangmanUserDictionary = do 
  
  fileExists <- doesFileExist const_string_File_Hangman_Dictionary 
  if ( not fileExists )
    then printLine $ "\n" ++ const_string_Error_Hangman_DictionaryFileDoesNotExist ++ "\n"
    else do
      words <-readFile const_string_File_Hangman_Dictionary
      randomWord <- getRandomWordFromStringList $ lines $ words
      playHangManLoop $ Hangman.initializeGuessWord randomWord
  
initializeHangmanHotSeat :: IO ()
initializeHangmanHotSeat = do

    wordToGuess <- promptLine const_string_Message_EnterWord
    printLine clearScreenString
    playHangManLoop $ Hangman.initializeGuessWord wordToGuess

playHangManLoop :: Hangman.HangmanData -> IO ()
playHangManLoop hangmanData = do
    
    printLine clearScreenString
    printHangManGame $ hangmanData

    guessCharacter <- promptChar const_string_Message_EnterCharacter const_string_Error_NoCharacterEntered
    let updatedHangmanData = Hangman.doGuessForWord guessCharacter hangmanData

    if Hangman.isGuessed updatedHangmanData
      then ( printLine $ const_string_Message_GameWon ++ Hangman.printGame updatedHangmanData ++ "\n" )
      else   playHangManLoop updatedHangmanData

printHangManGame :: Hangman.HangmanData -> IO ()
printHangManGame hangmanData = printLine $ "\n" ++  Hangman.printGame hangmanData

--- utils ---

getRandomWordFromStringList :: [String] -> IO String
getRandomWordFromStringList inputStringList = do
      let wordListLength = length inputStringList
      randomNumber <- randomRIO (0, wordListLength-1)
      let randomWord = inputStringList !! randomNumber
      return randomWord

printLine :: String -> IO ()
printLine inputString = putStrLn inputString

printSameLine :: String -> IO ()
printSameLine inputString = putStr inputString

readLine :: IO String
readLine = getLine

promptLine :: String -> IO String
promptLine inputString = printLine inputString >> readLine

promptChar :: String -> String -> IO (Char) 
promptChar displayString errorString = do 
                         inputString <- promptLine displayString
                         let inputCharacter = stringToMaybeFirstChar inputString
                         if (Data.Maybe.isNothing inputCharacter )
                           then do
                              printLine errorString
                              promptChar displayString errorString
                           else
                             return $ case inputCharacter of
                                                            Just character -> character
                                                            Nothing -> '?'

stringToMaybeFirstChar :: String -> Maybe Char
stringToMaybeFirstChar inputString 
                                    | length inputString > 0 = Just (inputString !! 0)
                                    | otherwise = Nothing

                                  

printErrorIfInvalidMaybeCharacterEntered :: Maybe Char -> String -> IO ()
printErrorIfInvalidMaybeCharacterEntered character errorString
                                                  | isValidCharacter = printSameLine ""
                                                  | otherwise = printLine errorString
                                                    where 
                                                    isValidCharacter = Data.Maybe.isJust character 
              

--- consts ---

const_string_Message_Hangman_Title :: String
const_string_Message_Hangman_Title = "[ Hangman v1.0 ] "

const_string_Message_Hangman_Menu :: String
const_string_Message_Hangman_Menu = "Welcome to Hangman!\nPlease choose whether you want to\n1) Play against the computer, using a built-in dictionary\n2) Play against the computer, using a 'words.txt' file in this application directory\n3) Play in hotseat mode (enter a word by yourself)\n4) Exit\nOption: "

const_string_Message_Hangman_Exit :: String
const_string_Message_Hangman_Exit = "Thank you for playing!"

const_string_Message_EnterWord :: String
const_string_Message_EnterWord = "> Give word to guess: "

const_string_Message_EnterCharacter :: String
const_string_Message_EnterCharacter = "> Give character to guess: "

const_string_Message_GameWon :: String
const_string_Message_GameWon = ">>> You won the game!\nThe word was: "

const_string_Message_Done :: String
const_string_Message_Done = "Done"


const_string_Error_NoCharacterEntered :: String
const_string_Error_NoCharacterEntered = "[error] 'Please enter a valid character!\n"

const_string_Error_Hangman_DictionaryFileDoesNotExist :: String
const_string_Error_Hangman_DictionaryFileDoesNotExist = "[error] A file named 'words.txt' does not exist in the Hangman application directory!"


const_string_File_Hangman_Dictionary :: String
const_string_File_Hangman_Dictionary = "words.txt"

dictionaryOfWords :: [String]
dictionaryOfWords = ["apple","banana","fruit","car","train","airplane","engineer","nurse","soldier","cake","pizza","bread","bird","chicken","tiger"]

clearScreenString :: String
clearScreenString = "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
