{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import System.Directory
import System.Console.ANSI
import System.Posix.Directory
import System.Posix.Process
import System.Posix.Types
import Data.Text hiding (head,tail,map)

-- Aliases for bind, makes the syntax prettier
(|>>=) :: Monad m => m a -> (a -> m b) -> m b
m |>>= f = m >>= f
infixl 1 |>>=

-- Aliases for >>
(|>>) :: Monad m => m a -> m b -> m b
m |>> f = m >> f
infixl 1 |>>

-- Just an alias for $, which is an ugly symbol
(<|) :: (a -> b) -> a -> b
f <| x = f x
infixr 2 <|

get_prompt :: IO Text
get_prompt = do
  dir <- getCurrentDirectory
  return <| pack <| dir ++ " | hanabi >_ "

type Path = Text
type Program = Text
type Args = [Text]

data Command
  = LS
  | PWD
  | EXIT
  | CD Path
  | CLEAR
  | EXEC Program Args
  | NONE
  | ECHO Text

-- Retrieves the exit status of the process for pid as a Maybe
-- If the exit status is not available (ie process still running), getProcessStatus returns "Nothing"
-- Thus we continue waiting.
-- When we're done waiting, return an empty IO Monad so we can sequence back to prompt
wait_for_child_exit :: ProcessID -> IO ()
wait_for_child_exit pid = do
  status <- getProcessStatus True True pid
  case status of
    Just _ -> return ()

    Nothing -> wait_for_child_exit pid

exec :: Command -> IO ()
exec command =
  case command of
    LS ->
      getDirectoryContents "."
      |>>= mapM_ putStrLn
      |>> prompt

    PWD ->
      getCurrentDirectory
      |>>= putStrLn
      |>> prompt

    EXIT ->
      return ()

    CD path ->
      changeWorkingDirectory <| unpack path
      |>> prompt

    CLEAR ->
      clearScreen
      |>> setCursorPosition 0 0
      |>> prompt

    EXEC cmd args ->
      forkProcess <| executeFile (unpack cmd) True (unpack <$> args) Nothing
      |>>= wait_for_child_exit
      |>> prompt

    ECHO line ->
      putStrLn <| unpack line
      |>> prompt
    NONE ->
      putStrLn "Not a valid command"
      |>> prompt

parse_command :: Text -> Command
parse_command line =
  let
    parts = Data.Text.words line
    command = head parts
    args = tail parts
  in
    case command of
      "ls" -> LS

      "pwd" -> PWD

      "exit" -> EXIT

      "clear" -> CLEAR

      "cd" -> CD <| head args

      "echo" -> ECHO <| Data.Text.unwords args

      _ -> EXEC command args

print_prompt :: Text -> IO ()
print_prompt text = do
  putStrLn ""
  setSGR [SetColor Foreground Vivid Blue]
  putStr <| unpack text
  setSGR [Reset]
  hFlush stdout

prompt :: IO ()
prompt = do
  get_prompt >>= print_prompt
  (parse_command . pack <$> getLine) >>= exec

initialize :: IO ()
initialize =
  clearScreen >> setCursorPosition 0 0

main :: IO ()
main = initialize >> prompt
