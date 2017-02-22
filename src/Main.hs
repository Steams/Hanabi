{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import System.Directory
import System.Console.ANSI
import System.Posix.Directory
import System.Posix.Process
import System.Posix.Types
import Data.Text hiding (head,tail)

get_prompt :: IO Text
get_prompt = do
  dir <- getCurrentDirectory
  return $ pack $ "" ++ dir ++ " | hanabi >_ "

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
    LS -> do
      putStrLn "list dirs here"
      prompt

    PWD -> do
      dir <- getCurrentDirectory
      putStrLn dir
      prompt

    EXIT ->
      return ()

    CD path -> do
      changeWorkingDirectory $ unpack path
      prompt

    CLEAR -> do
      clearScreen
      setCursorPosition 0 0
      prompt

    EXEC cmd args -> do
      pid <- forkProcess $ executeFile (unpack cmd) True (unpack <$> args) Nothing
      wait_for_child_exit pid >> prompt

    NONE -> do
      putStrLn "Not a valid command"
      prompt


parse_command :: Text -> Command
parse_command line =
  let
    parts = splitOn " " line
    command = head parts
    args = tail parts
  in
    case command of
      "ls" -> LS

      "pwd" -> PWD

      "exit" -> EXIT

      "clear" -> CLEAR

      "cd" -> CD $ head args

      _ ->
        EXEC command args

print_prompt :: Text -> IO ()
print_prompt s = do
  setSGR [SetColor Foreground Vivid Blue]
  putStr . unpack $ s
  setSGR [Reset]
  hFlush stdout


prompt :: IO ()
prompt = do
  putStrLn ""
  get_prompt >>= print_prompt
  (parse_command . pack <$> getLine) >>= exec

initialize :: IO ()
initialize = do
  clearScreen
  setCursorPosition 0 0

main :: IO ()
main = do
  initialize
  prompt
