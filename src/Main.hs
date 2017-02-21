module Main where

import System.IO
import System.Directory
import System.Console.ANSI

--- I Hate $ syntax for function application. Who thought that was a good idea ???
--- Must redefine to a sane operator
(<|) :: (a -> b) -> a -> b
(<|) = ($)

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

title :: IO String
title = do
  dir <- getCurrentDirectory
  return ( "" ++ dir ++ " | hanabi >_ " )

type Path = String

data Command
  = LS
  | PWD
  | EXIT
  | CD Path
  | CLEAR
  | NONE

exec :: Command -> IO ()
exec command =
  case command of
    LS -> do
      putStrLn "list dirs here"
      prompt

    PWD -> do
      dir <- getCurrentDirectory
      putStrLn ( dir ++ "\n" )
      prompt

    EXIT ->
      return ()

    CD path -> do
      putStrLn ( "changing directories not yet implemented :: " ++ path ++ "\n")
      prompt

    CLEAR -> do
      clearScreen
      setCursorPosition 0 0
      prompt

    NONE -> do
      putStrLn "Not a valid command"
      prompt


parse_command :: String -> Command
parse_command line =
  case line of
    "ls" -> LS

    "pwd" -> PWD

    "exit" -> EXIT

    "clear" -> CLEAR

    'c':'d':xs -> CD xs

    _ -> NONE

print_prompt :: String -> IO ()
print_prompt s = do
  setSGR [SetColor Foreground Vivid Blue]
  putStr s
  hFlush stdout
  setSGR [Reset]

initialize :: IO ()
initialize = do
  clearScreen
  setCursorPosition 0 0

prompt :: IO ()
prompt = do
  tltl <- title
  print_prompt tltl
  command <- parse_command <$> getLine
  exec command

main :: IO ()
main = do
  initialize
  prompt
