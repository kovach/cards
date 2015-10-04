{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parse
import System.Environment


main = do
  f <- readFile "prog.txt"
  args <- getArgs
  let command = if length args > 0 then prog else finish prog
  case runParser command f of
    xs -> do
      mapM_ print $ take 5 xs
      print $ length xs
