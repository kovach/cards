{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parse


main = do
  f <- readFile "prog.txt"
  case runParser prog $ f of
    xs -> do
      mapM_ print $ take 5 xs
      print $ length xs
