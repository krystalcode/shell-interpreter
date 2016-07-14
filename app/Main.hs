{-# LANGUAGE OverloadedStrings #-}

module Main where

import ShellInterpreter.Interpretation

main :: IO ()
main = print $ make interpretation
