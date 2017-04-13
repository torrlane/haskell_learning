module Main where

import Lib
import System.IO 

main :: IO ()
-- main = someFunc
main = do  
    handle <- openFile "C:/Users/John/Downloads/security_movements (2).csv" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle
