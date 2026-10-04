module Main (main) where

import Reader (l0)

main :: IO ()
main = do
  print (show (l0 "repl" "(0 2 a #:b \n)"))
