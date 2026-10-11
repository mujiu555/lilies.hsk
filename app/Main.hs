module Main (main) where

import Control.Exception (IOException, try)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import P0 (l0Tol0p)
import Pretty (prettyL0, prettyL1)
import Reader (l0)
import T01 (resolve)

-- | @cabal run lilies-hsk [file]@ reads one unit -- the named file, or stdin when
--   no file is given -- and prints it twice: first the L0 the reader made of the
--   source, then the L1 form the resolver made of that.
main :: IO ()
main = do
  args <- getArgs
  (name, src) <- case args of
    (p : _) -> do
      r <- try (readFile p) :: IO (Either IOException String)
      case r of
        Left e  -> do hPutStrLn stderr ("cannot read " ++ p ++ ": " ++ show e)
                      exitFailure
        Right s -> pure (p, s)
    [] -> do
      s <- getContents
      pure ("stdin", s)

  let tree = l0 name src
  putStrLn "==== L0 ===="
  putStr (prettyL0 tree)

  let l1 = resolve (l0Tol0p tree)
  putStrLn "==== L1 ===="
  putStr (prettyL1 l1)
