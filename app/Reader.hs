module Reader where

import qualified L0 as N
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import Data.Void (Void)

type Parser = P.Parsec Void String

delimited :: Applicative m => m trigger -> m delimiter -> (m a -> m a) -> m a
delimited trigger delimiter f = go
  where
    go = trigger *> f go <* delimiter
