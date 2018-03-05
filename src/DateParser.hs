module DateParser where

import           Data.Text
import           Data.Time       (UTCTime)
import           Data.Void
import           Text.Megaparsec


type Parser = Parsec Void Text

data DateRepr = DateRepr


dateParser :: Parser DateRepr
dateParser = undefined

fixDateRepr :: UTCTime -> DateRepr -> UTCTime
fixDateRepr = undefined

parseDate :: UTCTime -> Text -> Maybe UTCTime
parseDate now = parseMaybe (fixDateRepr now <$> dateParser)
