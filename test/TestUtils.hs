module TestUtils
    (
    runParseRecordTest,
    eitherParseRecordTest
    )
where
import           Data.Csv                (FromRecord (..), parseRecord, record,
                                          runParser, toField)
import           Data.Either.Combinators (fromRight')
import           Data.Text               (Text)
import           Utils                   (toByteString)

{-
 - Take a "csv" as input and parse it into a type using the Data.Csv.parseRecord method, which will, in turn, use the appropriate fromRecord function for the desired type.
 - We assume success and return the right hand side of the Either.
 -}
runParseRecordTest :: FromRecord a => [String] -> a
runParseRecordTest xs = fromRight' . runParser . parseRecord . record $ fmap toByteString xs

{- take a list of strings that when joined by ',' would form a line of a csv file and parse it into therequired FromRecord type
 -}
eitherParseRecordTest :: FromRecord a => [Text] -> Either String a
eitherParseRecordTest = runParser . parseRecord . record . map toField
