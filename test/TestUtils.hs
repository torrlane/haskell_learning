module TestUtils
    (
    runParseRecordTest,
    eitherParseRecordTest 
    )
where
import Data.Either.Combinators          (fromRight')
import Data.Csv                         (FromRecord(..), record, parseRecord, runParser)
import Utils                            (toByteString)

{-
 - Take a "csv" as input and parse it into a type using the Data.Csv.parseRecord method, which will, in turn, use the appropriate fromRecord function for the desired type.
 - We assume success and return the right hand side of the Either.
 -}
runParseRecordTest :: FromRecord a => [String] -> a
runParseRecordTest xs = fromRight' . runParser . parseRecord . record $ fmap toByteString xs

eitherParseRecordTest :: FromRecord a => [String] -> Either String a
eitherParseRecordTest xs = runParser . parseRecord . record $ fmap toByteString xs
    
