import Data.Char(toUpper)
import System.IO

main :: IO ()
main = do
         hSetBuffering stdin NoBuffering
         hSetBuffering stdout NoBuffering
         interact (map toUpper)
