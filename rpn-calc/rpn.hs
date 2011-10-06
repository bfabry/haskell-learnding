import Data.List
import Control.Monad

solveRPN ::  String -> Maybe Double
solveRPN expression = do
  [result] <- foldM processSymbol [] (words expression)
  return result

processSymbol :: [Double] -> String -> Maybe [Double]
processSymbol (x:y:stack) "*" = return ((x * y):stack)
processSymbol (x:y:stack) "+" = return ((x + y):stack)
processSymbol (x:y:stack) "-" = return ((y - x):stack)
processSymbol (x:y:stack) "/" = return ((y / x):stack)
processSymbol (x:y:stack) "^" = return ((y ** x):stack)
processSymbol (x:stack) "ln"  = return (log x:stack)
processSymbol (stack) "sum"   = return ([sum stack])
processSymbol stack numberString = liftM (:stack) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe num_str = case reads num_str of [(x,"")] -> Just x
                                          _ -> Nothing 
