module Main where
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric (readDec,readOct,readHex,readFloat)
import Ratio
import Complex
import Data.Array
import Control.Monad.Error
import IO hiding (try)
import Data.IORef

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

data LispVal = Atom String
             | List [LispVal]
             | Vector (Array Int LispVal)
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Char Char
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (Vector array) = "(" ++ unwordsList (elems array) ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList = unwords . map showVal

eval :: LispVal -> ThrowsError LispVal
eval (List [Atom "if", pred, conseq, alt]) =
  do result <- eval pred
     case result of
       Bool False -> eval alt
       Bool True -> eval conseq
       otherwise -> throwError $ TypeMismatch "bool" result
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Char _) = return val
eval val@(Float _) = return val
eval val@(Ratio _) = return val
eval val@(Complex _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [xs] x] = return $ x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = compareLists eqv arg1 arg2
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

compareLists cmp arg1 arg2 = return $ Bool $ (length arg1 == length arg2) &&
                                               (all (cmpPair cmp) $ zip arg1 arg2)
  where cmpPair cmp (x1, x2) = case cmp [x1, x2] of
                             Left err -> False
                             Right (Bool val) -> val


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("string?", unaryOp isString),
              ("number?", unaryOp isNumber),
              ("symbol?", unaryOp isSymbol),
              ("bool?", unaryOp isBool),
              ("list?", unaryOp isList),
              ("symbol->string", unaryOp symbol2String),
              ("string->symbol", unaryOp string2Symbol),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),
              ("cons", cons),
              ("cdr",cdr),
              ("car",car)]

unaryOp f [x] = f x

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [(List arg1), (List arg2)] = compareLists equal arg1 arg2
equal [arg1,arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

symbol2String (Atom s) = return $ String s
symbol2String notSym   = throwError $ TypeMismatch "symbol" notSym

string2Symbol (String s) = return $ Atom s
string2Symbol notString  = throwError $ TypeMismatch "string" notString

isList (List _) = return_true
isList _        = return_false

isSymbol (Atom _) = return_true
isSymbol _        = return_false

isBool (Bool _) = return_true
isBool _        = return_false

isString (String _) = return_true
isString _          = return_false

isNumber (Number _) = return_true
isNumber (Float _) = return_true
isNumber (Ratio _) = return_true
isNumber (Complex _) = return_true
isNumber _          = return_false

return_true :: ThrowsError LispVal
return_true = return $ Bool True

return_false :: ThrowsError LispVal
return_false = return $ Bool False

symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces = skipMany1 space

parseString = do
  char '"'
  x <- many (try parseEscapedChar <|> noneOf "\"")
  char '"'
  return $ String x
  where parseEscapedChar = do
          char '\\'
          x <- oneOf "nrt\\\""
          return $ case x of
                    'n' -> '\n'
                    'r' -> '\r'
                    't' -> '\t'
                    '\\' -> '\\'
                    '"' -> '"'

parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
           "#t" -> Bool True
           "#f" -> Bool False
           _    -> Atom atom

parseNumber = try parseFloat
          <|> try parseRatio
          <|> try parseComplex
          <|> parseInteger

parseInteger = (parseHex <|> parseOct <|> parseDec)
  where parseHex = (try (string "#x") >> readWith readHex)
        parseOct = (try (string "#o") >> readWith readOct)
        parseDec = (readWith readDec)
        readWith f = do
          digits <- parseInt
          return $ (Number . fst . head . f) digits

parseFloat = do
  x <- parseInt
  char '.'
  y <- parseInt
  return $ (Float . fst . head . readFloat) (x ++ "." ++ y)

parseRatio = do
  x <- parseInt
  char '/'
  y <- parseInt
  return $ Ratio ((read x) % (read y))

toDouble (Float f) = f
toDouble (Number n) = fromIntegral n

parseComplex = do  
  x <- (try parseFloat <|> parseInteger)
  char '+'
  y <- (try parseFloat <|> parseInteger)
  char 'i'
  return $ Complex (toDouble x :+ toDouble y)

parseInt = (many1 digit)

parseChar = do 
  try $ string "#\\"
  xs <- try (string "newline" <|> string "space")
        <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
  return $ Char $ case xs of
           "space"   -> ' '
           "newline" -> '\n'
           (x:[])    -> x

parseList = liftM List $ sepBy parseExpr spaces                     

parseVector = do
  internal_list <- sepBy parseExpr spaces
  return $ Vector (listArray (0,(length internal_list - 1)) internal_list)

parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseExpr = try parseChar
        <|> parseNumber
        <|> parseQuoted
        <|> parseQuasiQuoted
        <|> parseUnQuote
        <|> parseString
        <|> try (do
                   string "#("
                   x <- parseVector
                   char ')'
                   return x)
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x
        <|> parseAtom

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> evalAndPrint $ args !! 0
    otherwise -> putStrLn "Program takes only 0 or 1 argument"

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)
