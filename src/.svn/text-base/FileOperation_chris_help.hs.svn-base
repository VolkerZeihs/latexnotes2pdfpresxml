module FileOperation where

--import Control.Monad

joinFiles ::  String -> IO String
joinFiles [] = return []
joinFiles (k:r) 
--  | (testOfInput (k:r) ) || (testOfInclude (k:r))  = joinFiles (readFile (findFile k:r))  ++ joinFiles (afterRightCurlyBracket r)
-- (readFile filePath) >>= (\f -> test' (init f))
{-
 test' filePath = readFile filePath >>= \f -> 
                    readFile (init f) >>= \d -> return (f ++ d)
-}
-- Das ist die etwas komplexere Variante, tut aber das selbe wie unten
  | (testOfInput (k:r) ) || (testOfInclude (k:r))  = readFile (findFile (k:r)) >>= \readOne ->
                                                       joinFiles readOne >>= \joinOne -> 
                                                         joinFiles (afterRightCurlyBracket r) >>= \joinTwo ->
                                                           return $ joinOne ++ joinTwo
{- -- Das ist die einfache Variante
  | (testOfInput (k:r) ) || (testOfInclude (k:r))  = do
      readOne <- readFile (findFile (k:r))
      joinOne <- joinFiles readOne
      joinTwo <- joinFiles (afterRightCurlyBracket r)
      return (joinOne ++ joinTwo)
-}
  | otherwise  = do
      joinOne <- joinFiles r
      return $ (k:[]) ++ joinOne

findFile :: String -> FilePath 
findFile list = findRightCurlyBracket (findLeftCurlyBracket list)

afterRightCurlyBracket :: String -> String
afterRightCurlyBracket [] = []
afterRightCurlyBracket (k:r)
  | (k == '}') = r
  | otherwise = afterRightCurlyBracket r

findRightCurlyBracket :: String -> String
findRightCurlyBracket [] = []
findRightCurlyBracket (k:r)
  | (k == '}') = []
  | otherwise = k:[] ++ findRightCurlyBracket r


findLeftCurlyBracket :: String -> String
findLeftCurlyBracket [] = [] 
findLeftCurlyBracket (k:r) 
  | (k == '{') = r
  | otherwise = findLeftCurlyBracket r


testOfInput :: String -> Bool
testOfInput ('\\':'i':'n':'p':'u':'t':r) = True
testOfInput _ = False

testOfInclude :: String -> Bool
testOfInclude ('\\':'i':'n':'c':'l':'u':'d':'e':r) = True
testOfInclude _ = False
