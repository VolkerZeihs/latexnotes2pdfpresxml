module FileOperation where

joinFiles ::  String -> String
joinFiles [] = []
joinFiles (k:r) 
  | (testOfInput (k:r) ) || (testOfInclude (k:r))  = joinFiles (readFile (findFile k:r))  ++ joinFiles (afterRightCurlyBracket r)
  | otherwise  = k:[] ++ joinFiles r 

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
