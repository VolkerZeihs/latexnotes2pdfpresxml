-- | The Operation module contains string operations.
module Operation where

-- | The afterRightCurlyBracket function returns the String after the first closing curly bracket.
afterRightCurlyBracket :: String -- ^ complete string
  -> String -- ^ returns string after the first right curly bracket
afterRightCurlyBracket [] = []
afterRightCurlyBracket (k:r)
  | (k == '}') = r
  | otherwise = afterRightCurlyBracket r

-- | The findRightCurlyBracket function returns the string until the first closing curly bracket.
findRightCurlyBracket :: String -> String
findRightCurlyBracket [] = []
findRightCurlyBracket (k:r)
  | (k == '{') = nested (k:r)
  | (k == '}') = []
  | otherwise = k:[] ++ findRightCurlyBracket r

-- | The nested function allows to use simple nested curly brackets in the Latex notes.
nested :: String -> String
nested [] = []
nested (k:r)
  | (k == '}') = ('}':[]) ++ (findRightCurlyBracket r)
  | otherwise = (k:[]) ++ (nested r)

-- | The findLeftCurlyBracket function returns the given string after the first left curly bracket.
findLeftCurlyBracket :: String -> String
findLeftCurlyBracket [] = [] 
findLeftCurlyBracket (k:r) 
  | (k == '{') = r
  | otherwise = findLeftCurlyBracket r

-- | The testOfNote function returns true if the beginning of the given string is \note{.
testOfNote :: String -> Bool
testOfNote ('\\':'n':'o':'t':'e':'{':_) = True
testOfNote _ = False

-- | The testOfFrame function returns true if the beginning of the given string is \frame{ or \frame[.
testOfFrame :: String -> Bool
testOfFrame ('\\':'f':'r':'a':'m':'e':'{':_) = True
testOfFrame ('\\':'f':'r':'a':'m':'e':'[':_) = True
testOfFrame _ = False

-- | The testOfInput function returns true if the beginning of the given string is \input{.
testOfInput :: String -> Bool
testOfInput ('\\':'i':'n':'p':'u':'t':'{':_) = True
testOfInput _ = False

-- | The testOfInclude function returns true if the beginning of the given string is \include{.
testOfInclude :: String -> Bool
testOfInclude ('\\':'i':'n':'c':'l':'u':'d':'e':'{':_) = True
testOfInclude _ = False

-- | The testOfPause function returns true if the beginning of the given string is \pause.
testOfPause :: String -> Bool
testOfPause ('\\':'p':'a':'u':'s':'e':_) = True
testOfpause _ = False

-- | The removeComment function removes all extraneous source code (comments).
removeComment :: String -> String
removeComment [] = []
removeComment [k] = [k]
removeComment (k0:k1:r)
  | ((k0 /= '\\') && (k1 == '%')) =  (k0:[]) ++ (removeComment  (removeUntilLf (k1:r)))
  | (k0 == '%') = removeComment (removeUntilLf (k0:k1:r))
  | otherwise = (k0:k1:[]) ++ (removeComment r)

-- | The removeUntilLf function returns a string after the first linefeed.
removeUntilLf :: String -> String
removeUntilLf [] = []
removeUntilLf (k:r)
  | (k == '\n') = r
  | otherwise = removeUntilLf r


