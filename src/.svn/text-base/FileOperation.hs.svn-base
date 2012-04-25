-- | The FileOperation Module contains file operations related functions
module FileOperation where

import Operation
-- | The joinFiles function joins all included files of an Latex document.
joinFiles :: String -- ^ Latex file content 
  -> FilePath  -- ^ filepath to the Latex file containing directory
  -> IO String -- ^ return the complete content of the Latex document
joinFiles [] _ = return []
joinFiles (k:r) dirPath
  | (testOfInput (k:r) ) || (testOfInclude (k:r)) =  concatIoIo (rekursivCallJoinFiles (k:r) dirPath) (joinFiles (afterRightCurlyBracket r) dirPath)
  | otherwise = concatStringIo  (k:[])  (joinFiles r dirPath) 

-- | The concatStrinIo function is the concatenation of a String and an IO String.
concatStringIo :: String -> IO String -> IO String
concatStringIo string1 stringIo = do 
  string2 <- stringIo
  return (string1 ++ string2)  

-- | The concatIoIo function is the concatenation of two IO Strings.
concatIoIo :: IO String -> IO String -> IO String
concatIoIo string1 string2 = do
  stringOne <- string1
  stringTwo <- string2
  return (stringOne ++ stringTwo)

-- | The rekursivCallJoinFiles function calls the joinFiles function with the content of an included file
rekursivCallJoinFiles :: String -- ^ contains Latex sourcecode and should begin whith \include or \input
  -> FilePath -- ^ filepath to the Latex file containing directory
  -> IO String -- ^ return value
rekursivCallJoinFiles inputFile dirPath = do
  newFile <- readFile (findFile inputFile dirPath) 
  resultJoinFiles <- joinFiles (removeComment newFile) dirPath
  return (resultJoinFiles)

-- | The findFile function gives the content between the next curly brackets.
findFile :: String -- ^ Latex sourcecode
  -> FilePath -- ^ filepath to the Latex file containing directory
  -> FilePath -- ^ return the filepath to the included Latex file
findFile list dirPath = dirPath  ++ (findRightCurlyBracket (findLeftCurlyBracket list)) ++ ".tex"

-- | The makeXmlString composes the note tuples to an pdfPres readable XML file.
makeXmlString :: [(Int , String)] -- ^ list of slide number and note containing tuples 
  -> String -- ^ return the XML code 
makeXmlString [] = []
makeXmlString ((counter , note):r) = "<slide number=\"" ++ (show counter)  ++ "\"> " ++ note ++ " </slide>\n" ++ makeXmlString r 

-- | The writeToXml function writes the XML header and the notes to an pdfPres XML file.
writeToXml :: FilePath -- ^ filepath to the XML file
  -> [(Int, String)]  -- ^ list of slide number and note containing tuples
  -> IO () 
writeToXml filePath notes  = writeFile filePath  ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<notes>"  ++ (makeXmlString notes) ++ "</notes>")

-- | The getTexFilesDirectory returns the Latex file containing directory.
getTexFilesDirectory :: FilePath -- ^ filepath to the Latex file
  -> FilePath -- ^ return filepath to directory
getTexFilesDirectory [] = []
getTexFilesDirectory filePath 
  | ((last filePath) == '/') = filePath 
  | otherwise = getTexFilesDirectory (init filePath)
