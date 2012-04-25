module Main (main) where

import FileOperation
import NoteOperation
import Operation

-- | Main Function
main = do
  putStrLn "Filepath to Tex File: "
  texFilePath <- getLine
  
  mainTexFile <- readFile texFilePath -- ^ read main tex files

  allTexFiles <- joinFiles (removeComment mainTexFile) (getTexFilesDirectory  texFilePath) -- ^ join all included tex files and remove tex comments
 
  putStrLn "Filepath to XML File: "
  xmlFilePath <- getLine
  writeToXml  xmlFilePath (elimRedundancy (joinNotes allTexFiles 0 [])) -- ^ Generate notes and write them to the XML file. The first parameter allTexFiles contains all included tex files. The second parameter gives the initial value for the slide counter, you can use this parameter to compensate the framebreakes in the table of content. 

