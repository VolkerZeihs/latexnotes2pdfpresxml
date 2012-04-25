-- | The NoteOpreation Module conatins functions to work with Notes
module NoteOperation where

import FileOperation
import Operation

-- | The joinNotes function finds all notes in a Latex sourcecode and combines them in a list of tuples.
joinNotes :: String -- ^ Latex source code
  -> Int -- ^ counter to remember the slide numbers every function call
  -> [(Int , String)] -- ^ accumulator for rest recursive function
  -> [(Int , String)] -- ^ returns a list of tuples (slide number, note)
joinNotes [] _ akku = akku
joinNotes texContent frameCounter akku 
  | (testOfFrame texContent) = joinNotes (afterRightCurlyBracket texContent) (frameCounter + 1) akku
  | (testOfPause texContent) = joinNotes (tail (tail (tail (tail (tail texContent))))) (frameCounter + 1) akku
  | (testOfNote texContent) = joinNotes (afterRightCurlyBracket texContent) frameCounter (akku ++ (createNoteTupel texContent frameCounter))
  | otherwise = joinNotes (tail texContent) frameCounter akku

-- | The createNoteTupel function returns a list with one tuple in it. The tuple contains the slidenumber and the note.
createNoteTupel :: String -- ^ Latex source code, it should begin with \note
  -> Int -- ^ slide number
  -> [(Int , String)] -- ^ return value
createNoteTupel texContent frameCounter = (frameCounter , findRightCurlyBracket (findLeftCurlyBracket texContent)):[]

-- | The elimRedundancy function unites tuples with same slide number.
elimRedundancy :: [(Int , String)] -- ^ list with redundancy
  -> [(Int , String)] -- ^ return list without redundancy
elimRedundancy [] = []
elimRedundancy [note] = [note]
elimRedundancy ((counter0 , note0):(counter1 , note1):r)
  | (counter0 == counter1) = elimRedundancy ((counter0 , (note0 ++ "\n" ++ note1)):r)
  | otherwise = ((counter0 , note0):[]) ++ elimRedundancy ((counter1 , note1):r)
