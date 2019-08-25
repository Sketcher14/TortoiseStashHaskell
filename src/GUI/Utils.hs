module GUI.Utils
  ( File(..)
  , DataState(..)
  , parseFullPath
  , emptyFile
  , replaceBoxOnButtonInBox
  , replaceButtonOnBoxInBox
  , getButton
  , getBox
  , getFCButton
  , getEntry
  , getDialog
  , getFCDialog
  , startDataState
  , updateDataState
  , ButtonsPack(..)
  , BoxesPack(..)
  , FCButtonsPack(..)
  , getFileFromDataState
  , createFullPath
  )
where


import Data.List
import Data.Maybe

import Graphics.UI.Gtk


data File = File
  { name :: String
  , path :: String
  }
  deriving (Show)

data DataState = DataState
  { file1 :: File
  , file2 :: File
  , file3 :: File
  , file4 :: File
  , file5 :: File
  } deriving (Show) 

emptyFile :: File
emptyFile = File { name = "", path = "" }

extension :: String
extension = "ts"


parseFullPath :: Maybe String -> File
parseFullPath Nothing = emptyFile
parseFullPath (Just fullPath) = File { name = newName, path = newPath }
  where
    index = last $ elemIndices '/' fullPath
    newName = drop (index + 1) fullPath
    newPath = take index fullPath


startDataState :: DataState
startDataState = DataState
  { file1 = emptyFile
  , file2 = emptyFile
  , file3 = emptyFile
  , file4 = emptyFile
  , file5 = emptyFile
  }

updateDataState :: DataState -> Int -> File -> DataState
updateDataState state 1 file = state { file1 = file }
updateDataState state 2 file = state { file2 = file }
updateDataState state 3 file = state { file3 = file }
updateDataState state 4 file = state { file4 = file }
updateDataState state 5 file = state { file5 = file }
-- updateDataState state another???? file

getFileFromDataState :: DataState -> Int -> File
getFileFromDataState state 1 = file1 state
getFileFromDataState state 2 = file2 state
getFileFromDataState state 3 = file3 state
getFileFromDataState state 4 = file4 state
getFileFromDataState state 5 = file5 state


createFullPath :: File -> String
createFullPath File { name = name, path = path } = path ++ "/" ++ name ++ "." ++ extension


replaceButtonOnBoxInBox :: Box -> Button -> Box -> IO ()
replaceButtonOnBoxInBox parent old new = do
  position <- get parent $ boxChildPosition old
  containerRemove parent old
  containerAdd parent new
  boxReorderChild parent new position

replaceBoxOnButtonInBox :: Box -> Box -> Button -> IO ()
replaceBoxOnButtonInBox parent old new = do
  position <- get parent $ boxChildPosition old
  containerRemove parent old
  containerAdd parent new
  boxReorderChild parent new position


data ButtonsPack = ButtonsPack
  { but1 :: Button
  , but2 :: Button
  , but3 :: Button
  , but4 :: Button
  , but5 :: Button
  }

data BoxesPack = BoxesPack
  { box1 :: Box
  , box2 :: Box
  , box3 :: Box
  , box4 :: Box
  , box5 :: Box
  }

data FCButtonsPack = FCButtonsPack
  { fcBut1 :: FileChooserButton
  , fcBut2 :: FileChooserButton
  , fcBut3 :: FileChooserButton
  , fcBut4 :: FileChooserButton
  , fcBut5 :: FileChooserButton
  }


getButton :: Builder -> String -> IO Button
getButton builder = builderGetObject builder castToButton

getBox :: Builder -> String -> IO Box
getBox builder = builderGetObject builder castToBox

getFCButton :: Builder -> String -> IO FileChooserButton
getFCButton builder = builderGetObject builder castToFileChooserButton

getEntry :: Builder -> String -> IO Entry
getEntry builder = builderGetObject builder castToEntry

getDialog :: Builder -> String -> IO Dialog
getDialog builder = builderGetObject builder castToDialog

getFCDialog :: Builder -> String -> IO FileChooserDialog
getFCDialog builder = builderGetObject builder castToFileChooserDialog