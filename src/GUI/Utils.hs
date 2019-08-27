module GUI.Utils
  ( File(..)
  , FilesPack(..)
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
  , getLabel
  , startDataState
  , ButtonsPack(..)
  , BoxesPack(..)
  , FCButtonsPack(..)
  , createFullPath
  , extension
  )
where


import Data.List
import Data.Maybe
import Control.Concurrent (forkIO,  forkOS, threadDelay)
import Control.Monad (forever)

import Graphics.UI.Gtk


data File = File
  { name :: String
  , path :: String
  } deriving (Show)

data FilesPack = FilesPack
  { file1 :: File
  , file2 :: File
  , file3 :: File
  , file4 :: File
  , file5 :: File
  } deriving (Show)

data DataState = DataState
  { dec :: FilesPack
  , enc :: FilesPack
  } deriving (Show)

extension :: String
extension = "ts"

emptyFile :: File
emptyFile = File { name = "", path = "" }

emptyFilesPack :: FilesPack
emptyFilesPack = FilesPack
  { file1 = emptyFile
  , file2 = emptyFile
  , file3 = emptyFile
  , file4 = emptyFile
  , file5 = emptyFile
  }

startDataState :: DataState
startDataState = DataState { dec = emptyFilesPack, enc = emptyFilesPack }

parseFullPath :: Maybe String -> File
parseFullPath Nothing = emptyFile
parseFullPath (Just fullPath) = File { name = newName, path = newPath }
  where
    index = last $ elemIndices '/' fullPath
    newName = drop (index + 1) fullPath
    newPath = take index fullPath


createFullPath :: File -> String
createFullPath File { name = name, path = path } = path ++ "/" ++ name


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

getLabel :: Builder -> String -> IO Label
getLabel builder = builderGetObject builder castToLabel