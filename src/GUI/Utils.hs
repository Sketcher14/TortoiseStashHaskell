{-# LANGUAGE ScopedTypeVariables #-}
module GUI.Utils
  ( removeExtension
  , emptyFile
  , startDataState
  , parseFullPath
  , createFullPath
  , replaceInBox
  , createAppExtensionFileFilter
  , createNoFilterFileFilter
  , addFileFilterToFCPack
  , buildEmptiesPack
  , showMessage
  , validateFile
  , createThread
  ) where

import GUI.Global
import Data.List
import Data.Maybe
import System.IO

import Graphics.UI.Gtk
import System.IO.Error
import Control.Exception  (IOException, handle)
import Control.Concurrent (forkFinally)

removeExtension :: String -> String
removeExtension name = newName
  where
    index = last $ elemIndices '.' name
    newName = take index name

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

replaceInBox :: Box -> Widget -> Widget -> IO ()
replaceInBox parent old new = do
  pos <- get parent $ boxChildPosition old
  containerRemove parent old
  containerAdd parent new
  boxReorderChild parent new pos

createAppExtensionFileFilter :: IO FileFilter
createAppExtensionFileFilter = do
  filter <- fileFilterNew
  fileFilterAddPattern filter $ "*." ++ extension
  fileFilterSetName filter appFileFilterName
  return filter

createNoFilterFileFilter :: IO FileFilter
createNoFilterFileFilter = do
  filter <- fileFilterNew
  fileFilterAddPattern filter "*.*"
  fileFilterSetName filter "All files"
  return filter

addFileFilterToFCPack :: FileFilter -> FCButtonsPack -> IO ()
addFileFilterToFCPack
  fileFilter
  FCButtonsPack { fcBut1 = fcBut1, fcBut2 = fcBut2, fcBut3 = fcBut3, fcBut4 = fcBut4, fcBut5 = fcBut5 }= do
    fileChooserAddFilter fcBut1 fileFilter
    fileChooserAddFilter fcBut2 fileFilter
    fileChooserAddFilter fcBut3 fileFilter
    fileChooserAddFilter fcBut4 fileFilter
    fileChooserAddFilter fcBut5 fileFilter

buildEmptiesPack :: Builder -> IO EmptiesPack
buildEmptiesPack builder = do
  empty1 <- getLabel builder "empty1"
  empty2 <- getLabel builder "empty2"
  empty3 <- getLabel builder "empty3"
  empty4 <- getLabel builder "empty4"
  empty5 <- getLabel builder "empty5"
  return EmptiesPack { empty1 = empty1
                     , empty2 = empty2
                     , empty3 = empty3
                     , empty4 = empty4
                     , empty5 = empty5
                     }

showMessage :: Dialog -> Label -> String -> IO ()
showMessage dMessage dMessageName text = do
  labelSetText dMessageName text
  widgetShowAll dMessage

validateFile :: String -> IO (Bool, String)
validateFile filePath = handle (\(e :: IOException) -> print e >> return (False, show e)) $ do
                          h <- openFile filePath ReadMode
                          return (True, "")

createThread :: IO () -> IO () -> IO ()
createThread action finalAction = do
  forkFinally action (const finalAction)
  return ()