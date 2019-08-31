module GUI.Utils
  ( File(..)
  , FilesPack(..)
  , DataState(..)
  , CurrentArrow(..)
  , ButtonsPack(..)
  , BoxesPack(..)
  , FCButtonsPack(..)
  , StringsPack(..)
  , extension
  , removeExtension
  , emptyFile
  , startDataState
  , parseFullPath
  , createFullPath
  , replaceInBox
  , getButton
  , getBox
  , getFCButton
  , getEntry
  , getDialog
  , getFCDialog
  , getLabel
  , getButtonFromPack
  , getBoxFromPack
  , getFCButtonFromPack
  )
where


import Data.List
import Data.Maybe
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


data CurrentArrow = CurrentArrow
  { position :: Int
  , isEncryption :: Bool
  } deriving (Show)


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


data StringsPack = StringsPack
  { str1 :: String
  , str2 :: String
  , str3 :: String
  , str4 :: String
  , str5 :: String
  }


extension :: String
extension = "TTS"

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


getButtonFromPack :: ButtonsPack -> Int -> IO Button
getButtonFromPack buttonsPack 1 = return $ but1 buttonsPack
getButtonFromPack buttonsPack 2 = return $ but2 buttonsPack
getButtonFromPack buttonsPack 3 = return $ but3 buttonsPack
getButtonFromPack buttonsPack 4 = return $ but4 buttonsPack
getButtonFromPack buttonsPack 5 = return $ but5 buttonsPack


getBoxFromPack :: BoxesPack -> Int -> IO Box
getBoxFromPack boxesPack 1 = return $ box1 boxesPack
getBoxFromPack boxesPack 2 = return $ box2 boxesPack
getBoxFromPack boxesPack 3 = return $ box3 boxesPack
getBoxFromPack boxesPack 4 = return $ box4 boxesPack
getBoxFromPack boxesPack 5 = return $ box5 boxesPack


getFCButtonFromPack :: FCButtonsPack -> Int -> IO FileChooserButton
getFCButtonFromPack fcButtonsPack 1 = return $ fcBut1 fcButtonsPack
getFCButtonFromPack fcButtonsPack 2 = return $ fcBut2 fcButtonsPack
getFCButtonFromPack fcButtonsPack 3 = return $ fcBut3 fcButtonsPack
getFCButtonFromPack fcButtonsPack 4 = return $ fcBut4 fcButtonsPack
getFCButtonFromPack fcButtonsPack 5 = return $ fcBut5 fcButtonsPack