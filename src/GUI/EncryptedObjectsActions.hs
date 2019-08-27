module GUI.EncryptedObjectsActions 
  ( updateEncDataState
  , getEncFileFromDataState
  , buildEncAddButtons
  , buildEncFileBoxes
  , buildEncTrashButtons
  , buildEncFCButtons
  , buildEncArrowButtons 
  ) where


import GUI.Utils

import Graphics.UI.Gtk
import Data.IORef


updateEncDataState :: DataState -> Int -> File -> DataState
updateEncDataState state id file = state { enc = filesPack }
  where
    encFilesPack = enc state
    filesPack = case id of
                  1 -> encFilesPack { file1 = file }
                  2 -> encFilesPack { file2 = file }
                  3 -> encFilesPack { file3 = file }
                  4 -> encFilesPack { file4 = file }
                  5 -> encFilesPack { file5 = file }


getEncFileFromDataState :: DataState -> Int -> File
getEncFileFromDataState state 1 = file1 $ enc state
getEncFileFromDataState state 2 = file2 $ enc state
getEncFileFromDataState state 3 = file3 $ enc state
getEncFileFromDataState state 4 = file4 $ enc state
getEncFileFromDataState state 5 = file5 $ enc state


buildEncAddButtons :: Builder -> IO ButtonsPack
buildEncAddButtons builder = do
  encAddButton1 <- getButton builder "add_encrypt1"
  encAddButton2 <- getButton builder "add_encrypt2"
  encAddButton3 <- getButton builder "add_encrypt3"
  encAddButton4 <- getButton builder "add_encrypt4"
  encAddButton5 <- getButton builder "add_encrypt5" 
  return ButtonsPack { but1 = encAddButton1
                     , but2 = encAddButton2
                     , but3 = encAddButton3
                     , but4 = encAddButton4
                     , but5 = encAddButton5
                     }
                     
buildEncFileBoxes :: Builder -> IO BoxesPack
buildEncFileBoxes builder = do
  encBox1 <- getBox builder "encrypted_box1"
  encBox2 <- getBox builder "encrypted_box2"
  encBox3 <- getBox builder "encrypted_box3"
  encBox4 <- getBox builder "encrypted_box4"
  encBox5 <- getBox builder "encrypted_box5"
  return BoxesPack { box1 = encBox1
                   , box2 = encBox2
                   , box3 = encBox3
                   , box4 = encBox4
                   , box5 = encBox5
                   }

buildEncTrashButtons :: Builder -> IO ButtonsPack
buildEncTrashButtons builder = do
  encTrashButton1 <- getButton builder "encrypted_box_trash1"
  encTrashButton2 <- getButton builder "encrypted_box_trash2"
  encTrashButton3 <- getButton builder "encrypted_box_trash3"
  encTrashButton4 <- getButton builder "encrypted_box_trash4"
  encTrashButton5 <- getButton builder "encrypted_box_trash5"
  return ButtonsPack { but1 = encTrashButton1
                     , but2 = encTrashButton2
                     , but3 = encTrashButton3
                     , but4 = encTrashButton4
                     , but5 = encTrashButton5
                     }
                     
buildEncFCButtons :: Builder -> IO FCButtonsPack
buildEncFCButtons builder = do
  encFCButton1 <- getFCButton builder "encrypted_box_chooser1"
  encFCButton2 <- getFCButton builder "encrypted_box_chooser2"
  encFCButton3 <- getFCButton builder "encrypted_box_chooser3"
  encFCButton4 <- getFCButton builder "encrypted_box_chooser4"
  encFCButton5 <- getFCButton builder "encrypted_box_chooser5"
  return FCButtonsPack { fcBut1 = encFCButton1
                       , fcBut2 = encFCButton2
                       , fcBut3 = encFCButton3
                       , fcBut4 = encFCButton4
                       , fcBut5 = encFCButton5
                       }

buildEncArrowButtons :: Builder -> IO ButtonsPack
buildEncArrowButtons builder = do
  encArrowButton1 <- getButton builder "encrypted_box_arrow1"
  encArrowButton2 <- getButton builder "encrypted_box_arrow2"
  encArrowButton3 <- getButton builder "encrypted_box_arrow3"
  encArrowButton4 <- getButton builder "encrypted_box_arrow4"
  encArrowButton5 <- getButton builder "encrypted_box_arrow5"
  return ButtonsPack { but1 = encArrowButton1
                     , but2 = encArrowButton2
                     , but3 = encArrowButton3
                     , but4 = encArrowButton4
                     , but5 = encArrowButton5
                     }


