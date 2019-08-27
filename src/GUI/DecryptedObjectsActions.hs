module GUI.DecryptedObjectsActions
  ( updateDecDataState
  , getDecFileFromDataState
  , buildDecAddButtons
  , buildDecFileBoxes
  , buildDecTrashButtons
  , buildDecFCButtons
  , buildDecArrowButtons
  , onDecAddButtonsClick
  , onDecTrashButtonsClick
  , onDecFCButtonsClick
  , onDecArrowButtonsClick
  ) where

import GUI.Utils

import Graphics.UI.Gtk
import Data.IORef


updateDecDataState :: DataState -> Int -> File -> DataState
updateDecDataState state id file = state { dec = filesPack }
  where
    decFilesPack = dec state
    filesPack = case id of
                  1 -> decFilesPack { file1 = file }
                  2 -> decFilesPack { file2 = file }
                  3 -> decFilesPack { file3 = file }
                  4 -> decFilesPack { file4 = file }
                  5 -> decFilesPack { file5 = file }


getDecFileFromDataState :: DataState -> Int -> File
getDecFileFromDataState state 1 = file1 $ dec state
getDecFileFromDataState state 2 = file2 $ dec state
getDecFileFromDataState state 3 = file3 $ dec state
getDecFileFromDataState state 4 = file4 $ dec state
getDecFileFromDataState state 5 = file5 $ dec state


buildDecAddButtons :: Builder -> IO ButtonsPack
buildDecAddButtons builder = do
  decAddButton1 <- getButton builder "add_decrypt1"
  decAddButton2 <- getButton builder "add_decrypt2"
  decAddButton3 <- getButton builder "add_decrypt3"
  decAddButton4 <- getButton builder "add_decrypt4"
  decAddButton5 <- getButton builder "add_decrypt5"
  return ButtonsPack { but1 = decAddButton1
                     , but2 = decAddButton2
                     , but3 = decAddButton3
                     , but4 = decAddButton4
                     , but5 = decAddButton5
                     }

buildDecFileBoxes :: Builder -> IO BoxesPack
buildDecFileBoxes builder = do
  decBox1 <- getBox builder "decrypted_box1"
  decBox2 <- getBox builder "decrypted_box2"
  decBox3 <- getBox builder "decrypted_box3"
  decBox4 <- getBox builder "decrypted_box4"
  decBox5 <- getBox builder "decrypted_box5"
  return BoxesPack { box1 = decBox1
                   , box2 = decBox2
                   , box3 = decBox3
                   , box4 = decBox4
                   , box5 = decBox5
                   }

buildDecTrashButtons :: Builder -> IO ButtonsPack
buildDecTrashButtons builder = do
  decTrashButton1 <- getButton builder "decrypted_box_trash1"
  decTrashButton2 <- getButton builder "decrypted_box_trash2"
  decTrashButton3 <- getButton builder "decrypted_box_trash3"
  decTrashButton4 <- getButton builder "decrypted_box_trash4"
  decTrashButton5 <- getButton builder "decrypted_box_trash5"
  return ButtonsPack { but1 = decTrashButton1
                     , but2 = decTrashButton2
                     , but3 = decTrashButton3
                     , but4 = decTrashButton4
                     , but5 = decTrashButton5
                     }

buildDecFCButtons :: Builder -> IO FCButtonsPack
buildDecFCButtons builder = do
  decFCButton1 <- getFCButton builder "decrypted_box_chooser1"
  decFCButton2 <- getFCButton builder "decrypted_box_chooser2"
  decFCButton3 <- getFCButton builder "decrypted_box_chooser3"
  decFCButton4 <- getFCButton builder "decrypted_box_chooser4"
  decFCButton5 <- getFCButton builder "decrypted_box_chooser5"
  return FCButtonsPack { fcBut1 = decFCButton1
                       , fcBut2 = decFCButton2
                       , fcBut3 = decFCButton3
                       , fcBut4 = decFCButton4
                       , fcBut5 = decFCButton5
                       }

buildDecArrowButtons :: Builder -> IO ButtonsPack
buildDecArrowButtons builder = do
  decArrowButton1 <- getButton builder "decrypted_box_arrow1"
  decArrowButton2 <- getButton builder "decrypted_box_arrow2"
  decArrowButton3 <- getButton builder "decrypted_box_arrow3"
  decArrowButton4 <- getButton builder "decrypted_box_arrow4"
  decArrowButton5 <- getButton builder "decrypted_box_arrow5"
  return ButtonsPack { but1 = decArrowButton1
                     , but2 = decArrowButton2
                     , but3 = decArrowButton3
                     , but4 = decArrowButton4
                     , but5 = decArrowButton5
                     }

onDecAddButtonsClick :: Box -> ButtonsPack -> BoxesPack -> IO ()
onDecAddButtonsClick
  decTable
  ButtonsPack { but1 = but1, but2 = but2, but3 = but3, but4 = but4, but5 = but5 }
  BoxesPack { box1 = box1, box2 = box2, box3 = box3, box4 = box4, box5 = box5 } = do
  on but1 buttonActivated $ replaceButtonOnBoxInBox decTable but1 box1
  on but2 buttonActivated $ replaceButtonOnBoxInBox decTable but2 box2
  on but3 buttonActivated $ replaceButtonOnBoxInBox decTable but3 box3
  on but4 buttonActivated $ replaceButtonOnBoxInBox decTable but4 box4
  on but5 buttonActivated $ replaceButtonOnBoxInBox decTable but5 box5
  return ()

onDecTrashButtonsClick :: Box -> ButtonsPack -> BoxesPack -> ButtonsPack -> IO ()
onDecTrashButtonsClick
  decTable
  ButtonsPack { but1 = trashBut1, but2 = trashBut2, but3 = trashBut3, but4 = trashBut4, but5 = trashBut5 }
  BoxesPack { box1 = box1, box2 = box2, box3 = box3, box4 = box4, box5 = box5 }
  ButtonsPack { but1 = addBut1, but2 = addBut2, but3 = addBut3, but4 = addBut4, but5 = addBut5 } = do
  on trashBut1 buttonActivated $ replaceBoxOnButtonInBox decTable box1 addBut1
  on trashBut2 buttonActivated $ replaceBoxOnButtonInBox decTable box2 addBut2
  on trashBut3 buttonActivated $ replaceBoxOnButtonInBox decTable box3 addBut3
  on trashBut4 buttonActivated $ replaceBoxOnButtonInBox decTable box4 addBut4
  on trashBut5 buttonActivated $ replaceBoxOnButtonInBox decTable box5 addBut5
  return ()


onFCButtonClick :: IORef DataState -> Int -> FileChooserButton -> IO ()
onFCButtonClick refState id fcButton = do
  -- TODO error handling
  mbFilename <- fileChooserGetFilename fcButton
  state <- readIORef refState
  writeIORef refState $ updateDecDataState state id $ parseFullPath mbFilename

onDecFCButtonsClick :: IORef DataState -> FCButtonsPack -> IO ()
onDecFCButtonsClick
  refState
  FCButtonsPack { fcBut1 = fcBut1, fcBut2 = fcBut2, fcBut3 = fcBut3, fcBut4 = fcBut4, fcBut5 = fcBut5 } = do
  on fcBut1 fileChooserButtonFileSet $ onFCButtonClick refState 1 fcBut1
  on fcBut2 fileChooserButtonFileSet $ onFCButtonClick refState 2 fcBut2
  on fcBut3 fileChooserButtonFileSet $ onFCButtonClick refState 3 fcBut3
  on fcBut4 fileChooserButtonFileSet $ onFCButtonClick refState 4 fcBut4
  on fcBut5 fileChooserButtonFileSet $ onFCButtonClick refState 5 fcBut5
  return ()

onArrowButtonClick :: IORef DataState -> IORef Int -> Int -> Dialog -> Entry -> IO ()
onArrowButtonClick refState position id dFileSave dFileSaveEntry = do
    writeIORef position id
    state <- readIORef refState
    entrySetText dFileSaveEntry $ createFullPath (getDecFileFromDataState state id) ++ "." ++ extension
    widgetShowAll dFileSave

onDecArrowButtonsClick :: IORef DataState -> IORef Int -> Dialog -> Entry -> ButtonsPack -> IO ()
onDecArrowButtonsClick
  refState
  position
  dFileSave
  dFileSaveEntry
  ButtonsPack { but1 = arrowBut1, but2 = arrowBut2, but3 = arrowBut3, but4 = arrowBut4, but5 = arrowBut5 } = do
  -- TODO check empty refState
  on arrowBut1 buttonActivated $ onArrowButtonClick refState position 1 dFileSave dFileSaveEntry
  on arrowBut2 buttonActivated $ onArrowButtonClick refState position 2 dFileSave dFileSaveEntry
  on arrowBut3 buttonActivated $ onArrowButtonClick refState position 3 dFileSave dFileSaveEntry
  on arrowBut4 buttonActivated $ onArrowButtonClick refState position 4 dFileSave dFileSaveEntry
  on arrowBut5 buttonActivated $ onArrowButtonClick refState position 5 dFileSave dFileSaveEntry
  return ()
