module GUI.EncryptedObjectsActions 
  ( updateEncDataState
  , getEncFileFromDataState
  , buildEncAddButtons
  , buildEncFileBoxes
  , buildEncTrashButtons
  , buildEncFCButtons
  , buildEncArrowButtons
  , onEncAddButtonsClick
  , onEncTrashButtonsClick
  , onEncFCButtonsClick
  , onEncArrowButtonsClick
  , onEncPasswordStartClick
  ) where


import GUI.Utils
import AES128.Decryption

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

onEncAddButtonsClick :: Box -> ButtonsPack -> BoxesPack -> IO () -- common
onEncAddButtonsClick
  encTable
  ButtonsPack { but1 = but1, but2 = but2, but3 = but3, but4 = but4, but5 = but5 }
  BoxesPack { box1 = box1, box2 = box2, box3 = box3, box4 = box4, box5 = box5 } = do
  on but1 buttonActivated $ replaceButtonOnBoxInBox encTable but1 box1
  on but2 buttonActivated $ replaceButtonOnBoxInBox encTable but2 box2
  on but3 buttonActivated $ replaceButtonOnBoxInBox encTable but3 box3
  on but4 buttonActivated $ replaceButtonOnBoxInBox encTable but4 box4
  on but5 buttonActivated $ replaceButtonOnBoxInBox encTable but5 box5
  return ()

onEncTrashButtonsClick :: Box -> ButtonsPack -> BoxesPack -> ButtonsPack -> IO () -- common
onEncTrashButtonsClick
  encTable
  ButtonsPack { but1 = trashBut1, but2 = trashBut2, but3 = trashBut3, but4 = trashBut4, but5 = trashBut5 }
  BoxesPack { box1 = box1, box2 = box2, box3 = box3, box4 = box4, box5 = box5 }
  ButtonsPack { but1 = addBut1, but2 = addBut2, but3 = addBut3, but4 = addBut4, but5 = addBut5 } = do
  on trashBut1 buttonActivated $ replaceBoxOnButtonInBox encTable box1 addBut1
  on trashBut2 buttonActivated $ replaceBoxOnButtonInBox encTable box2 addBut2
  on trashBut3 buttonActivated $ replaceBoxOnButtonInBox encTable box3 addBut3
  on trashBut4 buttonActivated $ replaceBoxOnButtonInBox encTable box4 addBut4
  on trashBut5 buttonActivated $ replaceBoxOnButtonInBox encTable box5 addBut5
  return ()

onFCButtonClick :: IORef DataState -> Int -> FileChooserButton -> IO () -- common
onFCButtonClick refState id fcButton = do
  mbFileName <- fileChooserGetFilename fcButton
  state <- readIORef refState
  writeIORef refState $ updateEncDataState state id $ parseFullPath mbFileName

onEncFCButtonsClick :: IORef DataState -> FCButtonsPack -> IO () -- common
onEncFCButtonsClick
  refState
  FCButtonsPack { fcBut1 = fcBut1, fcBut2 = fcBut2, fcBut3 = fcBut3, fcBut4 = fcBut4, fcBut5 = fcBut5 } = do
  on fcBut1 fileChooserButtonFileSet $ onFCButtonClick refState 1 fcBut1
  on fcBut2 fileChooserButtonFileSet $ onFCButtonClick refState 2 fcBut2
  on fcBut3 fileChooserButtonFileSet $ onFCButtonClick refState 3 fcBut3
  on fcBut4 fileChooserButtonFileSet $ onFCButtonClick refState 4 fcBut4
  on fcBut5 fileChooserButtonFileSet $ onFCButtonClick refState 5 fcBut5
  return ()

onArrowButtonClick :: IORef DataState -> IORef CurrentArrow -> Int -> Dialog -> Entry -> IO () -- common
onArrowButtonClick refState refCurrentArrow id dFileSave dFileSaveEntry = do
    writeIORef refCurrentArrow CurrentArrow { position = id, isEncryption = False }
    state <- readIORef refState
    entrySetText dFileSaveEntry $ createFullPath (getEncFileFromDataState state id) -- ++ "." ++ extension
    widgetShowAll dFileSave

onEncArrowButtonsClick :: IORef DataState -> IORef CurrentArrow -> Dialog -> Entry -> ButtonsPack -> IO ()
onEncArrowButtonsClick
  refState
  refCurrentArrow
  dFileSave
  dFileSaveEntry
  ButtonsPack { but1 = arrowBut1, but2 = arrowBut2, but3 = arrowBut3, but4 = arrowBut4, but5 = arrowBut5 } = do
  on arrowBut1 buttonActivated $ onArrowButtonClick refState refCurrentArrow 1 dFileSave dFileSaveEntry
  on arrowBut2 buttonActivated $ onArrowButtonClick refState refCurrentArrow 2 dFileSave dFileSaveEntry
  on arrowBut3 buttonActivated $ onArrowButtonClick refState refCurrentArrow 3 dFileSave dFileSaveEntry
  on arrowBut4 buttonActivated $ onArrowButtonClick refState refCurrentArrow 4 dFileSave dFileSaveEntry
  on arrowBut5 buttonActivated $ onArrowButtonClick refState refCurrentArrow 5 dFileSave dFileSaveEntry
  return ()

passwordStartClick :: IORef DataState -> IORef CurrentArrow
  -> Button -> Box -> ButtonsPack -> BoxesPack -> FCButtonsPack -> IO ()
passwordStartClick refState refCurrentArrow dPasswordStart encTable encAddButtons encFileBoxes encFCButtons = do
  state <- readIORef refState
  currentArrow <- readIORef refCurrentArrow
  encAddButton <- getButtonFromPack encAddButtons $ position currentArrow
  encFileBox <- getBoxFromPack encFileBoxes $ position currentArrow
  encFCButton <- getFCButtonFromPack encFCButtons $ position currentArrow
  if isEncryption currentArrow
    then do
      fileChooserSetFilename encFCButton $ createFullPath $ getEncFileFromDataState state $ position currentArrow
      replaceButtonOnBoxInBox encTable encAddButton encFileBox
    else do
      fileChooserSetFilename encFCButton "(No)"
      writeIORef refState $ updateEncDataState state (position currentArrow) emptyFile
      replaceBoxOnButtonInBox encTable encFileBox encAddButton


onEncPasswordStartClick :: IORef DataState -> IORef CurrentArrow
  -> Button -> Box -> ButtonsPack -> BoxesPack -> FCButtonsPack -> IO ()
onEncPasswordStartClick refState refCurrentArrow dPasswordStart encTable encAddButtons encFileBoxes encFCButtons = do
  on dPasswordStart buttonActivated $
    passwordStartClick refState refCurrentArrow dPasswordStart encTable encAddButtons encFileBoxes encFCButtons
  return ()