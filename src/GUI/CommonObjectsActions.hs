module GUI.CommonObjectsActions
  ( updateDataState
  , getFileFromDataState
  , buildButtons
  , buildBoxes
  , buildFCButtons
  , onAddButtonClick
  , onTrashButtonsClick
  , onFCButtonClick
  , onArrowButtonsClick
  ) where

import GUI.Utils

import Graphics.UI.Gtk
import Data.IORef
import Data.Maybe

updateDataState :: FilesPack -> Int -> File -> FilesPack
updateDataState filesPack 1 file = filesPack { file1 = file }
updateDataState filesPack 2 file = filesPack { file2 = file }
updateDataState filesPack 3 file = filesPack { file3 = file }
updateDataState filesPack 4 file = filesPack { file4 = file }
updateDataState filesPack 5 file = filesPack { file5 = file }

getFileFromDataState :: FilesPack -> Int -> File
getFileFromDataState filesPack 1 = file1 filesPack
getFileFromDataState filesPack 2 = file2 filesPack
getFileFromDataState filesPack 3 = file3 filesPack
getFileFromDataState filesPack 4 = file4 filesPack
getFileFromDataState filesPack 5 = file5 filesPack

buildButtons :: Builder -> StringsPack -> IO ButtonsPack
buildButtons builder StringsPack { str1 = name1, str2 = name2, str3 = name3, str4 = name4, str5 = name5 } = do
  button1 <- getButton builder name1
  button2 <- getButton builder name2
  button3 <- getButton builder name3
  button4 <- getButton builder name4
  button5 <- getButton builder name5
  return ButtonsPack { but1 = button1
                     , but2 = button2
                     , but3 = button3
                     , but4 = button4
                     , but5 = button5
                     }

buildBoxes :: Builder -> StringsPack -> IO BoxesPack
buildBoxes builder StringsPack { str1 = name1, str2 = name2, str3 = name3, str4 = name4, str5 = name5 } = do
  box1 <- getBox builder name1
  box2 <- getBox builder name2
  box3 <- getBox builder name3
  box4 <- getBox builder name4
  box5 <- getBox builder name5
  return BoxesPack { box1 = box1
                   , box2 = box2
                   , box3 = box3
                   , box4 = box4
                   , box5 = box5
                   }

buildFCButtons :: Builder -> StringsPack -> IO FCButtonsPack
buildFCButtons builder StringsPack { str1 = name1, str2 = name2, str3 = name3, str4 = name4, str5 = name5 } = do
  fcButton1 <- getFCButton builder name1
  fcButton2 <- getFCButton builder name2
  fcButton3 <- getFCButton builder name3
  fcButton4 <- getFCButton builder name4
  fcButton5 <- getFCButton builder name5
  return FCButtonsPack { fcBut1 = fcButton1
                       , fcBut2 = fcButton2
                       , fcBut3 = fcButton3
                       , fcBut4 = fcButton4
                       , fcBut5 = fcButton5
                       }

onAddButtonClick :: Box -> ButtonsPack -> BoxesPack -> IO ()
onAddButtonClick
  table
  ButtonsPack { but1 = but1, but2 = but2, but3 = but3, but4 = but4, but5 = but5 }
  BoxesPack { box1 = box1, box2 = box2, box3 = box3, box4 = box4, box5 = box5 } = do
    on but1 buttonActivated $ replaceInBox table (castToWidget but1) (castToWidget box1)
    on but2 buttonActivated $ replaceInBox table (castToWidget but2) (castToWidget box2)
    on but3 buttonActivated $ replaceInBox table (castToWidget but3) (castToWidget box3)
    on but4 buttonActivated $ replaceInBox table (castToWidget but4) (castToWidget box4)
    on but5 buttonActivated $ replaceInBox table (castToWidget but5) (castToWidget box5)
    return ()

trashButtonClick :: IORef DataState -> Int -> (DataState -> Int -> File -> DataState)
  -> Box -> Box -> Button -> FileChooserButton -> IO ()
trashButtonClick refState id updateDataStateFun table fileBox addButton fcButton = do
  state <- readIORef refState
  replaceInBox table (castToWidget fileBox) (castToWidget addButton)
  fileChooserSetFilename fcButton "(No)"
  writeIORef refState $ updateDataStateFun state id emptyFile

onTrashButtonsClick :: IORef DataState -> (DataState -> Int -> File -> DataState)
  -> Box -> ButtonsPack -> BoxesPack -> ButtonsPack -> FCButtonsPack -> IO ()
onTrashButtonsClick
  refState
  updateDataStateFun
  table
  ButtonsPack { but1 = trashBut1, but2 = trashBut2, but3 = trashBut3, but4 = trashBut4, but5 = trashBut5 }
  BoxesPack { box1 = fileBox1, box2 = fileBox2, box3 = fileBox3, box4 = fileBox4, box5 = fileBox5 }
  ButtonsPack { but1 = addBut1, but2 = addBut2, but3 = addBut3, but4 = addBut4, but5 = addBut5 }
  FCButtonsPack { fcBut1 = fcBut1, fcBut2 = fcBut2, fcBut3 = fcBut3, fcBut4 = fcBut4, fcBut5 = fcBut5 }= do
    on trashBut1 buttonActivated $ trashButtonClick refState 1 updateDataStateFun table fileBox1 addBut1 fcBut1
    on trashBut2 buttonActivated $ trashButtonClick refState 2 updateDataStateFun table fileBox2 addBut2 fcBut2
    on trashBut3 buttonActivated $ trashButtonClick refState 3 updateDataStateFun table fileBox3 addBut3 fcBut3
    on trashBut4 buttonActivated $ trashButtonClick refState 4 updateDataStateFun table fileBox4 addBut4 fcBut4
    on trashBut5 buttonActivated $ trashButtonClick refState 5 updateDataStateFun table fileBox5 addBut5 fcBut5
    return ()

fcButtonClick :: IORef DataState -> Int -> (DataState -> Int -> File -> DataState) -> FileChooserButton -> IO ()
fcButtonClick refState id updateDataStateFun fcButton = do
  mbFileName <- fileChooserGetFilename fcButton
  case mbFileName of
    Nothing -> print "No file is selected, or the selected file can't be represented with a local filename"
    Just fileName -> do
      state <- readIORef refState
      writeIORef refState $ updateDataStateFun state id $ parseFullPath mbFileName

onFCButtonClick :: IORef DataState -> (DataState -> Int -> File -> DataState) -> FCButtonsPack -> IO ()
onFCButtonClick
  refState
  updateDataStateFun
  FCButtonsPack { fcBut1 = fcBut1, fcBut2 = fcBut2, fcBut3 = fcBut3, fcBut4 = fcBut4, fcBut5 = fcBut5 } = do
    on fcBut1 fileChooserButtonFileSet $ fcButtonClick refState 1 updateDataStateFun fcBut1
    on fcBut2 fileChooserButtonFileSet $ fcButtonClick refState 2 updateDataStateFun fcBut2
    on fcBut3 fileChooserButtonFileSet $ fcButtonClick refState 3 updateDataStateFun fcBut3
    on fcBut4 fileChooserButtonFileSet $ fcButtonClick refState 4 updateDataStateFun fcBut4
    on fcBut5 fileChooserButtonFileSet $ fcButtonClick refState 5 updateDataStateFun fcBut5
    return ()

arrowButtonClick :: IORef DataState -> IORef CurrentArrow -> Int -> (DataState -> Int -> File)
  -> Bool -> Dialog -> Entry -> FileChooserDialog -> FileFilter -> IO ()
arrowButtonClick 
  refState refCurrentArrow id getFileFromDataStateFun isEnc dFileSave dFileSaveEntry dFileChooser fileFilter = do
    writeIORef refCurrentArrow CurrentArrow { position = id, isEncryption = isEnc }
    state <- readIORef refState
    if isEnc
      then entrySetText dFileSaveEntry $ createFullPath (getFileFromDataStateFun state id) ++ "." ++ extension
      else entrySetText dFileSaveEntry $ removeExtension $ createFullPath (getFileFromDataStateFun state id)
    fileChooserSetFilter dFileChooser fileFilter
    widgetShowAll dFileSave

onArrowButtonsClick :: IORef DataState -> IORef CurrentArrow ->
  (DataState -> Int -> File) -> Bool -> Dialog -> Entry -> FileChooserDialog -> FileFilter -> ButtonsPack ->  IO ()
onArrowButtonsClick
  refState
  refCurrentArrow
  getFileFromDataStateFun
  isEncryption
  dFileSave
  dFileSaveEntry
  dFileChooser
  fileFilter
  ButtonsPack { but1 = arrowBut1, but2 = arrowBut2, but3 = arrowBut3, but4 = arrowBut4, but5 = arrowBut5 } = do
    on arrowBut1 buttonActivated $
      arrowButtonClick refState refCurrentArrow 1 getFileFromDataStateFun isEncryption dFileSave dFileSaveEntry dFileChooser fileFilter
    on arrowBut2 buttonActivated $
      arrowButtonClick refState refCurrentArrow 2 getFileFromDataStateFun isEncryption dFileSave dFileSaveEntry dFileChooser fileFilter
    on arrowBut3 buttonActivated $
      arrowButtonClick refState refCurrentArrow 3 getFileFromDataStateFun isEncryption dFileSave dFileSaveEntry dFileChooser fileFilter
    on arrowBut4 buttonActivated $
      arrowButtonClick refState refCurrentArrow 4 getFileFromDataStateFun isEncryption dFileSave dFileSaveEntry dFileChooser fileFilter
    on arrowBut5 buttonActivated $
      arrowButtonClick refState refCurrentArrow 5 getFileFromDataStateFun isEncryption dFileSave dFileSaveEntry dFileChooser fileFilter
    return ()