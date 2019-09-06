{-# LANGUAGE ScopedTypeVariables #-}
module GUI.IntermediateWindowsActions
  ( onFileSaveCancelButtonClick
  , onFileSaveBrowseButtonClick
  , onFileSaveNextButtonClick
  , onFileChooserCancelClick
  , onFileChooserApplyClick
  , onPasswordCancelClick
  , onPasswordEntriesReleased
  , onPasswordStartClick
  , onMessageOkClick
  ) where

import GUI.Utils
import GUI.EncryptedObjectsActions
import GUI.DecryptedObjectsActions
import AES128.Encryption
import AES128.Decryption

import Graphics.UI.Gtk
import Data.IORef
import Control.Monad.Trans
import Data.Maybe
import Control.Concurrent                 (forkIO, forkFinally)

onFileSaveBrowseButtonClick :: Button -> FileChooserDialog -> IO ()
onFileSaveBrowseButtonClick dFileSaveBrowse dFileChooser = do
  on dFileSaveBrowse buttonActivated $ do
    answer <- dialogRun dFileChooser
    case answer of
      ResponseDeleteEvent -> widgetHide dFileChooser
      ResponseNone -> widgetHide dFileChooser
      _ -> print $ "dFileChooser other " ++ show answer
  return ()

onFileSaveCancelButtonClick :: Button -> Dialog -> IO ()
onFileSaveCancelButtonClick dFileSaveCancel dFileSave = do
  on dFileSaveCancel buttonActivated $ widgetHide dFileSave
  return ()

onFileSaveNextButtonClick :: IORef DataState -> IORef CurrentArrow
  -> Button -> Entry -> Dialog -> Dialog -> Entry -> Entry -> IO ()
onFileSaveNextButtonClick refState refCurrentArrow
  dFileSaveNext dFileSaveEntry dFileSave dPassword dPasswordInputEntry dPasswordRepeatEntry = do
    on dFileSaveNext buttonActivated $ do 
      fullPath::String <- entryGetText dFileSaveEntry
      state <- readIORef refState
      currentArrow <- readIORef refCurrentArrow
      if isEncryption currentArrow
        then writeIORef refState $ updateEncDataState state (position currentArrow) $ parseFullPath (Just fullPath)
        else writeIORef refState $ updateDecDataState state (position currentArrow) $ parseFullPath (Just fullPath)
      widgetHide dFileSave
      answer <- dialogRun dPassword
      case answer of
        ResponseDeleteEvent -> passwordCancelClick dPassword dPasswordInputEntry dPasswordRepeatEntry
        ResponseNone -> passwordCancelClick dPassword dPasswordInputEntry dPasswordRepeatEntry
        _ -> print $ "dPassword other " ++ show answer
    return ()

onFileChooserCancelClick :: Button -> FileChooserDialog -> IO ()
onFileChooserCancelClick dFileChooserCancel dFileChooser = do
  on dFileChooserCancel buttonActivated $ widgetHide dFileChooser
  return ()

fileChooserApplyClick :: IORef CurrentArrow -> FileChooserDialog -> Entry -> IO ()
fileChooserApplyClick refCurrentArrow dFileChooser dFileSaveEntry = do
  mbFileName <- fileChooserGetFilename dFileChooser
  currentArrow <- readIORef refCurrentArrow
  case mbFileName of
    Nothing -> print "No file is selected, or the selected file can't be represented with a local filename"
    Just fileName -> if isEncryption currentArrow
                       then entrySetText dFileSaveEntry $ fileName ++ "." ++ extension
                       else entrySetText dFileSaveEntry $ removeExtension fileName
  widgetHide dFileChooser

onFileChooserApplyClick :: IORef CurrentArrow -> Button -> FileChooserDialog -> Entry -> IO ()
onFileChooserApplyClick refCurrentArrow dFileChooserApply dFileChooser dFileSaveEntry = do
  on dFileChooserApply buttonActivated $ fileChooserApplyClick refCurrentArrow dFileChooser dFileSaveEntry
  return ()

passwordCancelClick :: Dialog -> Entry -> Entry -> IO ()
passwordCancelClick dPassword dPasswordInputEntry dPasswordRepeatEntry = do
  entrySetText dPasswordInputEntry ""
  entrySetText dPasswordRepeatEntry ""
  widgetHide dPassword

onPasswordCancelClick :: Button -> Dialog -> Entry -> Entry -> IO ()
onPasswordCancelClick dPasswordCancel dPassword dPasswordInputEntry dPasswordRepeatEntry = do
  on dPasswordCancel buttonActivated $ passwordCancelClick dPassword dPasswordInputEntry dPasswordRepeatEntry
  return ()

compareEntriesTexts :: Entry -> Entry -> IO Bool
compareEntriesTexts entry1 entry2 = do
  text1::String <- entryGetText entry1
  text2::String <- entryGetText entry2
  return (text1 == text2)

passwordEntryReleased :: Entry -> Entry -> Label -> IO ()
passwordEntryReleased dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel = do
  bEqual <- compareEntriesTexts dPasswordInputEntry dPasswordRepeatEntry
  if bEqual
    then labelSetText dPasswordLabel "Passwords match"
    else labelSetText dPasswordLabel "Passwords doesn't match"

onPasswordEntriesReleased :: Entry -> Entry -> Label -> IO ()
onPasswordEntriesReleased dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel = do
  on dPasswordInputEntry keyReleaseEvent $ tryEvent $ liftIO $
    passwordEntryReleased dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel
  on dPasswordRepeatEntry keyReleaseEvent $ tryEvent $ liftIO $
    passwordEntryReleased dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel
  return ()

passwordStartClick :: IORef DataState -> IORef CurrentArrow -> Button -> Entry -> Entry -> Dialog -> IO () -> IO () -> IO ()
passwordStartClick refState refCurrentArrow dPasswordStart dPasswordInputEntry dPasswordRepeatEntry dPassword foo1 foo2 = do
  bEqual <- compareEntriesTexts dPasswordInputEntry dPasswordRepeatEntry
  if not bEqual
    then return ()
    else do
      state <- readIORef refState
      currentArrow <- readIORef refCurrentArrow
      let decFullPath = createFullPath $ getDecFileFromDataState state $ position currentArrow
      let encFullPath = createFullPath $ getEncFileFromDataState state $ position currentArrow
      password::String <- entryGetText dPasswordInputEntry
      entrySetText dPasswordInputEntry ""
      entrySetText dPasswordRepeatEntry ""
      createThread (if isEncryption currentArrow
                      then readEncryptWrite decFullPath encFullPath password
                      else readDecryptWrite encFullPath decFullPath password
                   )
                   (do
                      foo1
                      foo2
                   )
      widgetHide dPassword

createThread :: IO () -> IO () -> IO ()
createThread action finalAction = do
  forkFinally action (const finalAction)
  return ()

onPasswordStartClick :: IORef DataState -> IORef CurrentArrow -> Button -> Entry -> Entry -> Dialog -> EmptiesPack
  -> Box -> ButtonsPack -> BoxesPack -> FCButtonsPack
  -> Box -> ButtonsPack -> BoxesPack -> FCButtonsPack -> IO ()
onPasswordStartClick
  refState refCurrentArrow dPasswordStart dPasswordInputEntry dPasswordRepeatEntry dPassword emptiesPack
  decTable decAddButtonsPack decFileBoxesPack decFCButtonsPack
  encTable encAddButtonsPack encFileBoxesPack encFCButtonsPack = do
    on dPasswordStart buttonActivated $ do
      onDecPasswordStartClick refCurrentArrow decTable decAddButtonsPack decFCButtonsPack emptiesPack
      onEncPasswordStartClick refCurrentArrow encTable encAddButtonsPack encFCButtonsPack emptiesPack
      passwordStartClick refState refCurrentArrow dPasswordStart dPasswordInputEntry dPasswordRepeatEntry dPassword
        (onDecAfterCrypto refState refCurrentArrow decTable decAddButtonsPack decFileBoxesPack decFCButtonsPack emptiesPack)
        (onEncAfterCrypto refState refCurrentArrow encTable encAddButtonsPack encFileBoxesPack encFCButtonsPack emptiesPack)
    return ()
    
onMessageOkClick :: Dialog -> Button -> IO ()
onMessageOkClick dMessage dMessageOk = do
  on dMessageOk buttonActivated $ widgetHide dMessage
  return () 