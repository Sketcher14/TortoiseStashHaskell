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

onFileSaveBrowseButtonClick :: Button -> FileChooserDialog -> IO ()
onFileSaveBrowseButtonClick dFileSaveBrowse dFileChooser = do
  on dFileSaveBrowse buttonActivated $ widgetShowAll dFileChooser
  return ()

onFileSaveCancelButtonClick :: Button -> Dialog -> IO ()
onFileSaveCancelButtonClick dFileSaveCancel dFileSave = do
  on dFileSaveCancel buttonActivated $ widgetHide dFileSave
  return ()

onFileSaveNextButtonClick :: IORef DataState -> IORef CurrentArrow -> Button -> Entry -> Dialog -> Dialog -> IO ()
onFileSaveNextButtonClick refState refCurrentArrow dFileSaveNext dFileSaveEntry dFileSave dPassword = do
    on dFileSaveNext buttonActivated $ do 
      fullPath::String <- entryGetText dFileSaveEntry
      state <- readIORef refState
      currentArrow <- readIORef refCurrentArrow
      if isEncryption currentArrow
        then writeIORef refState $ updateEncDataState state (position currentArrow) $ parseFullPath (Just fullPath)
        else writeIORef refState $ updateDecDataState state (position currentArrow) $ parseFullPath (Just fullPath)
      widgetHide dFileSave
      widgetShowAll dPassword
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

onPasswordCancelClick :: Button -> Dialog -> IO ()
onPasswordCancelClick dPasswordCancel dPassword = do
  on dPasswordCancel buttonActivated $ widgetHide dPassword
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
    then labelSetText dPasswordLabel "Passwords match"      -- TODO set color
    else labelSetText dPasswordLabel "Passwords doesn't match"

onPasswordEntriesReleased :: Entry -> Entry -> Label -> IO ()
onPasswordEntriesReleased dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel = do
  on dPasswordInputEntry keyReleaseEvent $ tryEvent $ liftIO $
    passwordEntryReleased dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel
  on dPasswordRepeatEntry keyReleaseEvent $ tryEvent $ liftIO $
    passwordEntryReleased dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel
  return ()

passwordStartClick :: IORef DataState -> IORef CurrentArrow -> Button -> Entry -> Entry -> Dialog -> IO ()
passwordStartClick refState refCurrentArrow dPasswordStart dPasswordInputEntry dPasswordRepeatEntry dPassword = do
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
      if isEncryption currentArrow
        then readEncryptWrite decFullPath encFullPath password
        else readDecryptWrite encFullPath decFullPath password
      widgetHide dPassword

onPasswordStartClick :: IORef DataState -> IORef CurrentArrow -> Button -> Entry -> Entry -> Dialog
  -> Box -> ButtonsPack -> BoxesPack -> FCButtonsPack
  -> Box -> ButtonsPack -> BoxesPack -> FCButtonsPack -> IO ()
onPasswordStartClick
  refState refCurrentArrow dPasswordStart dPasswordInputEntry dPasswordRepeatEntry dPassword
  decTable decAddButtonsPack decFileBoxesPack decFCButtonsPack
  encTable encAddButtonsPack encFileBoxesPack encFCButtonsPack = do
    on dPasswordStart buttonActivated $ do
      passwordStartClick refState refCurrentArrow dPasswordStart dPasswordInputEntry dPasswordRepeatEntry dPassword
      onDecPasswordStartClick refState refCurrentArrow decTable decAddButtonsPack decFileBoxesPack decFCButtonsPack
      onEncPasswordStartClick refState refCurrentArrow encTable encAddButtonsPack encFileBoxesPack encFCButtonsPack
    return ()