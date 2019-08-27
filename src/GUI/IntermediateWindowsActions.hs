{-# LANGUAGE ScopedTypeVariables #-}
module GUI.IntermediateWindowsActions
  ( onFileSaveBrowseButtonClick
  , onFileSaveCancelButtonClick
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

onFileSaveBrowseButtonClick :: Button -> FileChooserDialog -> IO ()
onFileSaveBrowseButtonClick dFileSaveBrowse dFileChooser = do
  on dFileSaveBrowse buttonActivated $ widgetShowAll dFileChooser
  return ()

onFileSaveCancelButtonClick :: Button -> Dialog -> IO ()
onFileSaveCancelButtonClick dFileSaveCancel dFileSave = do
  on dFileSaveCancel buttonActivated $ widgetHide dFileSave
  return ()

onFileSaveNextButtonClick :: IORef DataState -> IORef Int -> Button -> Entry -> Dialog -> Dialog -> IO ()
onFileSaveNextButtonClick refState position dFileSaveNext dFileSaveEntry dFileSave dPassword = do
    on dFileSaveNext buttonActivated $ do 
      fullPath::String <- entryGetText dFileSaveEntry
      pos <- readIORef position
      state <- readIORef refState
      writeIORef refState $ updateEncDataState state pos $ parseFullPath (Just fullPath)
      widgetHide dFileSave
      widgetShowAll dPassword
    return ()

onFileChooserCancelClick :: Button -> FileChooserDialog -> IO ()
onFileChooserCancelClick dFileChooserCancel dFileChooser = do
  on dFileChooserCancel buttonActivated $ widgetHide dFileChooser
  return ()

fileChooserApplyClick :: FileChooserDialog -> Entry -> IO ()
fileChooserApplyClick dFileChooser dFileSaveEntry = do
  -- TODO error handling
  (Just filepath)::Maybe String <- fileChooserGetFilename dFileChooser
  entrySetText dFileSaveEntry $ filepath ++ "." ++ extension
  widgetHide dFileChooser

onFileChooserApplyClick :: Button -> FileChooserDialog -> Entry -> IO ()
onFileChooserApplyClick dFileChooserApply dFileChooser dFileSaveEntry = do
  on dFileChooserApply buttonActivated $ fileChooserApplyClick dFileChooser dFileSaveEntry
  return ()

onPasswordCancelClick :: Button -> Dialog -> IO ()
onPasswordCancelClick dPasswordCancel dPassword = do
  on dPasswordCancel buttonActivated $ widgetHide dPassword -- reset state????
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


passwordStartClick :: IORef DataState -> IORef Int -> Button -> Entry -> Entry -> Dialog -> IO ()
passwordStartClick refState refPosition dPasswordStart dPasswordInputEntry dPasswordRepeatEntry dPassword = do
  bEqual <- compareEntriesTexts dPasswordInputEntry dPasswordRepeatEntry
  if not bEqual
    then return ()
    else do
      state <- readIORef refState
      position <- readIORef refPosition
      let decFullPath = createFullPath $ getDecFileFromDataState state position
      let encFullPath = createFullPath $ getEncFileFromDataState state position
      password::String <- entryGetText dPasswordInputEntry
      readEncryptWrite decFullPath encFullPath password
      widgetHide dPassword


onPasswordStartClick :: IORef DataState -> IORef Int -> Button -> Entry -> Entry -> Dialog -> IO ()
onPasswordStartClick refState position dPasswordStart dPasswordInputEntry dPasswordRepeatEntry dPassword = do
  on dPasswordStart buttonActivated $
    passwordStartClick refState position dPasswordStart dPasswordInputEntry dPasswordRepeatEntry dPassword
  return ()

