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
import GUI.Global
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
  -> Button -> Entry -> Dialog -> Dialog -> Entry -> Entry -> Label -> IO ()
onFileSaveNextButtonClick refState refCurrentArrow
  dFileSaveNext dFileSaveEntry dFileSave dPassword dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel = do
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
        ResponseDeleteEvent -> passwordCancelClick dPassword dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel
        ResponseNone -> passwordCancelClick dPassword dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel
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

passwordCancelClick :: Dialog -> Entry -> Entry -> Label -> IO ()
passwordCancelClick dPassword dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel = do
  entrySetText dPasswordInputEntry ""
  entrySetText dPasswordRepeatEntry ""
  labelSetText dPasswordLabel ""
  widgetHide dPassword

onPasswordCancelClick :: Button -> Dialog -> Entry -> Entry -> Label -> IO ()
onPasswordCancelClick dPasswordCancel dPassword dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel = do
  on dPasswordCancel buttonActivated $ passwordCancelClick dPassword dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel
  return ()

compareEntriesTexts :: Entry -> Entry -> IO ComparisonAnswer
compareEntriesTexts entry1 entry2 = do
  text1::String <- entryGetText entry1
  text2::String <- entryGetText entry2
  if text1 /= text2
    then return NotEqual
  else if text1 == ""
    then return NotEntered
    else return Equal

passwordEntryReleased :: Entry -> Entry -> Label -> IO ()
passwordEntryReleased dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel = do
  answer <- compareEntriesTexts dPasswordInputEntry dPasswordRepeatEntry
  case answer of
    NotEntered -> labelSetText dPasswordLabel "Password is empty"
    NotEqual -> labelSetText dPasswordLabel "Passwords doesn't match"
    Equal -> labelSetText dPasswordLabel "Passwords match"

onPasswordEntriesReleased :: Entry -> Entry -> Label -> IO ()
onPasswordEntriesReleased dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel = do
  on dPasswordInputEntry keyReleaseEvent $ tryEvent $ liftIO $
    passwordEntryReleased dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel
  on dPasswordRepeatEntry keyReleaseEvent $ tryEvent $ liftIO $
    passwordEntryReleased dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel
  return ()

passwordStartClick :: IORef DataState -> IORef CurrentArrow -> Button -> Entry -> Entry -> Dialog 
  -> IO () -> IO () -> IO () -> IO () -> IO ()
passwordStartClick refState refCurrentArrow dPasswordStart dPasswordInputEntry dPasswordRepeatEntry dPassword 
  preDecPassFoo preEncPassFoo postDecPassFoo postEncPassFoo = do
    answer <- compareEntriesTexts dPasswordInputEntry dPasswordRepeatEntry
    case answer of
      NotEntered -> return ()
      NotEqual -> return ()
      Equal -> do
        preDecPassFoo
        preEncPassFoo
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
                     (do postDecPassFoo
                         postEncPassFoo
                     )
        widgetHide dPassword

onPasswordStartClick :: IORef DataState -> IORef CurrentArrow -> Button -> Entry -> Entry -> Dialog -> EmptiesPack
  -> Box -> ButtonsPack -> BoxesPack -> FCButtonsPack
  -> Box -> ButtonsPack -> BoxesPack -> FCButtonsPack -> IO ()
onPasswordStartClick
  refState refCurrentArrow dPasswordStart dPasswordInputEntry dPasswordRepeatEntry dPassword emptiesPack
  decTable decAddButtonsPack decFileBoxesPack decFCButtonsPack
  encTable encAddButtonsPack encFileBoxesPack encFCButtonsPack = do
    on dPasswordStart buttonActivated $ do
      currentArrow <- readIORef refCurrentArrow
      let isEnc = isEncryption currentArrow
      let pos = position currentArrow
      passwordStartClick refState refCurrentArrow dPasswordStart dPasswordInputEntry dPasswordRepeatEntry dPassword
        (onDecPasswordStartClick isEnc pos decTable decAddButtonsPack decFileBoxesPack emptiesPack)
        (onEncPasswordStartClick isEnc pos encTable encAddButtonsPack encFileBoxesPack emptiesPack)
        (postGUIAsync $ onDecAfterCrypto refState isEnc pos decTable decAddButtonsPack decFileBoxesPack decFCButtonsPack emptiesPack)
        (postGUIAsync $ onEncAfterCrypto refState isEnc pos encTable encAddButtonsPack encFileBoxesPack encFCButtonsPack emptiesPack)
    return ()
    
onMessageOkClick :: Dialog -> Button -> IO ()
onMessageOkClick dMessage dMessageOk = do
  on dMessageOk buttonActivated $ widgetHide dMessage
  return () 