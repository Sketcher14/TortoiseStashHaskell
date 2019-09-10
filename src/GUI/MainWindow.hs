{-# LANGUAGE ScopedTypeVariables #-}
module GUI.MainWindow
  ( mainWindow
  ) where

import Data.IORef
import Control.Monad.Trans
import Graphics.UI.Gtk

import GUI.Utils
import GUI.Global
import GUI.DecryptedObjectsActions
import GUI.EncryptedObjectsActions
import GUI.IntermediateWindowsActions

mainWindow :: IO()
mainWindow = do
  initGUI

  state <- newIORef startDataState
  currentArrow <- newIORef CurrentArrow { position = 0, isEncryption = False }

  builder <- builderNew
  builderAddFromFile builder "assets/glade/windows.glade"

  window <- builderGetObject builder castToWindow "main_window"

  decTable <- getBox builder "main_window_box_table_decrypt"
  encTable <- getBox builder "main_window_box_table_encrypt"

  emptiesPack <- buildEmptiesPack builder

  appFileFilter <- createAppExtensionFileFilter
  noFilter <- createNoFilterFileFilter

  dFileSave <- getDialog builder "file_save"
  dFileSaveCancel <- getButton builder "file_save_buttons_cancel"
  dFileSaveNext <- getButton builder "file_save_buttons_next"
  dFileSaveBrowse <- getButton builder "file_save_box_chooser"
  dFileSaveEntry <- getEntry builder "file_save_box_entry"

  dFileChooser <- getFCDialog builder "file_chooser"
  fileChooserAddFilter dFileChooser noFilter
  fileChooserAddFilter dFileChooser appFileFilter
  dFileChooserCancel <- getButton builder "file_chooser_box_buttons_cancel"
  dFileChooserApply <- getButton builder "file_chooser_box_buttons_apply"

  dPassword <- getDialog builder "password"
  dPasswordCancel <- getButton builder "password_box_buttons_cancel"
  dPasswordStart <- getButton builder "password_box_buttons_start"
  dPasswordInputEntry <- getEntry builder "password_box_input_entry"
  dPasswordRepeatEntry <- getEntry builder "password_box_repeat_entry"
  dPasswordLabel <- getLabel builder "password_box_buttons_label"

  dMessage <- getDialog builder "message"
  dMessageName <- getLabel builder "message_name"
  dMessageOk <- getButton builder "message_ok"

  decBuilder <- builderNew
  builderAddFromFile decBuilder "assets/glade/decrypted_boxes.glade"

  decAddButtonsPack <- buildDecAddButtons builder
  decFileBoxesPack <- buildDecFileBoxes decBuilder
  decTrashButtonsPack <- buildDecTrashButtons decBuilder
  decFCButtonsPack <- buildDecFCButtons decBuilder
  addFileFilterToFCPack noFilter decFCButtonsPack
  addFileFilterToFCPack appFileFilter decFCButtonsPack
  decArrowButtonsPack <- buildDecArrowButtons decBuilder


  onDecAddButtonsClick decTable decAddButtonsPack decFileBoxesPack
  onDecTrashButtonsClick state decTable decTrashButtonsPack decFileBoxesPack decAddButtonsPack decFCButtonsPack
  onDecFCButtonsClick state decFCButtonsPack
  onDecArrowButtonsClick state currentArrow dFileSave dFileSaveEntry dFileChooser noFilter dMessage dMessageName decArrowButtonsPack


  encBuilder <- builderNew
  builderAddFromFile encBuilder "assets/glade/encrypted_boxes.glade"
  
  encAddButtonsPack <- buildEncAddButtons builder
  encFileBoxesPack <- buildEncFileBoxes encBuilder
  encTrashButtonsPack <- buildEncTrashButtons encBuilder
  encFCButtonsPack <- buildEncFCButtons encBuilder
  encArrowButtonsPack <- buildEncArrowButtons encBuilder
  
  onEncAddButtonsClick encTable encAddButtonsPack encFileBoxesPack
  onEncTrashButtonsClick state encTable encTrashButtonsPack encFileBoxesPack encAddButtonsPack encFCButtonsPack
  onEncFCButtonsClick state encFCButtonsPack
  addFileFilterToFCPack appFileFilter encFCButtonsPack
  addFileFilterToFCPack noFilter encFCButtonsPack
  onEncArrowButtonsClick state currentArrow dFileSave dFileSaveEntry dFileChooser appFileFilter dMessage dMessageName encArrowButtonsPack

  onFileSaveCancelButtonClick dFileSaveCancel dFileSave
  onFileSaveBrowseButtonClick dFileSaveBrowse dFileChooser
  onFileSaveNextButtonClick state currentArrow dFileSaveNext dFileSaveEntry dFileSave dPassword dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel

  onFileChooserCancelClick dFileChooserCancel dFileChooser
  onFileChooserApplyClick currentArrow dFileChooserApply dFileChooser dFileSaveEntry

  onPasswordCancelClick dPasswordCancel dPassword dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel
  onPasswordEntriesReleased dPasswordInputEntry dPasswordRepeatEntry dPasswordLabel
  onPasswordStartClick
    state currentArrow dPasswordStart dPasswordInputEntry dPasswordRepeatEntry dPassword emptiesPack
    decTable decAddButtonsPack decFileBoxesPack decFCButtonsPack
    encTable encAddButtonsPack encFileBoxesPack encFCButtonsPack

  onMessageOkClick dMessage dMessageOk

  widgetShowAll window
  on window deleteEvent $ liftIO mainQuit >> return False

  mainGUI