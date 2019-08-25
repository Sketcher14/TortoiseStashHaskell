module GUI.MainWindow
  ( mainWindow
  ) where

import Data.IORef
import Control.Monad.Trans
import Graphics.UI.Gtk

import GUI.Utils
import GUI.DecryptedObjectsActions
import GUI.EncryptedObjectsActions
import GUI.IntermediateWindowsActions


mainWindow :: IO()
mainWindow = do
  initGUI
  state <- newIORef startDataState

  builder <- builderNew
  builderAddFromFile builder "assets/glade/windows.glade"

  window <- builderGetObject builder castToWindow "main_window"

  decTable <- getBox builder "main_window_box_table_decrypt"
  encTable <- getBox builder "main_window_box_table_encrypt"

  dFileSave <- getDialog builder "file_save"
  dFileSaveCancel <- getButton builder "file_save_buttons_cancel"
  dFileSaveNext <- getButton builder "file_save_buttons_next"
  dFileSaveBrowse <- getButton builder "file_save_box_chooser"
  dFileSaveEntry <- getEntry builder "file_save_box_entry"


  dFileChooser <- getFCDialog builder "file_chooser"
  dFileChooserCancel <- getButton builder "file_chooser_box_buttons_cancel"
  dFileChooserChoose <- getButton builder "file_chooser_box_buttons_choose"


  dPassword <- getDialog builder "password"
  dPasswordCancel <- getButton builder "password_box_buttons_cancel"
  dPasswordStart <- getButton builder "password_box_buttons_start"
  dPasswordInputEntry <- getEntry builder "password_box_input_entry"
  dPasswordRepeatEntry <- getEntry builder "password_box_repeat_entry"
  
  
  
  -- TODO need update for entry (look example in trello)
  onFileSaveBrowseButtonClick dFileSaveBrowse dFileChooser
  onFileSaveCancelButtonClick dFileSaveCancel dFileSave
  onFileSaveNextButtonClick dFileSaveNext dFileSaveEntry dPassword


  decBuilder <- builderNew
  builderAddFromFile decBuilder "assets/glade/decrypted_boxes.glade"

  decAddButtonsPack <- buildDecAddButtons builder

  decFileBoxesPack <- buildDecFileBoxes decBuilder

  decTrashButtonsPack <- buildDecTrashButtons decBuilder

  decFCButtonsPack <- buildDecFCButtons decBuilder

  decArrowButtonsPack <- buildDecArrowButtons decBuilder

  onDecAddButtonsClick decTable decAddButtonsPack decFileBoxesPack
  
  onDecTrashButtonsClick decTable decTrashButtonsPack decFileBoxesPack decAddButtonsPack

  onDecFCButtonsClick state decFCButtonsPack

  onDecArrowButtonsClick state dFileSave dFileSaveEntry decArrowButtonsPack


  encBuilder <- builderNew
  builderAddFromFile encBuilder "assets/glade/encrypted_boxes.glade"
  
  encAddButtonsPack <- buildEncAddButtons builder
  
  encFileBoxesPack <- buildEncFileBoxes encBuilder
  
  encTrashButtonsPack <- buildEncTrashButtons encBuilder
  
  encFCButtonsPack <- buildEncFCButtons encBuilder
  
  encArrowButtonsPack <- buildEncArrowButtons encBuilder
  

  widgetShowAll window

  on window deleteEvent $ liftIO mainQuit >> return False

  mainGUI
