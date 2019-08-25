{-# LANGUAGE ScopedTypeVariables #-}
module GUI.IntermediateWindowsActions
  ( onFileSaveBrowseButtonClick
  , onFileSaveCancelButtonClick
  , onFileSaveNextButtonClick
  ) where


import Graphics.UI.Gtk

onFileSaveBrowseButtonClick :: Button -> FileChooserDialog -> IO ()
onFileSaveBrowseButtonClick dFileSaveBrowse dFileChooser = do
  on dFileSaveBrowse buttonActivated $ widgetShowAll dFileChooser
  return ()

onFileSaveCancelButtonClick :: Button -> Dialog -> IO ()
onFileSaveCancelButtonClick dFileSaveCancel dFileSave = do
  on dFileSaveCancel buttonActivated $ widgetHide dFileSave
  return ()

onFileSaveNextButtonClick :: Button -> Entry -> Dialog -> IO ()
onFileSaveNextButtonClick dFileSaveNext dFileSaveEntry dPassword = do
  fullPath <- get dFileSaveEntry entryText
  on dFileSaveNext buttonActivated $ putStrLn $ "ahaah" ++ fullPath
  return ()
