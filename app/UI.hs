{-# LANGUAGE OverloadedStrings #-}
module UI where

import qualified Graphics.UI.Gtk as Gtk
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import System.FilePath ((</>))
import XmlParser (parseXmlFile, generateMechanicalXml, generateElectricalXml)
import qualified Database as DB
import Database.SQLite.Simple (open, close)
import XmlTypes
import System.Directory (doesFileExist)
import Data.Text (Text)
import qualified Data.Text as T

data UIState = UIState
    { xmlPath :: IORef FilePath
    , dbPath :: IORef FilePath
    }

initUI :: IO ()
initUI = do
    Gtk.initGUI
    
    window <- Gtk.windowNew
    Gtk.set window [ Gtk.windowTitle Gtk.:= (T.pack "XML Interpreter" :: Text)
                   , Gtk.containerBorderWidth Gtk.:= 10
                   ]
    
    vbox <- Gtk.vBoxNew False 5
    Gtk.containerAdd window vbox
    
    xmlButton <- Gtk.buttonNewWithLabel (T.pack "Select XML File" :: Text)
    dbButton <- Gtk.buttonNewWithLabel (T.pack "Select Database" :: Text)
    processButton <- Gtk.buttonNewWithLabel (T.pack "Process XML" :: Text)
    
    Gtk.boxPackStart vbox xmlButton Gtk.PackNatural 0
    Gtk.boxPackStart vbox dbButton Gtk.PackNatural 0
    Gtk.boxPackStart vbox processButton Gtk.PackNatural 0
    
    state <- UIState <$> newIORef "" <*> newIORef ""
    
    Gtk.on xmlButton Gtk.buttonActivated $ selectXmlFile window state
    Gtk.on dbButton Gtk.buttonActivated $ selectDbFile window state
    Gtk.on processButton Gtk.buttonActivated $ processXml state
    
    Gtk.on window Gtk.deleteEvent $ liftIO Gtk.mainQuit >> return False
    
    Gtk.widgetShowAll window
    Gtk.mainGUI

selectXmlFile :: Gtk.Window -> UIState -> IO ()
selectXmlFile window state = do
    dialog <- Gtk.fileChooserDialogNew
        (Just (T.pack "Open XML file" :: Text))
        (Just window)
        Gtk.FileChooserActionOpen
        [(T.pack "Cancel" :: Text, Gtk.ResponseCancel), (T.pack "Open" :: Text, Gtk.ResponseAccept)]
    
    filter <- Gtk.fileFilterNew
    Gtk.fileFilterAddPattern filter (T.pack "*.xml" :: Text)
    Gtk.fileFilterSetName filter (T.pack "XML files" :: Text)
    Gtk.fileChooserAddFilter dialog filter
    
    response <- Gtk.dialogRun dialog
    case response of
        Gtk.ResponseAccept -> do
            mPath <- Gtk.fileChooserGetFilename dialog
            case mPath of
                Just path -> liftIO $ writeIORef (xmlPath state) path
                Nothing -> return ()
        _ -> return ()
    
    Gtk.widgetDestroy dialog

selectDbFile :: Gtk.Window -> UIState -> IO ()
selectDbFile window state = do
    dialog <- Gtk.fileChooserDialogNew
        (Just (T.pack "Open Database file" :: Text))
        (Just window)
        Gtk.FileChooserActionOpen
        [(T.pack "Cancel" :: Text, Gtk.ResponseCancel), (T.pack "Open" :: Text, Gtk.ResponseAccept)]
    
    filter <- Gtk.fileFilterNew
    Gtk.fileFilterAddPattern filter (T.pack "*.db" :: Text)
    Gtk.fileFilterSetName filter (T.pack "Database files" :: Text)
    Gtk.fileChooserAddFilter dialog filter
    
    response <- Gtk.dialogRun dialog
    case response of
        Gtk.ResponseAccept -> do
            mPath <- Gtk.fileChooserGetFilename dialog
            case mPath of
                Just path -> liftIO $ writeIORef (dbPath state) path
                Nothing -> return ()
        _ -> return ()
    
    Gtk.widgetDestroy dialog

processXml :: UIState -> IO ()
processXml state = do
    xmlPath' <- readIORef (xmlPath state)
    dbPath' <- readIORef (dbPath state)
    
    when (not (null xmlPath') && not (null dbPath')) $ do
        xmlExists <- doesFileExist xmlPath'
        dbExists <- doesFileExist dbPath'
        
        if xmlExists && dbExists
            then do
                result <- parseXmlFile xmlPath'
                case result of
                    Left err -> putStrLn $ "Error: " ++ err
                    Right xmlInterp -> do
                        conn <- open dbPath'
                        generateMechanicalXml xmlInterp (xmlPath' <> ".mech.xml")
                        generateElectricalXml xmlInterp conn (xmlPath' <> ".elec.xml")
                        close conn
                        putStrLn "Processing complete"
            else putStrLn "XML or database file not found"
