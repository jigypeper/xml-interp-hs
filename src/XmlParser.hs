{-# LANGUAGE OverloadedStrings #-}
module XmlParser where

import qualified Data.Text as T
import qualified Text.XML as XML
import qualified Data.Map.Strict as Map
import Control.Monad (forM)
import Control.Exception (try, catch, SomeException)
import System.FilePath ((</>))
import Text.XML.Cursor
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import qualified Database as DB
import XmlTypes
import Data.List (minimum)
import Database.SQLite.Simple (Connection)


getElementName :: Cursor -> T.Text
getElementName cursor = case node cursor of
    XML.NodeElement element -> XML.nameLocalName $ XML.elementName element
    _ -> ""

getElementContent :: Cursor -> T.Text
getElementContent cursor = T.concat $ cursor $// content

parseDueDiligence :: XML.Document -> Map.Map T.Text T.Text
parseDueDiligence doc = 
    let cursor = fromDocument doc
        elements = cursor $// element "ILOGICEPLAN" &/ anyElement
        ddParams = [ "ETOCONVPS_01", "ETOCONVDD_01", "ETOCONVDI_01"
                  , "ETOCONVAF_01", "ETOCONVBF_01", "ETOCONVPC_01"
                  , "ETOCONVHF_01", "ETOCONVCS_01", "ETOCONVBO_01"
                  ]
        
        extractDD cursor = do
            let name = getElementName cursor
                value = getElementContent cursor
            if name `elem` ddParams && value /= "None"
                then Just (name, value)
                else Nothing
                
    in Map.fromList $ catMaybes $ map extractDD elements

parseProductHeights :: XML.Document -> [T.Text]
parseProductHeights doc =
    let cursor = fromDocument doc
        elements = cursor $// element "ILOGICEPLAN" &/ anyElement
        heightParams = ["ETOPRDH_01", "ETOPRDH_02", "ETOPRDH_03", "ETOPRDH_04"]
        
        extractHeight cursor = do
            let name = getElementName cursor
                value = getElementContent cursor
            if name `elem` heightParams && value /= "None"
                then Just value
                else Nothing
                
    in catMaybes $ map extractHeight elements

parseOrderDict :: XML.Document -> DueDiligenceOptions -> Map.Map T.Text T.Text
parseOrderDict doc opts =
    let cursor = fromDocument doc
        elements = cursor $// element "ILOGICEPLAN" &/ anyElement
        
        extractOrder cursor = do
            let name = getElementName cursor
                value = getElementContent cursor
            if name `elem` orderParams opts && value /= "None"
                then Just (name, value)
                else Nothing
                
    in Map.fromList $ catMaybes $ map extractOrder elements

parseTableType :: XML.Document -> T.Text
parseTableType doc =
    let cursor = fromDocument doc
        elements = cursor $// element "ILOGICEPLAN" &/ anyElement
        
        findTable cursor = 
            let name = getElementName cursor
                value = getElementContent cursor
            in if name `elem` ["ETOCONVY_01", "TYPE_01"]
                then Just value
                else Nothing
                
    in fromMaybe "DEFAULT" $ listToMaybe $ catMaybes $ map findTable elements

parseXmlFile :: FilePath -> IO (Either String XmlInterp)
parseXmlFile path = do
    result <- try $ XML.readFile XML.def path
    case result of
        Left err -> return $ Left $ "Error reading XML: " ++ show (err :: SomeException)
        Right doc -> return $ Right $ XmlInterp 
            { xmlRoot = doc
            , dueDiligence = parseDueDiligence doc
            , productHeights = parseProductHeights doc
            , orderDict = parseOrderDict doc defaultDDOptions
            , tableType = parseTableType doc
            }

generateMechanicalXml :: XmlInterp -> FilePath -> IO ()
generateMechanicalXml interp outputPath = do
    let doc = createMechanicalXml interp
    XML.writeFile XML.def outputPath doc

createMechanicalXml :: XmlInterp -> XML.Document
createMechanicalXml interp = 
    let root = XML.Element "ILOGICEPLAN" Map.empty []
        addElement name value = XML.Element (XML.Name name Nothing Nothing) Map.empty [XML.NodeContent value]
        
        mechanicalElements = Map.toList (dueDiligence interp) ++
                           Map.toList (orderDict interp)
        
        nodes = map (\(name, value) -> XML.NodeElement $ addElement name value) mechanicalElements
        docRoot = root { XML.elementNodes = nodes }
        
    in XML.Document (XML.Prologue [] Nothing []) docRoot []

generateElectricalXml :: XmlInterp -> Connection -> FilePath -> IO ()
generateElectricalXml interp conn outputPath = do
    doc <- createElectricalXml interp conn
    XML.writeFile XML.def outputPath doc

createElectricalXml :: XmlInterp -> Connection -> IO XML.Document
createElectricalXml interp conn = do
    let root = XML.Element (XML.Name "Configuration" Nothing Nothing) (Map.singleton "typical" "ILOGICEPLAN") []
        config = XML.Element (XML.Name "ConfigurationVariables" Nothing Nothing) Map.empty []
        
    elements <- processElectricalElements interp conn
    
    let configWithElements = config { XML.elementNodes = elements }
        rootWithConfig = root { XML.elementNodes = [XML.NodeElement configWithElements] }
        
    return $ XML.Document (XML.Prologue [] Nothing []) rootWithConfig []

processElectricalElements :: XmlInterp -> Connection -> IO [XML.Node]
processElectricalElements interp conn = do
    let params = Map.toList (dueDiligence interp) ++
                Map.toList (orderDict interp)
    
    nodes <- forM params $ \(name, value) -> do
        isElec <- DB.checkElectricalParam conn name
        if isElec
            then do
                eplanValue <- DB.queryDb conn (tableType interp) name value
                return $ Just $ XML.NodeElement $ 
                    XML.Element (XML.Name "ConfigurationVariable" Nothing Nothing)
                              (Map.singleton "name" name)
                              [XML.NodeContent $ fromMaybe "Not Required" eplanValue]
            else return Nothing
            
    -- Process product heights specially
    let minHeight = minimum $ map (read . T.unpack) (productHeights interp)
        heightValue = if minHeight <= 4 
                     then "Very low product height"
                     else if minHeight <= 10
                          then "Low product height"
                          else "Standard product height"
                          
        heightNode = XML.NodeElement $ 
            XML.Element (XML.Name "ConfigurationVariable" Nothing Nothing)
                      (Map.singleton "name" "ETOPRDH_01")
                      [XML.NodeContent heightValue]
                      
    return $ heightNode : catMaybes nodes
