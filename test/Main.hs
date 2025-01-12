{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Map.Strict as Map
import qualified Text.XML as XML
import Text.XML.Cursor
import Database.SQLite.Simple
import System.Directory (removeFile, doesFileExist)
import Control.Exception (bracket)
import Control.Monad ((>=>))
import qualified Database as DB
import XmlParser
import XmlTypes

-- Helper function to create a test database
setupTestDb :: IO Connection
setupTestDb = do
    conn <- open ":memory:"
    execute_ conn "CREATE TABLE olcparam (xml_name TEXT, m_e TEXT)"
    execute_ conn "CREATE TABLE test_table (xml_param_name TEXT, xml_value TEXT, eplan_value TEXT)"
    
    execute conn "INSERT INTO olcparam (xml_name, m_e) VALUES (?, ?)" 
           ("ETOCONVPS_01" :: T.Text, "m" :: T.Text)
    execute conn "INSERT INTO olcparam (xml_name, m_e) VALUES (?, ?)" 
           ("ETOPRDH_01" :: T.Text, "e" :: T.Text)
    
    execute conn "INSERT INTO test_table (xml_param_name, xml_value, eplan_value) VALUES (?, ?, ?)"
           ("ETOPRDH_01" :: T.Text, "5" :: T.Text, "Low product height" :: T.Text)
    
    return conn

-- Sample XML content for testing
sampleXml :: TL.Text
sampleXml = TL.fromStrict $ T.unlines
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<ILOGICEPLAN>"
    , "  <ETOCONVPS_01>PS_Value</ETOCONVPS_01>"
    , "  <ETOPRDH_01>5</ETOPRDH_01>"
    , "  <ETOPRDH_02>10</ETOPRDH_02>"
    , "  <ETOCONVY_01>test_table</ETOCONVY_01>"
    , "  <ETOSPID_01>Order1</ETOSPID_01>"
    , "</ILOGICEPLAN>"
    ]

-- Helper function to parse XML safely
parseTestXml :: TL.Text -> IO XML.Document
parseTestXml input = do
    case XML.parseText XML.def input of
        Left err -> fail $ "Failed to parse XML: " ++ show err
        Right doc -> return doc

main :: IO ()
main = hspec $ do
    describe "Database Operations" $ do
        it "should correctly identify mechanical parameters" $ do
            bracket setupTestDb close $ \conn -> do
                result <- DB.checkMechanicalParam conn "ETOCONVPS_01"
                result `shouldBe` True

        it "should correctly identify electrical parameters" $ do
            bracket setupTestDb close $ \conn -> do
                result <- DB.checkElectricalParam conn "ETOPRDH_01"
                result `shouldBe` True

        it "should return correct DB query results" $ do
            bracket setupTestDb close $ \conn -> do
                result <- DB.queryDb conn "test_table" "ETOPRDH_01" "5"
                result `shouldBe` Just "Low product height"

    describe "XML Parsing" $ do
        it "should parse due diligence parameters correctly" $ do
            doc <- parseTestXml sampleXml
            let ddMap = parseDueDiligence doc
            Map.lookup "ETOCONVPS_01" ddMap `shouldBe` Just "PS_Value"

        it "should parse product heights correctly" $ do
            doc <- parseTestXml sampleXml
            let heights = parseProductHeights doc
            heights `shouldBe` ["5", "10"]

        it "should parse table type correctly" $ do
            doc <- parseTestXml sampleXml
            let tabletype = parseTableType doc
            tabletype `shouldBe` "test_table"

        it "should parse order dictionary correctly" $ do
            doc <- parseTestXml sampleXml
            let orderDict' = parseOrderDict doc defaultDDOptions
            Map.lookup "ETOSPID_01" orderDict' `shouldBe` Just "Order1"

    describe "XML Generation" $ do
        it "should generate mechanical XML correctly" $ do
            doc <- parseTestXml sampleXml
            let xmlInterp = XmlInterp 
                    { xmlRoot = doc
                    , dueDiligence = parseDueDiligence doc
                    , productHeights = parseProductHeights doc
                    , orderDict = parseOrderDict doc defaultDDOptions
                    , tableType = parseTableType doc
                    }
            
            let mechDoc = createMechanicalXml xmlInterp
            let cursor = fromDocument mechDoc
            let content' = cursor $// element "ETOCONVPS_01" &/ content
            content' `shouldBe` ["PS_Value"]

        it "should generate electrical XML correctly" $ do
            bracket setupTestDb close $ \conn -> do
                doc <- parseTestXml sampleXml
                let xmlInterp = XmlInterp 
                        { xmlRoot = doc
                        , dueDiligence = parseDueDiligence doc
                        , productHeights = parseProductHeights doc
                        , orderDict = parseOrderDict doc defaultDDOptions
                        , tableType = parseTableType doc
                        }
                
                elecDoc <- createElectricalXml xmlInterp conn
                let cursor = fromDocument elecDoc
                let configVars = cursor $// element "ConfigurationVariable"
                length configVars `shouldBe` 1
                
                let heightVar = head configVars
                let name = attribute "name" heightVar
                name `shouldBe` ["ETOPRDH_01"]
                let content' = heightVar $/ content
                content' `shouldBe` ["Low product height"]

    describe "Full Integration" $ do
        it "should process XML file end-to-end" $ do
            bracket setupTestDb close $ \conn -> do
                writeFile "test.xml" (TL.unpack sampleXml)
                
                result <- parseXmlFile "test.xml"
                case result of
                    Left err -> fail err
                    Right xmlInterp -> do
                        generateMechanicalXml xmlInterp "test.mech.xml"
                        mechExists <- doesFileExist "test.mech.xml"
                        mechExists `shouldBe` True
                        
                        generateElectricalXml xmlInterp conn "test.elec.xml"
                        elecExists <- doesFileExist "test.elec.xml"
                        elecExists `shouldBe` True
                        
                        removeFile "test.xml"
                        removeFile "test.mech.xml"
                        removeFile "test.elec.xml"
