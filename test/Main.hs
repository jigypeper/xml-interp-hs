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
import Control.Monad (void)
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

-- Fixed test XML content with proper structure
sampleXml :: TL.Text
sampleXml = TL.fromStrict $ T.unlines
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<root>"
    , "  <ILOGICEPLAN>"
    , "    <ETOCONVPS_01>PS_Value</ETOCONVPS_01>"
    , "    <ETOPRDH_01>5</ETOPRDH_01>"
    , "    <ETOPRDH_02>10</ETOPRDH_02>"
    , "    <ETOCONVY_01>test_table</ETOCONVY_01>"
    , "    <ETOSPID_01>Order1</ETOSPID_01>"
    , "    <ETOCONVDD_01>DD_Value</ETOCONVDD_01>"  -- Added due diligence parameter
    , "  </ILOGICEPLAN>"
    , "</root>"
    ]

main :: IO ()
main = hspec $ do
    describe "Database Operations" $ do
        it "identifies mechanical parameters correctly" $ do
            bracket setupTestDb close $ \conn -> do
                result <- DB.checkMechanicalParam conn "ETOCONVPS_01"
                result `shouldBe` True
                
                result2 <- DB.checkMechanicalParam conn "NONEXISTENT"
                result2 `shouldBe` False

        it "identifies electrical parameters correctly" $ do
            bracket setupTestDb close $ \conn -> do
                result <- DB.checkElectricalParam conn "ETOPRDH_01"
                result `shouldBe` True
                
                result2 <- DB.checkElectricalParam conn "NONEXISTENT"
                result2 `shouldBe` False

        it "returns correct database query results" $ do
            bracket setupTestDb close $ \conn -> do
                result <- DB.queryDb conn "test_table" "ETOPRDH_01" "5"
                result `shouldBe` Just "Low product height"
                
                result2 <- DB.queryDb conn "test_table" "NONEXISTENT" "5"
                result2 `shouldBe` Just "nope"

    describe "XML Parsing" $ do
        it "parses due diligence parameters correctly" $ do
            case XML.parseText XML.def sampleXml of
                Left err -> expectationFailure $ show err
                Right doc -> do
                    let ddMap = parseDueDiligence doc
                    Map.lookup "ETOCONVPS_01" ddMap `shouldBe` Just "PS_Value"
                    -- Debug print
                    putStrLn $ "DD Map: " ++ show ddMap

        it "parses product heights correctly" $ do
            case XML.parseText XML.def sampleXml of
                Left err -> expectationFailure $ show err
                Right doc -> do
                    let heights = parseProductHeights doc
                    -- Debug print
                    putStrLn $ "Heights found: " ++ show heights
                    heights `shouldBe` ["5", "10"]

    describe "XML Generation" $ do
        it "generates mechanical XML with correct structure" $ do
            case XML.parseText XML.def sampleXml of
                Left err -> expectationFailure $ show err
                Right doc -> do
                    let xmlInterp = XmlInterp 
                            { xmlRoot = doc
                            , dueDiligence = parseDueDiligence doc
                            , productHeights = parseProductHeights doc
                            , orderDict = parseOrderDict doc defaultDDOptions
                            , tableType = parseTableType doc
                            }
                    
                    let mechDoc = createMechanicalXml xmlInterp
                    let cursor = fromDocument mechDoc
                    let ps = (cursor $// element "ETOCONVPS_01" &/ content)
                    -- Debug print
                    putStrLn $ "Generated mechanical XML content: " ++ show ps
                    ps `shouldBe` ["PS_Value"]

        it "generates electrical XML with correct structure" $ do
            bracket setupTestDb close $ \conn -> do
                case XML.parseText XML.def sampleXml of
                    Left err -> expectationFailure $ show err
                    Right doc -> do
                        let xmlInterp = XmlInterp 
                                { xmlRoot = doc
                                , dueDiligence = parseDueDiligence doc
                                , productHeights = ["5", "10"]  -- Explicit heights to avoid empty list
                                , orderDict = parseOrderDict doc defaultDDOptions
                                , tableType = parseTableType doc
                                }
                        
                        elecDoc <- createElectricalXml xmlInterp conn
                        let cursor = fromDocument elecDoc
                        let vars = cursor $// element "ConfigurationVariable"
                        length vars `shouldBe` 1
                        
                        let heightVar = head vars
                        (attribute "name" heightVar) `shouldBe` ["ETOPRDH_01"]
                        (heightVar $/ content) `shouldBe` ["Low product height"]

    describe "Error Handling" $ do
        it "handles invalid XML gracefully" $ do
            let invalidXml = "not valid xml"
            case XML.parseText XML.def (TL.fromStrict invalidXml) of
                Left _ -> return () -- Expected to fail
                Right _ -> expectationFailure "Should have failed on invalid XML"

        it "handles database errors gracefully" $ do
            bracket setupTestDb close $ \conn -> do
                result <- DB.queryDb conn "nonexistent_table" "param" "value"
                result `shouldBe` Nothing
