{-# LANGUAGE OverloadedStrings #-}
module XmlTypes where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Text.XML as XML

data XmlInterp = XmlInterp
    { xmlRoot :: XML.Document
    , dueDiligence :: Map.Map T.Text T.Text
    , productHeights :: [T.Text]
    , orderDict :: Map.Map T.Text T.Text
    , tableType :: T.Text
    } deriving Show

data DueDiligenceOptions = DueDiligenceOptions
    { ddIndicatorOptions :: Map.Map T.Text T.Text
    , orderParams :: [T.Text]
    } deriving Show

defaultDDOptions :: DueDiligenceOptions
defaultDDOptions = DueDiligenceOptions
    { ddIndicatorOptions = Map.fromList
        [ ("AA", "BSAA")
        , ("BL", "BSBL")
        , ("BLA", "BSBA")
        ]
    , orderParams = ["ETOSPID_01", "ETOSO_01", "ETOSOL_01"]
    }
