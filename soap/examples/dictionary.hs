#!/usr/bin/env stack
{- stack runghc --package soap -}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror -fno-warn-unused-imports #-}

import           Control.Monad               (forM_)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           Network.HTTP.Client         (defaultManagerSettings)
import           Network.SOAP                (ResponseParser (CursorParser),
                                              Transport, invokeWS)
import           Network.SOAP.Transport.HTTP (initTransportWithM, printBody, printRequest)
import           System.Environment          (getArgs)
import           Text.XML.Cursor             (Cursor, content, laxElement, ($/),
                                              (&/), (&//))
import qualified Text.XML.Writer             as W

data DefineResult = DefineResult
    { drWord :: Text
    , drDefs :: [Definition]
    } deriving (Show)

data Definition = Definition
    { dWord :: Text
    , dDict :: Dictionary
    , dDef  :: Text
    } deriving (Show)

data Dictionary = Dictionary
    { diId   :: Text
    , diName :: Text
    } deriving (Show)

main :: IO ()
main = do
    args <- map T.pack <$> getArgs

    -- find WSDL here: http://services.aonaware.com/dictservice/dictservice.asmx?wsdl
    transport <- initTransportWithM
        defaultManagerSettings
        "http://services.aonaware.com/DictService/DictService.asmx"
        pure -- or printRequest
        pure -- or printBody

    forM_ args $ \word -> do
        DefineResult{drWord, drDefs} <- define transport word

        putStrLn $ "Word definitions of " ++ show drWord
        forM_ drDefs $ \Definition{dDict, dDef} -> do
            T.putStrLn $ T.concat ["=== SOURCE: ", diName dDict, " ==="]
            T.putStrLn dDef

define :: Transport -> Text -> IO DefineResult
define transport word = invokeWS transport url () body (CursorParser parser)
   where
    url = "http://services.aonaware.com/webservices/Define"

    body =
        W.elementA "Define" [("xmlns", "http://services.aonaware.com/webservices/")] $
            W.element "word" word

    parser cur = DefineResult (textOf res "Word") defs
      where
        res = head $ cur $/ laxElement "DefineResponse" &/ laxElement "DefineResult"
        defs = res $/ laxElement "Definitions" &/ def
        def c =
            [ Definition
                (textOf c "Word")
                (Dictionary (textOf dict "id") (textOf dict "Name"))
                (textOf c "WordDefinition")
            ]
          where
            dict = head $ c $/ laxElement "Dictionary"

textOf :: Cursor -> Text -> Text
textOf c t = T.concat $ c $/ laxElement t &// content
