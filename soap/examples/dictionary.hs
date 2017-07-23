#!/usr/bin/env stack
{- stack --install-ghc --resolver lts-8.23 runghc --package soap -}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

data Response = Response {
    rWord :: Text,
    rDefs :: [Definition]
} deriving (Show)

data Definition = Definition {
    dWord :: Text,
    dDict :: Dictionary,
    dDef  :: Text
} deriving (Show)

data Dictionary = Dictionary {
    diId   :: Text,
    diName :: Text
} deriving (Show)

body :: Text -> W.XML
body word = W.elementA "Define" [("xmlns", "http://services.aonaware.com/webservices/")]
        $ W.element "word" (word :: Text)

textOf :: Cursor -> Text -> Text
textOf c t = T.concat $ c $/ laxElement t &// content

def :: Cursor -> [Definition]
def c = let
        dict :: Cursor = head $ c $/ laxElement "Dictionary"
    in
        [Definition (textOf c "Word")
                    (Dictionary (textOf dict "id") (textOf dict "Name"))
                    (textOf c "WordDefinition")]

parser :: Cursor -> Response
parser c = let
        res = head (c $/ laxElement "DefineResponse" &/ laxElement "DefineResult")
        defs = res $/ laxElement "Definitions" &/ def
    in
        Response (textOf res "Word") defs

main :: IO ()
main = do
    args <- map T.pack <$> getArgs

    -- find WSDL here: http://services.aonaware.com/dictservice/dictservice.asmx?wsdl

    transport <- initTransportWithM defaultManagerSettings
                 "http://services.aonaware.com/DictService/DictService.asmx"
                 return return
                 -- instead of "return return" you can use "printRequest printBody"

    forM_ args $ \word -> do
        r <- invokeWS transport "http://services.aonaware.com/webservices/Define"
                () (body word) (CursorParser parser)

        putStrLn $ "Word definitions of " ++ show (rWord r)
        forM_ (rDefs r) $ \d -> do
            T.putStrLn $ T.concat ["=== SOURCE: ", diName $ dDict d, " ==="]
            T.putStrLn $ dDef d


