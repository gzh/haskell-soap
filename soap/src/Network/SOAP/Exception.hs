{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Network.SOAP.Exception
    ( SOAPParsingError(..)
    , SOAPFault(..), extractSoapFault
    ) where

import Control.Exception as E
import Data.Typeable
import Text.XML (Document)
import Text.XML.Cursor
import qualified Data.Text as T


data SOAPParsingError = SOAPParsingError String deriving (Show, Typeable)
instance Exception SOAPParsingError

-- | Exception to be thrown when transport encounters an exception that is
--   acutally a SOAP Fault.
data SOAPFault = SOAPFault { faultCode   :: T.Text
                           , faultString :: T.Text
                           } deriving (Eq, Show, Typeable)

instance Exception SOAPFault

-- | Try to find a SOAP Fault in a document.
extractSoapFault :: Document -> Maybe SOAPFault
extractSoapFault doc =
    case cur' of
        []    -> Nothing
        cur:_ -> Just $ SOAPFault { faultCode   = removeNs $ peek cur code
                                  , faultString = peek cur reason
                                  }
    where
        cur' = fromDocument doc $| laxElement "Envelope"
                                &/ laxElement "Body"
                                &/ laxElement "Fault"
        code = laxElement "Code" &/ laxElement "Subcode" &/ laxElement "Value"
        reason = laxElement "Reason" &/ laxElement "Text"
        peek cur sel = T.concat $! cur $/ sel &/ content
        removeNs = head . reverse . T.split (== ':')
