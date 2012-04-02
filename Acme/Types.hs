{-# LANGUAGE DeriveDataTypeable, RankNTypes, RecordWildCards #-}
module Acme.Types where

import Control.Exception.Extensible    (Exception)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Internal        (c2w)
import Data.Data                       (Data, Typeable)
import Text.PrettyPrint.HughesPJ       (Doc, ($$), (<+>), ($+$), (<>), char, nest, text, vcat)
import Data.Word                       (Word8)

------------------------------------------------------------------------------
-- HTTPVersion
------------------------------------------------------------------------------

data HTTPVersion
    = HTTP10
    | HTTP11
      deriving (Eq, Ord, Read, Show, Data, Typeable)

ppHTTPVersion :: HTTPVersion -> Doc
ppHTTPVersion HTTP10 = text "HTTP/1.0"
ppHTTPVersion HTTP11 = text "HTTP/1.1"

------------------------------------------------------------------------------
-- Method
------------------------------------------------------------------------------

data Method
    = OPTIONS
    | GET
    | GETONLY
    | HEAD
    | POST
    | PUT
    | DELETE
    | TRACE
    | CONNECT
    | EXTENSION ByteString
      deriving (Eq, Ord, Read, Show, Data, Typeable)

ppMethod :: Method -> Doc
ppMethod OPTIONS         = text "OPTIONS"
ppMethod GET             = text "GET"
ppMethod GETONLY         = text "GETONLY"
ppMethod HEAD            = text "HEAD"
ppMethod POST            = text "POST"
ppMethod PUT             = text "PUT"
ppMethod DELETE          = text "DELETE"
ppMethod TRACE           = text "TRACE"
ppMethod CONNECT         = text "CONNECT"
ppMethod (EXTENSION ext) = text (C.unpack ext)

------------------------------------------------------------------------------
-- Request
------------------------------------------------------------------------------

data Request = Request
    { rqMethod      :: !Method
    , rqURIbs       :: !ByteString
    , rqHTTPVersion :: !HTTPVersion
    , rqHeaders     :: ![(ByteString, ByteString)]
    , rqSecure      :: !Bool
    , rqBody        :: !ByteString
    }
    deriving Typeable

instance Show Request where
    show = show . ppRequest

ppRequest :: Request -> Doc
ppRequest Request{..} =
    text "Request {"  $+$
      nest 2 (
        vcat [ field "  rqMethod"      (ppMethod            rqMethod)
             , field ", rqURIbs"       (bytestring          rqURIbs)
             , field ", rqHTTPVersion" (ppHTTPVersion       rqHTTPVersion)
             , field ", rqHeaders"     (vcat $ map ppHeader rqHeaders)
             , field ", rqSecure"      (text $ show         rqSecure)
             ])        $+$
    text "}"

------------------------------------------------------------------------------
-- Response
------------------------------------------------------------------------------

data Response
    = PongResponse               -- ^ return PONG in the request body
    | ByteStringResponse
      { rsCode    :: !Int
      , rsHeaders :: ![(ByteString, ByteString)]
      , rsBody    :: !ByteString
      }

ppResponse :: Response -> Doc
ppResponse PongResponse = text "PongResponse"
ppResponse ByteStringResponse{..} =
    text "Response {"  $+$
      nest 2 (vcat [ field "rsCode"    (text $ show rsCode)
                   , field "rsHeaders" (text $ show rsCode)
                   , field "rsBody"    (text $ show rsBody)
                   ])  $+$
    text "}"

instance Show Response where
    show = show . ppResponse

------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

-- | thrown when the remote-side closes the connection
data ConnectionClosed
    = ConnectionClosed
      deriving (Typeable, Show)

instance Exception ConnectionClosed

------------------------------------------------------------------------------
-- pretty-print helpers
------------------------------------------------------------------------------

-- | render a 'ByteString' to 'Doc'
bytestring :: ByteString -> Doc
bytestring = text . C.unpack

-- | render, field = value
field :: String -- ^ field name
      -> Doc    -- ^ field value
      -> Doc
field name doc = text name $$ nest 15 (char '=' <+> doc)

-- | pretty-print an HTTP header
ppHeader :: (ByteString, ByteString) -> Doc
ppHeader (fieldName, fieldValue) =
    bytestring fieldName <> char ':' <> bytestring fieldValue


------------------------------------------------------------------------------
-- 'Word8' constants for popular characters
------------------------------------------------------------------------------

colon, cr, nl, space :: Word8
colon = c2w ':'
cr    = c2w '\r'
nl    = c2w '\n'
space = c2w ' '

