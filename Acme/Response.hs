{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Acme.Response where

import Acme.Types               (Response(..))
import Data.ByteString       (ByteString, concat, append)
import Data.ByteString.Char8 () -- instance IsString ByteString
import Prelude               hiding (concat)

------------------------------------------------------------------------------
-- send a response
------------------------------------------------------------------------------

pong :: (ByteString -> IO ()) -> IO ()
pong send =
    do send "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 4\r\n\r\nPONG"

sendResponse :: (ByteString -> IO ()) -> Response -> IO ()
sendResponse send PongResponse = pong send
sendResponse send ByteStringResponse{..} =
    send $ concat (statusLine rsCode : (formatHeaders rsHeaders) ++ [rsBody])
    where
      formatHeaders :: [(ByteString, ByteString)] -> [ByteString]
      formatHeaders         [] = ["\r\n"]
      formatHeaders ((f,v):hs) = [ f, ": " , v , "\r\n"] ++ formatHeaders hs

------------------------------------------------------------------------------
-- Status Lines
------------------------------------------------------------------------------

{-
  Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF
-}

-- FIXME: can the http version always be 1.1 or do we need to match the caller?
statusLine :: Int -> ByteString
statusLine 200 = ok_status

ok_status :: ByteString
ok_status = "HTTP/1.1 200 OK\r\n"


