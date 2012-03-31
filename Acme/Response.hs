{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Acme.Response where

import Data.ByteString       (ByteString, append)
import Data.ByteString.Char8 () -- instance IsString ByteString


------------------------------------------------------------------------------
-- send a response
------------------------------------------------------------------------------

pong :: (ByteString -> IO ()) -> IO ()
pong send =
    do send "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\nPONG"

{-
sendResponse :: Response -> (ByteString -> IO
sendResponse _ =
    do -- lift  $ liftIO $ putStrLn "responding..."
       yield $ statusLine rsCode `append` "Content-Length: 4\r\n\r\nPONG"
       return ()
-}

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


