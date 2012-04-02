{-# Language OverloadedStrings #-}
module Main where

import Acme.Serve
import Acme.Types
import Data.ByteString.Char8 (pack,length)
import Prelude hiding (length)

main :: IO ()
main = serve 8000 echo

echo :: Request -> IO Response
echo r =
    do let body = pack $ show r -- note: this only works correctly for ascii, but I did not want to add a depends on utf8-string
       return $ ByteStringResponse
                  { rsCode    = 200
                  , rsHeaders = [ ("Content-Length", pack (show (length body)))
                                , ("Content-Type"  , "text/plain")
                                ]
                  , rsBody    = body
                  }
