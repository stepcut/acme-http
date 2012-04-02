{-# Language OverloadedStrings #-}
module Main where

import Acme.Serve
import Acme.Types
import Data.ByteString.Char8 ()

main :: IO ()
main = serve 8000 pong

pong :: Request -> IO Response
pong r =
    return $ PongResponse

pong2 :: Request -> IO Response
pong2 r =
    return $ ByteStringResponse { rsCode = 200
                                , rsHeaders = [ ("Content-Length", "4")
                                              , ("Content-Type"  , "text/plain")
                                              ]
                                , rsBody    = "PONG"
                                }


