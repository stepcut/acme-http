module Main where

import Acme.Serve
import Acme.Types

main :: IO ()
main = serve 8000 pong

pong :: Request -> IO Response
pong r =
    return $ PongResponse
