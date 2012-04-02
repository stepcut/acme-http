module Acme.Serve where

import Acme.Request
import Acme.Response
import Acme.Types
import Control.Concurrent (killThread, forkIO)
import Control.Exception.Extensible             as E
import Control.Monad (forever)
import Control.Monad.Trans
import Data.ByteString                          (ByteString, empty)
import Data.ByteString.Char8                    (pack)
import Network.BSD                              (getProtocolNumber)
import Network.Socket                           (Socket, SockAddr(..), SocketOption(..), SocketType(Stream), Family(AF_INET), accept, bindSocket, iNADDR_ANY, sClose, listen, maxListenQueue, setSocketOption, socket)
import Network.Socket.ByteString                (recv, sendAll)
import System.IO



-- | start TCP listening on a port
listenOn :: Int  -- ^ port number
         -> IO Socket
listenOn portm = do
    proto <- getProtocolNumber "tcp"
    E.bracketOnError
        (socket AF_INET Stream proto)
        (sClose)
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            setSocketOption sock NoDelay 1
            bindSocket sock (SockAddrInet (fromIntegral portm) iNADDR_ANY)
            listen sock (max 1024 maxListenQueue)
            return sock
        )

-- | listen on a port and handle 'Requests'
serve ::  Int                                -- ^ port to listen on
      -> (Request -> IO Response)  -- ^ request handler
      -> IO ()
serve port app =
    bracket (listenOn port) sClose $ \listenSocket ->
        serveSocket listenSocket app

-- | handle 'Requests' from an already listening 'Socket'
serveSocket :: Socket                             -- ^ 'Socket' in listen mode
            -> (Request -> IO Response) -- ^ request handler
            -> IO ()
serveSocket listenSocket app =
    forever $
        do (sock, addr) <- accept listenSocket
           let reader = recv sock 4096
               writer = sendAll sock
           forkIO $ do requestLoop False addr reader writer app `E.catch` (\ConnectionClosed -> return ())
                       sClose sock

requestLoop :: Bool
            -> SockAddr
            -> IO ByteString
            -> (ByteString -> IO ())
            -> (Request -> IO Response)
            -> IO ()
requestLoop secure addr reader writer app =
    go empty
    where
      go bs =
          do (request, bs') <- parseRequest reader bs secure
             sendResponse writer =<< app request
             go bs'
