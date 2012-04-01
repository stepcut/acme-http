{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Acme.Request where

import Control.Monad.Trans             (lift, liftIO)
import Control.Exception.Extensible
import           Data.ByteString       (ByteString, elemIndex, empty, split, uncons)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Unsafe          (unsafeDrop, unsafeIndex, unsafeTake)
import Data.Monoid                     (mappend)
import Data.Typeable                   (Typeable)
import Acme.Types                      ( ConnectionClosed(..), HTTPVersion(..), Method(..)
                                       , Request(..), cr, colon, nl, space
                                       )

------------------------------------------------------------------------------
-- Parse Exception
------------------------------------------------------------------------------

data ParseError
    = Unexpected
    | MalformedRequestLine ByteString
    | MalformedHeader      ByteString
    | UnknownHTTPVersion   ByteString
      deriving (Typeable, Show, Eq)

instance Exception ParseError

------------------------------------------------------------------------------
-- Request Parser
------------------------------------------------------------------------------


{-
        Request       = Request-Line              ; Section 5.1
                        *(( general-header        ; Section 4.5
                         | request-header         ; Section 5.3
                         | entity-header ) CRLF)  ; Section 7.1
                        CRLF
                        [ message-body ]          ; Section 4.3
-}
parseRequest :: IO ByteString -> ByteString -> Bool -> IO (Request, ByteString)
parseRequest getChunk bs secure =
    do (line, bs')     <- takeLine getChunk bs
       let (method, requestURI, httpVersion) = parseRequestLine line
       (headers, bs'') <- parseHeaders getChunk bs'
       let request = Request { rqMethod      = method
                             , rqURIbs       = requestURI
                             , rqHTTPVersion = httpVersion
                             , rqHeaders     = headers
                             , rqSecure      = secure
                             , rqBody        = empty
                             }
--       liftIO $ print request
       return (request, bs'')

{-
The Request-Line begins with a method token, followed by the Request-URI and the protocol version, and ending with CRLF. The elements are separated by SP characters. No CR or LF is allowed except in the final CRLF sequence.

        Request-Line   = Method SP Request-URI SP HTTP-Version CRLF
-}
parseRequestLine :: ByteString -> (Method, ByteString, HTTPVersion)
parseRequestLine bs =
    case split space bs of
      [method, requestURI, httpVersion] ->
          (parseMethod method, requestURI, parseHTTPVersion httpVersion)
      _ -> throw (MalformedRequestLine bs)


{-

The Method token indicates the method to be performed on the resource identified by the Request-URI. The method is case-sensitive.

       Method         = "OPTIONS"                ; Section 9.2
                      | "GET"                    ; Section 9.3
                      | "HEAD"                   ; Section 9.4
                      | "POST"                   ; Section 9.5
                      | "PUT"                    ; Section 9.6
                      | "DELETE"                 ; Section 9.7
                      | "TRACE"                  ; Section 9.8
                      | "CONNECT"                ; Section 9.9
                      | extension-method
       extension-method = token
-}
parseMethod :: ByteString -> Method
parseMethod bs
    | bs == "OPTIONS" = OPTIONS
    | bs == "GET"     = GET
    | bs == "HEAD"    = HEAD
    | bs == "POST"    = POST
    | bs == "PUT"     = PUT
    | bs == "DELETE"  = DELETE
    | bs == "TRACE"   = TRACE
    | bs == "CONNECT" = CONNECT
    | otherwise       = EXTENSION bs


--         HTTP-Version   = "HTTP" "/" 1*DIGIT "." 1*DIGIT
parseHTTPVersion :: ByteString -> HTTPVersion
parseHTTPVersion bs
    | bs == "HTTP/1.1" = HTTP11
    | bs == "HTTP/1.0" = HTTP10
    | otherwise        = throw (UnknownHTTPVersion bs)

parseHeaders :: IO ByteString -> ByteString -> IO ([(ByteString, ByteString)], ByteString)
parseHeaders getChunk remainder =
    do (line, bs) <- takeLine getChunk remainder
       if B.null line
          then do return ([], bs)
          else do (headers, bs') <- parseHeaders getChunk bs
                  return ((parseHeader line : headers),  bs')


{-
       message-header = field-name ":" [ field-value ]
       field-name     = token
       field-value    = *( field-content | LWS )
       field-content  = <the OCTETs making up the field-value
                        and consisting of either *TEXT or combinations
                        of token, separators, and quoted-string>
-}
parseHeader :: ByteString -> (ByteString, ByteString)
parseHeader bs =
    let (fieldName, remaining) = parseToken bs
    in case uncons remaining of
         (Just (c, fieldValue))
             | c == colon -> (fieldName, fieldValue)
         _                -> throw (MalformedHeader bs)

{-
       token          = 1*<any CHAR except CTLs or separators>
       separators     = "(" | ")" | "<" | ">" | "@"
                      | "," | ";" | ":" | "\" | <">
                      | "/" | "[" | "]" | "?" | "="
                      | "{" | "}" | SP | HT
       CTL            = <any US-ASCII control character
                        (octets 0 - 31) and DEL (127)>
-}
-- FIXME: follow the spec
parseToken :: ByteString -> (ByteString, ByteString)
parseToken bs = B.span (/= colon) bs

-- | find a line terminated by a '\r\n'
takeLine :: IO ByteString -> ByteString -> IO (ByteString, ByteString)
takeLine getChunk bs =
    -- find the index of the next '\n'
    case elemIndex nl bs of
      Nothing ->
           do x <- getChunk
              if (B.null x)
                 then throw ConnectionClosed
                 else takeLine getChunk (bs `mappend` x)
      (Just 0) -> throw Unexpected
      (Just i) ->
          -- check that the '\n' was preceded by '\r'
          if unsafeIndex bs (i - 1) /= cr
             then throw Unexpected
             else return $ (unsafeTake (i - 1) bs, unsafeDrop (i + 1) bs)
