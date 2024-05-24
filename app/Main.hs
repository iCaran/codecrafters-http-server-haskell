{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad             (forever)

import qualified Data.ByteString.Char8     as BC

import           Data.Either               (fromRight)

import           Data.Maybe                (fromJust)

import           Network.Socket

import           Network.Socket.ByteString (recv, sendAll)

import           System.IO                 (BufferMode (..), hSetBuffering,

                                            stdout)

data Method = GET

            | POST

            deriving (Show, Read, Eq)

data Header = Header BC.ByteString BC.ByteString

    deriving (Show, Eq, Read)

data HttpRequest = HttpRequest { method  :: Method

                               , path    :: BC.ByteString

                               , headers :: [Header]

                               } deriving (Show, Read)

breakLines :: BC.ByteString -> [BC.ByteString]

breakLines bs = let (l, rs) = BC.breakSubstring "\r\n" bs

                    rest = BC.drop 2 rs

                 in l : if BC.null rest then [] else breakLines rest

parseMethod :: BC.ByteString -> Either String Method

parseMethod "GET"  = Right GET

parseMethod "POST" = Right POST

parseMethod other  = Left $ "Unknown method: " <> BC.unpack other

parseReq :: BC.ByteString -> Either String HttpRequest

parseReq rawReq = let lines = breakLines rawReq

                      first = head lines

                      (m : p : _) = BC.split ' ' first

                   in ((\method -> HttpRequest method p []) <$> parseMethod m)

handleReq :: Socket -> HttpRequest -> IO () -- TODO reader?

handleReq socket (HttpRequest GET path _) =

    case path of

    --   "/" -> sendAll socket resp200

    --   _   -> sendAll socket resp404

      "/"                            -> sendAll socket resp200

      p | "/echo/" `BC.isPrefixOf` p -> sendAll socket (echoResponse p)

      _                              -> sendAll socket resp404

  where

    resp404 = "HTTP/1.1 404 Not Found\r\n\r\n"

    resp200 = "HTTP/1.1 200 OK\r\n\r\n"

    echoResponse :: BC.ByteString -> BC.ByteString

    echoResponse echoPath = let body = fromJust $ BC.stripPrefix "/echo/" echoPath

                                contentLen = BC.length body

                                respLines  = ["HTTP/1.1 200 OK", "Content-Type: text/plain", "Content-Length: " <> BC.pack (show contentLen)] :: [BC.ByteString]

                             in BC.intercalate "\r\n" respLines <> "\r\n\r\n" <> body

main :: IO ()

main = do

    hSetBuffering stdout LineBuffering

    -- Uncomment this block to pass first stage

    let host = "127.0.0.1"

        port = "4221"

    BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port

    -- Get address information for the given host and port

    addrInfo <- getAddrInfo Nothing (Just host) (Just port)

    serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol

    setSocketOption serverSocket ReuseAddr 1

    bind serverSocket $ addrAddress $ head addrInfo

    listen serverSocket 5

    -- Accept connections and handle them forever

    forever $ do

        (clientSocket, clientAddr) <- accept serverSocket

        BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."

        -- Handle the clientSocket as needed...

        rawReq <- recv clientSocket 4096

        let req = parseReq rawReq

        case req of

          Right r  -> handleReq clientSocket r

          Left err -> fail err

        close clientSocket