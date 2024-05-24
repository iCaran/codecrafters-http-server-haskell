{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-# LANGUAGE BlockArguments #-}

{-# LANGUAGE OverloadedRecordDot #-}

module Main (main, createBody) where

import Control.Monad (forever)

import qualified Data.ByteString.Char8 as BC

import Network.Socket as NetSock

    ( getAddrInfo,

      accept,

      bind,

      listen,

      socket,

      close,

      defaultProtocol,

      AddrInfo(addrAddress, addrFamily),

      SocketType(Stream), setSocketOption, SocketOption (ReuseAddr), Socket )

import Network.Socket.ByteString as NBS (send, recv)

-- import System.IO (BufferMode (..), hSetBuffering, stdout, openFile, IOMode (ReadMode), hGetContents)

import System.IO (BufferMode (..), hSetBuffering, stdout, openFile, IOMode (..), hGetContents, hClose)

import qualified System.IO as SIO

import Data.Function ((&))

import Data.Maybe (isJust)

import Control.Concurrent (threadDelay, forkIO)

import qualified Data.List as List

import Control.Exception (catch, AsyncException (UserInterrupt), SomeException (SomeException))

import System.Environment as SE (getArgs)

import Data.ByteString (hPut)

newtype HttpError = HttpError String deriving (Show)

createBody :: [[Char]] -> [Char]

createBody payload =

    let body = (BC.unpack . BC.dropEnd 1 .  BC.pack . concat $ zipWith (++) payload (repeat "/")) in

        ("HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: " ++ show (length body) ++ "\r\n\r\n" ++ body)

createResponse :: HttpMsg -> [(BC.ByteString, BC.ByteString)] -> BC.ByteString -> [Char]

createResponse request headers body =

    ("HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: " ++ show (BC.length body) ++ "\r\n\r\n" ++ BC.unpack body)

data Method =

    GET

    | POST

    deriving (Show)

parseMethod "GET" = GET

parseMethod "POST" = POST

data HttpMsg = HttpMsg {

    -- method :: BC.ByteString

    method :: Method

    , path :: BC.ByteString

    , protocol :: [BC.ByteString]

    -- , headers :: [(BC.ByteString, BC.ByteString)]} deriving (Show)

    , headers :: [(BC.ByteString, BC.ByteString)]

    , body :: BC.ByteString} deriving (Show)

destructHeader :: [BC.ByteString] -> Maybe (BC.ByteString, BC.ByteString)

destructHeader [n, v] = Just (BC.dropEnd 1 n, v)

destructHeader _ = Nothing

parseHeaders :: [BC.ByteString] -> [(BC.ByteString, BC.ByteString)]

parseHeaders = foldr ((\header acc ->

                case header of

                    Just kv -> kv:acc

                    Nothing -> acc

        ) . destructHeader . BC.words) []

-- parseMsg :: BC.ByteString -> HttpMsg

parseMsg :: BC.ByteString -> Either HttpMsg HttpError

parseMsg msg =

    -- let first:headers = map (BC.dropEnd 1) $ BC.lines msg in

    --     let method:path:protocol = BC.split ' ' first in

    --         HttpMsg {method = method, path = path, protocol = protocol, headers = parseHeaders headers}

    

    let msgLines = BC.lines msg in

        case List.elemIndex (BC.pack "\r") msgLines of

            Just bodyStartIdx ->

                let (meta, body) = List.splitAt bodyStartIdx msgLines

                    first:headers = map (BC.dropEnd 1) meta

                    method:path:protocol = BC.split ' ' first in

                        Left HttpMsg {method = parseMethod . BC.unpack $ method, path = path, protocol = protocol, headers = parseHeaders headers, body = body & List.dropWhile (== BC.pack ['\r']) & BC.unlines & BC.dropEnd 1}

            Nothing ->

                Right $ HttpError "Malformed http message"

test :: BC.ByteString

-- test = BC.pack "GET /user-agent HTTP/1.1\r\n\

-- \Host: localhost:4221\r\n\ 

-- \User-Agent: curl/7.64.1\r\n\r\n"

test = BC.pack "POST /files/readme.txt HTTP/1.1\r\nHost: localhost:4221\r\nUser-Agent: curl/8.4.0\r\nAccept: */*\r\nContent-Length: 11\r\nContent-Type: application/x-www-form-urlencoded\r\n\r\nhello world"

-- "GET /user-agent HTTP/1.1\r\n\

-- \Host: localhost:4221\r\n\ 

-- \User-Agent: curl/7.64.1\r\n\r\n"

handleUserAgent request =

    case List.find (\(k,_) -> k == BC.pack "User-Agent") (headers request) of

        Just(_, v) -> createResponse request [] v

        Nothing -> createResponse request [] BC.empty

getDirectoryFromArgs :: [[Char]] -> Maybe [Char]

getDirectoryFromArgs ["--directory", dir] = Just dir

getDirectoryFromArgs _ = Nothing

handleGetFile :: Socket -> HttpMsg -> Maybe [Char] -> [Char] ->  IO Int

handleGetFile sock msg (Just dir) file = do

    let filePath = dir <> "/" <> file

    putStrLn $ "Going to open file " <> filePath

    catch (do

        handle <- openFile filePath ReadMode

        contents <- hGetContents handle

        send sock $ BC.pack ("HTTP/1.1 200 OK\r\nContent-Type: application/octet-stream\r\nContent-Length: " ++ show (length contents) ++ "\r\n\r\n" ++ contents))

        -- (\(SomeException _) -> 

        --     send sock "HTTP/1.1 404 NOT FOUND\r\n\r\n")

        (\(SomeException _) ->

            send sock "HTTP/1.1 404 Not Found\r\n\r\n")

handleGetFile sock _ Nothing _ = do

    -- send sock "HTTP/1.1 404 NOT FOUND\r\n\r\n"

    send sock "HTTP/1.1 404 Not Found\r\n\r\n"

main :: IO ()

main = do

    hSetBuffering stdout LineBuffering

    -- You can use print statements as follows for debugging, they'll be visible when running tests.

    BC.putStrLn "Logs from your program will appear here"

    -- Uncomment this block to pass first stage

    let host = "127.0.0.1"

        port = "4221"

    --

    BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port

    --

    -- Get address information for the given host and port

    addrInfo <- getAddrInfo Nothing (Just host) (Just port)

    --

    serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol

    setSocketOption serverSocket ReuseAddr 1

    bind serverSocket $ addrAddress $ head addrInfo

    listen serverSocket 2

    args <- getArgs

    BC.putStrLn $ "Args" <> BC.pack (show args)

    let directory = getDirectoryFromArgs args

    catch

        (

        -- Accept connections and handle them forever

        forever $ do

            (clientSocket, clientAddr) <- accept serverSocket

            forkIO $ do

                BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."

                -- Handle the clientSocket as needed...

                buff <- recv clientSocket 1024

                -- BC.putStrLn $ "Received " <> BC.pack (show buff)

                putStrLn $ "Received " <> show buff

                -- let msg = parseMsg buff

                -- let resp = case map BC.unpack . BC.split '/' $ msg.path of

                --             ("":"":_) -> "HTTP/1.1 200 OK\r\n\r\n"

                --             ("":"echo":payload) -> createBody payload

                --             ("":"user-agent":_) -> handleUserAgent msg

                --             ("":"files":filename) -> handleGetFile msg directory filename

                --             _ -> "HTTP/1.1 404 NOT FOUND\r\n\r\n"

                -- case map BC.unpack . BC.split '/' $ msg.path of

                --             ("":"":_) -> send clientSocket "HTTP/1.1 200 OK\r\n\r\n"

                --             ("":"echo":payload) -> send clientSocket $ BC.pack $ createBody payload

                --             ("":"user-agent":_) -> send clientSocket $ BC.pack $ handleUserAgent msg

                --             ("":"files":filename:_) -> handleGetFile clientSocket msg directory filename

                --             _ -> send clientSocket "HTTP/1.1 404 NOT FOUND\r\n\r\n"

                let Left msg = parseMsg buff

                let pathParts = map BC.unpack $ BC.split '/' msg.path

                putStrLn $ "Path " <> show pathParts

                case (msg.method, pathParts) of

                            (GET, "":"":_ )-> send clientSocket "HTTP/1.1 200 OK\r\n\r\n"

                            (GET, "":"echo":payload) -> send clientSocket $ BC.pack $ createBody payload

                            (GET, "":"user-agent":_) -> send clientSocket $ BC.pack $ handleUserAgent msg

                            (GET, "":"files":filename:_) -> handleGetFile clientSocket msg directory filename

                            (POST, "":"files":filename:_) -> handlePostFile clientSocket msg directory filename

                            (_, _) -> send clientSocket "HTTP/1.1 404 Not Found\r\n\r\n"

                    -- if path == "/" then

                    --      "HTTP/1.1 200 OK\r\n\r\n"

                    --     else  "HTTP/1.1 404 NOT FOUND\r\n\r\n"

                -- BC.putStrLn $ "Resp is " <> BC.pack (show resp)

                -- _ <- send clientSocket (BC.pack resp)

                close clientSocket

        )

        (   \UserInterrupt ->

            do

                close serverSocket

                BC.putStrLn "Interrupt exception - closing server"

        )

handlePostFile :: Socket -> HttpMsg -> Maybe [Char] -> [Char] -> IO Int

handlePostFile sock msg dir file = do

    putStrLn $ "Posted file " <> file

    let Just d = dir

    handle <- openFile (concat [d, "/", file]) WriteMode

    BC.hPut handle msg.body

    hClose handle

    send sock "HTTP/1.1 201 Created\r\n\r\n"

