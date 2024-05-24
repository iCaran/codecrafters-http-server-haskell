{-# LANGUAGE ImportQualifiedPost #-}

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)

import Data.Bifunctor (Bifunctor (second), first)

import Data.ByteString qualified as BS

import Data.ByteString.Char8 qualified as BC

import Data.ByteString.Internal qualified as BS

import Network.Socket

import Network.Socket.ByteString (recv, sendAll)

import System.IO (BufferMode (..), hSetBuffering, stdout)

main :: IO ()

main = do

  hSetBuffering stdout LineBuffering

  let host = "127.0.0.1"

      port = "4221"

  BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port

  addrInfo <- getAddrInfo Nothing (Just host) (Just port)

  serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol

  setSocketOption serverSocket ReuseAddr 1

  setSocketOption serverSocket ReusePort 1

  bind serverSocket $ addrAddress $ head addrInfo

  listen serverSocket 5

  forever $ do

    (clientSocket, clientAddr) <- accept serverSocket

    BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."

    -- let nextLine old = do

    --       new <- recv clientSocket 256

    --       let full = old <> new

    --       let (line, rest) = BS.breakSubstring "\r\n" full

    --       if BS.length line + 2 <= BS.length full

    --         then return (line, BS.drop 2 rest)

    --         else nextLine full

    -- (line, rest) <- nextLine ""

    -- let [_, path, _] = BS.split (BS.c2w ' ') line

    -- let splitPath = BS.split (BS.c2w '/') path

    -- sendAll clientSocket =<< case (path, splitPath) of

    init <- recv clientSocket 4096

    let parseLines bs = do

          if "\r\n" `BS.isPrefixOf` bs

            then do

              return ([], BS.drop 2 bs)

            else do

              let (line, rest) = BS.breakSubstring "\r\n" bs

              if BS.length line + 2 <= BS.length bs

                then do

                  first (line :) <$> parseLines (BS.drop 2 rest)

                else do

                  extra <- recv clientSocket 4096

                  parseLines $ bs <> extra

    (startLine : rawHeaders, bodyStart) <- parseLines init

    let [_, path, _] = BS.split (BS.c2w ' ') startLine

    let pathParts = BS.split (BS.c2w '/') path

    let headers = map (second (BS.drop 2) . BS.breakSubstring ": ") rawHeaders

    sendAll clientSocket =<< case (path, pathParts) of

      ("/", _) -> do

        return "HTTP/1.1 200 OK\r\n\r\n"

      (_, ["", "echo", s]) -> do

        return

          $ mconcat

            [ "HTTP/1.1 200 OK"

            , "\r\n"

            , "Content-Type: text/plain\r\n"

            , "Content-Length: " <> (BS.packChars . show $ BS.length s) <> "\r\n"

            , "\r\n"

            , s

            ]

      (_, ["", "user-agent"]) -> do

        let Just s = lookup "User-Agent" headers

        return

          $ mconcat

            [ "HTTP/1.1 200 OK"

            , "\r\n"

            , "Content-Type: text/plain\r\n"

            , "Content-Length: " <> (BS.packChars . show $ BS.length s) <> "\r\n"

            , "\r\n"

            , s

            ]

      _ -> do

        return "HTTP/1.1 404 Not Found\r\n\r\n"

    close clientSocket