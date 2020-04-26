{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified System.IO as IO
import Data.List (filter, isPrefixOf)
-- import Control.Monad.IO.Class (MonadIO)
import Network.HTTP.Simple (parseRequest, httpLBS, getResponseHeader, getResponseBody)

type URL = String
type Title = String
type Contents = L8.ByteString
type ResponseBody = L8.ByteString

main :: IO ()
main = do
  isbnDump <- IO.readFile "./ebook-isbns.txt"
  processIsbnList $ lines isbnDump

processIsbnList [] = print (" --- DONE, ENJOY READING --- ")
processIsbnList (x:xs) = do
  print ("Processing isbn : " ++ x ++ " (" ++ show (length xs) ++ " remaining)")
  pageReq <- parseRequest ("http://link.springer.com/openurl?genre=book&isbn=" ++ x)
  print ("Getting download link...")
  pageResponse <- download pageReq
  print ("Received download link")
  print ("Downloading book...")
  downloadReq <- parseRequest (getPdfUrl $ getResponseBody pageResponse)
  downloadRes <- download downloadReq
  print ("Downloaded book")
  let title = getBookTitle downloadRes
  print ("Saving as " ++ title ++ ".pdf")
  print ("---")
  writePdf (title, getResponseBody downloadRes)
  processIsbnList xs

getBookTitle res = takeWhile (/= '.')
  $ drop 11
  $ show (getResponseHeader "Content-Disposition" res)

getPdfUrl body =  "http://link.springer.com" ++ (
  takeWhile (/= '\"')
  $ drop 6
  $ head
  $ filter ("href=\"/content" `isPrefixOf`)
  $ words
  $ L8.unpack body)

download url = do
  response <- httpLBS url
  return response

writePdf (title, contents) = do
  handle <- IO.openFile (title ++ ".pdf") IO.WriteMode
  L8.hPutStr handle contents
