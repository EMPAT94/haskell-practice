{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified System.IO as IO
import Data.List (filter)

import Data.List
import Control.Monad.IO.Class (MonadIO)
import Network.HTTP.Simple

type URL = String
type Title = String
type Contents = L8.ByteString
type ResponseBody = L8.ByteString

pageUrl = "http://link.springer.com/openurl?genre=book&isbn=978-0-306-48048-5"

main :: IO ()
main = do
  isbnDump <- IO.readFile "./ebook-isbns.txt"
  responseBody <- download pageUrl
  let pdfUrl = getPdfUrl responseBody
  print(pdfUrl)
 -- >>= parsePage  >>= print
  -- >>= download >>= parsePDF >>= writePDF
  print ("DONE")

-- parseBody :: ResponseBody -> PDFUrl
getPdfUrl body = "http://link.springer.com" ++ (takeWhile (/= '\"') $ drop 6 $ head $ filter ("href=\"/content" `isPrefixOf`) $ words $ L8.unpack body)

download :: MonadIO m => Request -> m (ResponseBody)
download url = do
  response <- httpLBS url
  return (getResponseBody response)

writePDF :: (Title, Contents) -> IO ()
writePDF (title, contents) = do
  handle <- IO.openFile (title ++ ".pdf") IO.WriteMode
  L8.hPutStr handle contents
