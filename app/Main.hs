{-# OPTIONS_GHC -fno-full-laziness #-}
module Main (main) where

import XMLParse -- Our slightly modified copy of Text.XML.HaXml.Parse

import System.IO (hFlush, stdout)
import Text.XML.HaXml.Lex (xmlLex)
import Text.XML.HaXml.Posn (Posn ())
import Text.ParserCombinators.Poly.State (stGet)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import Data.List.Split (chop)
import qualified Network.XmlRpc.DTD_XMLRPC as XR
import Text.XML.HaXml.XmlContent (Document (..), fromXml)
import Text.XML.HaXml.Escape (xmlUnEscape, stdXmlEscaper)
import qualified Text.Pandoc as P
import qualified Network.XmlRpc.Internals as XRI

-- Modified version of XMLParse.document that doesn't wait for anything after
-- the top-level element
rpcMessage :: XParser (Document Posn)
rpcMessage = do
  p <- prolog
  e <- element
  (_,ge) <- stGet
  return $ Document p ge e []

rpcIn :: IO [Document Posn]
rpcIn = map (either error id) . chop (xmlParseWith rpcMessage) .
  xmlLex "<stdin>" <$> getContents

bslOut :: [BSL.ByteString] -> IO ()
bslOut = mapM_ $ (*> hFlush stdout) . BSL8.putStrLn

main :: IO ()
main = bslOut . plugin =<< rpcIn

plugin :: [Document Posn] -> [BSL.ByteString]
plugin [] = []
plugin (i:is) = case parseRpcCall i of
  Just ("import", _) -> (hookCall :) $ case is of
    i':is' | isSuccessResponse i' -> importResponse : plugin is'
    _ -> []
  Just ("htmlize", args) -> rpcHtmlize args : plugin is
  _ -> []

type RpcArgs = [(String, XR.Value_)]

parseRpcCall :: Document Posn -> Maybe (String, RpcArgs)
parseRpcCall doc = case fromXml (unEscapeDoc doc) of
  Right (XR.MethodCall (XR.MethodName method) mArgs) ->
    Just . (,) method $ case mArgs of
      Just (XR.Params params) -> flattenParams params
      Nothing -> []
  _ -> Nothing
  where
  flattenParams [] = []
  flattenParams ( XR.Param (XR.Value [XR.Value_AString (XR.AString key)]) 
                : XR.Param (XR.Value [val]) 
                : ps
                ) = (key, val) : flattenParams ps
  flattenParams _ = error "args not in named-value format"

unEscapeDoc :: Document i -> Document i
unEscapeDoc (Document p s e m) = Document p s (xmlUnEscape stdXmlEscaper e) m

hookCall :: BSL.ByteString
hookCall = XRI.renderCall . XRI.MethodCall "hook" . map XRI.ValueString $
    [ "type", "htmlize"
    , "id", "mdwn"
    , "call", "htmlize"
    ]

isSuccessResponse :: Document Posn -> Bool
isSuccessResponse doc = case fromXml doc of
  Right (XR.MethodResponseParams _) -> True
  _ -> False

importResponse :: BSL.ByteString
importResponse = XRI.renderResponse $ XRI.Return XRI.ValueNil

rpcHtmlize :: RpcArgs -> BSL.ByteString
rpcHtmlize args = XRI.renderResponse . XRI.Return . XRI.ValueString .
  htmlize $ mdwn
  where Just (XR.Value_AString (XR.AString mdwn)) = lookup "content" args

htmlize :: String -> String
htmlize = either (error . show) T.unpack . P.runPure .
  (P.writeHtml5String P.def =<<) . P.readMarkdown readOpts . T.pack .
  filter (\c -> c `elem` "\t\n\r" || (c>=' ' && c/='\x7f'))
  where readOpts = P.def {P.readerExtensions = P.pandocExtensions}
