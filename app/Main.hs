module Main (main) where

import Text.Pandoc hiding (handleError)
import XMLParse -- Our slightly modified copy of Text.XML.HaXml.Parse

import System.IO (hFlush, stdout)
import Text.XML.HaXml.Lex (xmlLex)
import Text.XML.HaXml.Posn (Posn ())
import Text.ParserCombinators.Poly.State (stGet)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import Data.List.Split (chop)
import qualified Network.XmlRpc.DTD_XMLRPC as XR
import Text.XML.HaXml.XmlContent (Document (..), toXml, fromXml)
import qualified Text.XML.HaXml.ByteStringPP as XPP
import Text.XML.HaXml.Escape (xmlEscape, stdXmlEscaper)

-- Modified version of XMLParse.document that doesn't wait for anything after
-- the top-level element
rpcMessage :: XParser (Document Posn)
rpcMessage = do
  p <- prolog
  e <- element
  (_,ge) <- stGet
  return $ Document p ge e []

escapeDoc :: Document i -> Document i
escapeDoc (Document p s e m) = Document p s (xmlEscape stdXmlEscaper e) m

rpcIn :: IO [Document Posn]
rpcIn = map (either error id) . chop (xmlParseWith rpcMessage) .
  xmlLex "<stdin>" <$> getContents

rpcOut :: [Document ()] -> IO ()
rpcOut = mapM_ $ (<*) (hFlush stdout) . BSL8.putStr . XPP.document . escapeDoc

main :: IO ()
main = rpcOut . plugin =<< rpcIn

plugin :: [Document Posn] -> [Document ()]
plugin [] = []
plugin (i:is) = case parseRpcCall i of
  Just ("import", _) -> (hookCall :) $ case is of
    i':is' | isSuccessResponse i' -> importResponse : plugin is'
    _ -> []
  Just ("htmlize", args) -> rpcHtmlize args : plugin is
  _ -> []

type RpcArgs = [(String, XR.Value_)]

parseRpcCall :: Document Posn -> Maybe (String, RpcArgs)
parseRpcCall doc = case fromXml doc of
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

hookCall :: Document ()
hookCall = toXml False . XR.MethodCall (XR.MethodName "hook") . Just .
  XR.Params . map stringParam $
    [ "type", "htmlize"
    , "id", "mdwn"
    , "call", "htmlize"
    ]
  where stringParam = XR.Param . XR.Value . (:[]) . XR.Value_AString . 
          XR.AString

isSuccessResponse :: Document Posn -> Bool
isSuccessResponse doc = case fromXml doc of
  Right (XR.MethodResponseParams _) -> True
  _ -> False

importResponse :: Document ()
importResponse = toXml False . XR.MethodResponseParams . XR.Params . (:[]) .
  XR.Param . XR.Value . (:[]) . XR.Value_Nil $ XR.Nil ()

rpcHtmlize :: RpcArgs -> Document ()
rpcHtmlize args = toXml False . XR.MethodResponseParams . XR.Params . (:[]) .
  XR.Param . XR.Value . (:[]) . XR.Value_AString . XR.AString $ htmlize mdwn
  where Just (XR.Value_AString (XR.AString mdwn)) = lookup "content" args

htmlize :: String -> String
htmlize mdwn = either (error . show) id . runPure $ do
  pdoc <- readMarkdown readOpts (T.pack mdwn)
  T.unpack <$> writeHtml5String def pdoc
  where readOpts = def {readerExtensions = pandocExtensions}
