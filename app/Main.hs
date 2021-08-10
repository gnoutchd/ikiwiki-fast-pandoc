module Main (main) where

import Network.XmlRpc.Internals
import XMLParse -- Our slightly modified copy of Text.XML.HaXml.Parse
import Data.IORef
import System.IO
import Text.XML.HaXml.XmlContent (fromXml)
import Text.XML.HaXml.Lex (Token (), xmlLex)
import Text.XML.HaXml.Types (Document (..))
import Text.XML.HaXml.Posn (Posn ())
import Text.ParserCombinators.Poly.State (stGet)
import Control.Monad.Except (liftEither)
import Text.Pandoc hiding (handleError)
import Control.Monad (void)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T

type Stream = IORef [Token]
type Callback = MethodCall -> IO MethodResponse

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  handleCalls =<< newIORef =<< xmlLex "<stdin>" <$> getContents

handleCalls :: Stream -> IO ()
handleCalls stream = do
  let callback = callbackWith stream
  mCall <- readCall stream
  case mCall of
    Nothing -> return ()
    Just call -> do
      BSL8.putStrLn . renderResponse =<< case call of
        MethodCall "import" [] -> handleImportCall callback
        MethodCall "htmlize" args -> handleHtmlizeCall args
        _ -> return (Fault 0 "Unknown method")
      handleCalls stream

readCall :: Stream -> IO (Maybe MethodCall)
readCall streamR = do
  stream <- readIORef streamR
  case stream of
    [] -> return Nothing
    _ -> Just <$> do
      let (xml, stream') = xmlParseWith rpcMessage stream
      writeIORef streamR stream'
      handleError fail $ fromXRMethodCall =<< liftEither (fromXml =<< xml)

callbackWith :: Stream -> Callback
callbackWith streamR call = do
  BSL8.putStrLn $ renderCall call
  (xml, stream') <- xmlParseWith rpcMessage <$> readIORef streamR
  writeIORef streamR stream'
  handleError fail $ fromXRMethodResponse =<< liftEither (fromXml =<< xml)

-- Modified version of XMLParse.document that doesn't wait for anything after
-- the top-level element
rpcMessage :: XParser (Document Posn)
rpcMessage = do
  p <- prolog
  e <- element
  (_,ge) <- stGet
  return $ Document p ge e []

handleImportCall :: Callback -> IO MethodResponse
handleImportCall callback = do
  void $ callback $ MethodCall "hook" 
    [ ValueString "type", ValueString "htmlize"
    , ValueString "id",   ValueString "mdwn"
    , ValueString "call", ValueString "htmlize"]
  return $ Return ValueNil

namedVals :: [Value] -> [(String, Value)]
namedVals ((ValueString key):val:xs) = (key, val) : namedVals xs
namedVals [] = []
namedVals _ = error "arguments not in IkiWiki named-value form"

handleHtmlizeCall :: [Value] -> IO MethodResponse
handleHtmlizeCall args = 
  either (fail . show) return . runPure $ do
    pdoc <- readMarkdown readOpts (T.pack mdwn)
    Return . ValueString . T.unpack <$> writeHtml5String def pdoc
  where Just (ValueString mdwn) = lookup "content" (namedVals args)
        readOpts = def {readerExtensions = pandocExtensions}
