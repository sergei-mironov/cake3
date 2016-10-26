{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module JS where

import Language.JavaScript.Parser as JS
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Applicative
import Text.Printf
import System.IO as IO
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List as L
import Data.Data
import Data.Typeable
import Data.Generics
import Data.Maybe

import Types

data JSFunc = JSFunc {
    urdecl :: String -- ^ URS declaration for this function
  , urname :: String -- ^ UrWeb name of this function
  , jsname :: String -- ^ JavaScript name of this function
  } deriving(Show)

data JSType = JSType {
    urtdecl :: String
  } deriving(Show)

-- | Parse the JavaScript file, extract top-level functions, convert their
-- signatures into Ur/Web format, return them as the list of strings
parse_js :: BS.ByteString -> IO (Either String ([JSType],[JSFunc]))
parse_js contents = do
  runExceptT $ do
    c <- either fail return (JS.parse (BS.unpack contents) "<urembed_input>")
    f <- concat <$> (forM (findTopLevelFunctions c) $ \f@(fn:_) -> (do
      ts <- mapM extractEmbeddedType (f`zip`(False:repeat True))
      let urdecl_ = urs_line ts
      let urname_ = (fst (head ts))
      let jsname_ = fn
      return [JSFunc urdecl_ urname_ jsname_]
      ) `catchError` (\(e::String) -> do
        err $ printf "ignoring function %s, reason:\n\t%s" fn e
        return []))
    t <- concat <$> (forM (findTopLevelVars c) $ \vn -> (do
      (n,t) <- extractEmbeddedType (vn,False)
      return [JSType $ printf "type %s" t]
      )`catchError`  (\(e::String) -> do
        err $ printf "ignoring variable %s, reason:\n\t%s" vn e
        return []))
    return (t,f)

  where
    urs_line :: [(String,String)] -> String
    urs_line [] = error "BUG: wrong function signature"
    urs_line ((n,nt):args) = printf "val %s : %s" n (fmtargs args) where
      fmtargs :: [(String,String)] -> String
      fmtargs ((an,at):as) = printf "%s -> %s" at (fmtargs as)
      fmtargs [] = let pf = L.stripPrefix "pure_" nt in
                   case pf of
                     Just p -> p
                     Nothing -> printf "transaction %s" nt

    extractEmbeddedType :: (MonadError String m) => (String,Bool) -> m (String,String)
    extractEmbeddedType ([],_) = throwError "Empty identifier (BUG?)"
    extractEmbeddedType (name,fallback) = check (msum [span2  "__" name , span2 "_as_" name]) where
      check (Just (n,t)) = return (n,t)
      check _ | fallback == True = return (name,name)
              | fallback == False = throwError $ printf "Failed to extract the Ur/Web type hint from the identifier '%s'" name

    findTopLevelFunctions :: JSNode -> [[String]]
    findTopLevelFunctions top = map decls $ listify is_func top where
      is_func n@(JSFunction a b c d e f) = True
      is_func _ = False
      decls (JSFunction a b c d e f) = (identifiers b) ++ (identifiersC d)

    findTopLevelVars :: JSNode -> [String]
    findTopLevelVars top = map decls $ listify is_var top where
      is_var n@(JSVarDecl a []) = True
      is_var _ = False
      decls (JSVarDecl a _) = (head $ identifiers a);

    identifiersC x = map name $ listify ids x where
      ids i@(NT (JSIdentifier s) _ com) = True
      ids _ = False
      name (NT (JSIdentifier n) _ com)
        | not $ null $ comglue = n ++ "_as_" ++ comglue
        | otherwise = n
        where
          comglue = concat $ map
            (\c ->
              case c of
                CommentA _ c -> unwords $ filter (\c -> c /= "/*" && c /= "*/") $ words c
                _ -> "") com
      
    identifiers x = map name $ listify ids x where
      ids i@(JSIdentifier s) = True
      ids _ = False
      name (JSIdentifier n) = n

    err,out :: (MonadIO m) => String -> m ()
    err = hio stderr
    out = hio stdout

    span2 :: String -> String -> Maybe (String,String)
    span2 inf s = span' [] s where
      span' _ [] = Nothing
      span' acc (c:cs)
        | L.isPrefixOf inf (c:cs) = Just (acc, drop (length inf) (c:cs))
        | otherwise = span' (acc++[c]) cs

    hio :: (MonadIO m) => Handle -> String -> m ()
    hio h = liftIO . hPutStrLn h


