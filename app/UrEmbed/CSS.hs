{-# LANGUAGE FlexibleContexts #-}
module CSS (parse_css) where

import Control.Applicative hiding((<|>),many)
import Control.Monad.Writer

import Data.Data
import Data.Either
import Data.Typeable

import Data.ByteString.Char8 (ByteString,pack,unpack)
import qualified Data.ByteString.Char8 as BS

import Language.JavaScript.Parser as JS

import System.Directory
import System.FilePath
import System.IO

import Text.Parsec as P hiding (string)
import Text.Parsec.Token as P hiding(lexeme, symbol)
import qualified Text.Parsec.Token as P
import Text.Parsec.ByteString as P
import Text.Printf

import Types

transform_css :: (Stream s m Char) => ParsecT s u m [Either ByteString Url]
transform_css = do
  l1 <- map Left <$> blabla
  l2 <- map Right <$> funs
  e <- try (eof >> return True) <|> (return False)
  case e of
    True -> return (l1++l2)
    False -> do
      l <- transform_css
      return (l1 ++ l2 ++ l)

  where

    symbol = P.symbol l
    lexeme = P.lexeme l

    string  = lexeme (
      between (char '\'') (char '\'') (strchars '\'') <|>
      between (char '"') (char '"') (strchars '"')) <|>
      manyTill anyChar (try (char ')'))
      where
        strchars e = many $ satisfy (/=e)

    fun1 = lexeme $ do
      symbol "url"
      symbol "("
      s <- string
      symbol ")"
      return s

    blabla = do
      l <- manyTill anyChar (eof <|> (try (lookAhead fun1) >> return ()))
      case null l of
        True -> return []
        False -> return [BS.pack l]

    funs = many (try fun1)

    l =  P.makeTokenParser $ P.LanguageDef
        { P.commentStart	 = "/*"
        , P.commentEnd	 = "*/"
        , P.commentLine	 = "//"
        , P.nestedComments = True
        , P.identStart	 = P.letter
        , P.identLetter	 = P.alphaNum <|> oneOf "_@-"
        , P.reservedNames   = []
        , P.reservedOpNames = []
        , P.caseSensitive  = False
        , P.opStart        = l
        , P.opLetter       = l
        }
        where l = oneOf ":!#$%&*+./<=>?@\\^|-~"

parse_css :: BS.ByteString -> Either P.ParseError (BS.ByteString, [Url])
parse_css inp =
  case P.runParser transform_css () "-" inp of
    Left e -> Left e
    Right pr -> Right $
      runWriter $ do
        BS.concat `liftM` (forM pr $ \i -> do
          case i of
            Left bs -> return bs
            Right u -> do
              let (h, t) = span (\c -> not $ elem c "?#") u
              let mn = guessModName h
              tell [ mn ]
              return $ pack ("url('/" ++ mn ++ "/content" ++ t ++ "')"))

