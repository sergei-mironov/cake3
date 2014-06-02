{-# LANGUAGE FlexibleContexts #-}

module Test where

import Data.Char

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 hiding(putStrLn,map,null)

import Control.Applicative hiding((<|>),many)
import Control.Monad

import Text.Parsec.ByteString as P
import Text.Parsec.Expr as P
import Text.Parsec.Combinator as P
import Text.Parsec.Prim as P
import qualified Text.Parsec.Char as P
import Text.Parsec.Char as P hiding (string)
import qualified Text.Parsec.Language as P
import Text.Parsec.Token as P hiding(lexeme, symbol)
import qualified Text.Parsec.Token as P
import Text.Parsec.ByteString as P

import System.IO


everything :: (Stream s m Char) => ParsecT s u m [Either ByteString [Char]]
everything = do
  l1 <- map Left <$> blabla
  l2 <- map Right <$> funs
  e <- try (eof >> return True) <|> (return False)
  case e of
    True -> return (l1++l2)
    False -> do
      l <- everything
      return (l1 ++ l2 ++ l)

  where

    symbol = P.symbol l
    lexeme = P.lexeme l

    string  = lexeme $ between (char '\'') (char '\'') strchars where
      strchars = many $ satisfy (\c -> (c /= '\''))

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
        False -> return [B.pack l]

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
    

main :: IO ()
main = do
  r <- P.parseFromFile everything "bootstrap.css"
  case r of
    Left err -> putStrLn $ show r
    Right ps -> do
      withFile "bootstrap.css.parsed" WriteMode $ \h -> do
        forM_ ps $ \i -> do
          case i of
            Left bs -> hPut h bs
            Right u -> hPut h (pack $ "url ('" ++ map toUpper u ++ "')")
      


