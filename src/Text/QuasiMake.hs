{-# LANGUAGE ExistentialQuantification, TemplateHaskell, QuasiQuotes, OverloadedStrings, FlexibleInstances, UndecidableInstances, IncoherentInstances #-}

module Text.QuasiMake (Chunk (..), getChunks) where
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Language.Haskell.Meta (parseExp)

import Data.Attoparsec.Text
import qualified Data.Text as T 
import Data.Text (Text)
import Data.Char

import Data.Monoid
import Control.Applicative
                        
instance Lift Text where
    lift = litE . stringL . T.unpack

data Chunk 
    = T Text -- ^ text
    | E Char Text -- ^ expression
    -- | V Text -- ^ value
  deriving (Show, Eq)

class Textish a where
    toText :: a -> Text

instance Textish Text where
    {-# INLINE toText #-}
    toText x = x

instance Textish [Char] where
    {-# INLINE toText #-}
    toText x = T.pack x

instance Show a => Textish a where 
    {-# INLINE toText #-}
    toText x = T.pack (show x)

-- | A simple 'QuasiQuoter' to interpolate 'Text' into other pieces of 'Text'. 
-- Expressions can be embedded using $(expr), and values can be interpolated 
-- with $name. Inside $( )s, if you have a string of ambiguous type, it will 
-- default to the Show instance for toText, which will escape unicode 
-- characters in the string, and add quotes around them.

-- embed :: QuasiQuoter
-- embed = QuasiQuoter
--     { quoteExp = \s -> 
--         let chunks = flip map (getChunks (T.pack s)) $ \c ->
--                     case c of
--                         T t -> [| t |]

--                         E t -> case parseExp (T.unpack t) of
--                             Left  e -> error e
--                             Right e -> appE [| toText |] (return e)

--                         V t -> appE [| toText |] (global (mkName (T.unpack t)))

--         in appE [| T.concat |] (listE chunks)
--     }

-- | Create 'Chunk's without any TH.
getChunks :: Text -> [Chunk]
getChunks i = case parseOnly parser (T.strip i) of
        Right m -> m
        _       -> error "Unclosed parenthesis."

  where
    parenthesis '(' = True
    parenthesis ')' = True
    parenthesis _   = False

    parseExpression :: Int -> Parser [Text]
    parseExpression level = do
        expr  <- takeTill parenthesis
        paren <- anyChar
        case paren of
            ')' | level <= 0 -> return [expr]
                | otherwise  -> do
                    next <- parseExpression (level - 1)
                    return ([expr, ")"] ++ next)

            '(' -> do
                next <- parseExpression (level + 1)
                return ([expr, "("] ++ next)

            _ -> return [expr, T.singleton paren]

    parser :: Parser [Chunk]
    parser = fmap concat $ flip manyTill endOfInput $ do
        text <- takeTill (\c -> elem c "@$%")
        end  <- atEnd
        if end
            then return [T text]
            else do
                delim <- anyChar
                next <- anyChar
                case next of
                    -- opening an experssion
                    '(' -> do
                        expr <- T.concat <$> parseExpression 0
                        return [T text, E delim expr]
                    c | c == delim -> do
                        -- escaped '$', '@' or '%' 
                        return [T (text <> T.singleton delim)]

                      | otherwise -> do
                        -- value
                        name <- takeTill (\c -> not (isAlphaNum c || c == '_') )
                        return [T text, E delim (T.cons next name)]
                        


