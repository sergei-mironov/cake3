module Development.Cake3.Ext.UrEmbed.Types where

import Data.Char
import System.FilePath
import Text.Printf

data Args = A
  { bver :: Bool
  , out_c :: FilePath
  , out_h :: FilePath
  , out_urs :: FilePath
  , out_wrapper :: FilePath
  , out_ffi_js :: FilePath
  , mangle_css_url :: Bool
  , inp :: FilePath
  }


guessModName :: FilePath -> String
guessModName = uwModName . (++ ".urs")

uwModName :: FilePath -> String
uwModName = upper1 . notnum . map under . takeFileName . dropExtension . checkurs where
  checkurs x | (takeExtension x) == ".urs" = x
             | (takeExtension x) == ".ur" = x
             | otherwise = error $ "uwModName: FILE.urs expected (got " ++ x ++ ")"
  under c | c`elem`"_-. /" = '_'
          | otherwise = c
  upper1 [] = []
  upper1 (x:xs) = (toUpper x) : xs
  notnum [] = error $ "uwModName: Empty name"
  notnum n@(x:xs) | isDigit x = error $ "uwModName: Names starting from digit is not allowed (got " ++ n ++ ")"
                  | otherwise = n


urblobfun = "blob"
urtextfun = "text"

cblobfun a = printf "uw_%s_%s" (uwModName (out_urs a)) urblobfun
ctextfun a = printf "uw_%s_%s" (uwModName (out_urs a)) urtextfun

type Url = String

css_mangle_flag = "css-mangle-urls"

