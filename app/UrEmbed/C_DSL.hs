{-# LANGUAGE OverloadedStrings #-}
module C_DSL where


import Data.Data
import Data.Either
import Data.Typeable
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Language.C.DSL hiding(fun, ty)
import Text.PrettyPrint (render)
import Text.Printf

cunit decls = CTranslUnit decls undefNode

fun :: [CDeclSpec] -> String -> [Maybe CExpr -> CDecl] -> CStat -> CExternalDeclaration NodeInfo
fun specs name args body = CFDefExt $ annotatedFun specs name args [] body

ty :: Ident -> CDeclSpec
ty i = CTypeSpec $ CTypeDef i undefNode

expr = liftE

blk :: BlockLike a => [a] -> CStat
blk = hBlock

mk_out_c :: String
mk_out_c =
  let
    blob =  ty "uw_Basis_blob"
    context =  ty "uw_context"
    unit =  ty "uw_unit"
  in
  render $
    pretty $
      cunit [
        fun [blob] "asBlob" [decl context (ptr "ctx"), decl unit "_"] $ blk [
            blk [uninit $ decl blob "uwblob"],
            expr $ ("uwblob" & "data") <-- (Addr`pre`("g_blob" ! 0)),
            expr $ ("uwblob" & "size") <-- "BLOBSZ",
            creturn "uwblob"
          ]
      ]

