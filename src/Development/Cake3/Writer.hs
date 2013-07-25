module Development.Cake3.Writer (toMake) where

import Control.Applicative
import Control.Monad.State
import Data.List
import Development.Cake3.Types
import Text.Printf

rule2vars r = let Uniq vs = fst $ collapse' (rvars r) in vs

tell1 s = modify $ \ws -> ws { ls = s:(ls ws) }

tellX ss = forM_ ss tell1

gen :: MakeWriter Int
gen = do
  x <- head <$> cnt <$> get
  modify $ \ws -> ws { cnt = tail (cnt ws) }
  return x

data WS = WS { cnt :: [Int] , ls :: [String] }
  deriving(Show)

type MakeWriter a = State WS a

toMake tree =
  let (Uniq vs, Uniq rs, es) = collapse tree
  in  unlines $ reverse $ ls $ flip execState (WS [1..] []) $ do

  tell1 "GUARD = $(1)_GUARD_$(shell echo $($(1)) | md5sum | cut -d ' ' -f 1)"

  forM_ vs $ \(Variable n v) -> do
    tell1 $ printf "%s = %s" n v

  forM_ rs $ \r -> do
    let varguard v = printf "$(call GUARD,%s)" (vname v)
    let deps = (intercalate " " $ (rsrc r) ++ (map varguard (rule2vars r)))

    when (rphony r) $ do
      tell1 $ printf ".PHONY: %s" $ intercalate " " (rtgt r)

    let tgts = rtgt r
    case (length tgts) of
      0 -> do
        return ()
      1 -> do
        let s = (head $ rtgt r)
        case null (rloc r) of
          True -> tell1 $ printf "%s : %s" s deps
          False -> tell1 $ printf "%s : %s # %s" s deps (rloc r)
        forM_ (rcmd r) $ \c -> tell1 ("\t" ++ c)
      otherwise -> do
        s <- (("stamp"++) . show) <$> gen
        tell1 $ ".INTERMEDIATE:" ++ s
        case null (rloc r) of
          True -> tell1 $ printf "%s : %s" s deps
          False -> tell1 $ printf "%s : %s # %s" s deps (rloc r)
        forM_ (rcmd r) $ \c -> tell1 ("\t" ++ c)
        tell1 $ (intercalate " " tgts) ++ " : " ++ s

  forM_ vs $ \v -> do
    tellX $ [
        printf "$(call GUARD,%s) :" (vname v)
      , printf "\trm -f %s_GUARD_*" (vname v)
      , printf "\ttouch $@"
      -- FIXME: why do we need this empty line?
      , ""
      ]
  
  
  
  
