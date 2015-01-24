{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Development.Cake3.Writer (defaultMakefile,buildMake) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (MonadState, StateT(..), runStateT, State(..), execState, evalState, runState, modify, get, put)
import Control.Monad.Trans
import Control.Monad.Writer (MonadWriter, WriterT(..), execWriterT, execWriter, tell)
import Data.List as L
import Data.Char
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.String
import Data.Foldable (Foldable(..), forM_)
import qualified Data.Foldable as F
import Data.Traversable (forM)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Text.Printf

import System.FilePath.Wrapper
import Development.Cake3.Types
import Development.Cake3.Monad

class ToMakeText x where
  toMakeText :: x -> String
  
instance ToMakeText [Char] where
  toMakeText = id

escapeFile f = escapeFile' (toFilePath f)
escapeFile' [] = []
escapeFile' (' ':xs) = "\\ " ++ escapeFile' xs
escapeFile' (x:xs) = (x:(escapeFile' xs))

instance ToMakeText File where
  toMakeText = escapeFile

instance ToMakeText Command where
  toMakeText cmd = concat $ map toMakeText cmd

instance ToMakeText CommandPiece where
  toMakeText (CmdStr s) = s
  toMakeText (CmdFile f) = toMakeText f

instance ToMakeText (Set File) where
  toMakeText s = intercalate " " (map toMakeText (S.toList s))
 
smap f = map f . S.toList

line :: (MonadState String m) => String -> m ()
line s = modify $ \ws -> concat [ws, s, "\n"]

text :: (MonadState String m) => String -> m ()
text s = modify $ \ws -> concat [ws, s]

newtype MakeLL a = MakeLL { unMakeLL :: State ([File], Set Recipe) a }
  deriving(Functor, Monad, MonadState ([File], Set Recipe), Applicative)

fresh :: MakeLL File
fresh = do
  (f:fs,rs) <- get
  put (fs,rs)
  return f

runMakeLL :: String -> MakeLL () -> Set Recipe
runMakeLL templ m = snd $ execState (unMakeLL m) (names, S.empty) where
  names = map fromFilePath $ map (\x -> printf ".%s%d" templ x) $ ([1..] :: [Int])

copyRecipeLL :: Recipe -> MakeLL ()
copyRecipeLL r = modify (\(a,b) -> (a,S.insert r b))

ruleLL :: A' MakeLL a -> MakeLL Recipe
ruleLL act = do
  (r,_) <- runA "<internal>" act
  copyRecipeLL r
  return r

applySubprojects :: Map File [Command] -> Set Recipe -> Set Recipe
applySubprojects sp rs = runMakeLL "subproject" (transformRecipesM_ f rs) where
  f r | L.null scmds = copyRecipeLL r
      | otherwise = do
          n1 <- fresh
          n2 <- fresh
          r1 <- ruleLL $ do
            produce n2
            commands scmds
            markPhony
          r2 <- ruleLL $ do
            produce n1
            depend (rsrc r)
            commands (rcmd r)
          ruleLL $ do
            produce (rtgt r)
            depend r1
            depend r2
          return ()
      where
        scmds :: [Command]
        scmds = concat $ catMaybes $ map (flip M.lookup sp) (S.toList $ rsrc r)


-- | Turns multi-target rules of the form
--
--    a b c : d e f
--      cmd1
--
-- into the pair:
--
--    a : stampX
--    b : stampX
--    c : stampX
--
--    .INTERMEDIATE:stampX
--    stampX : d e f
--      cmd1
--
fixMultiTarget :: (Foldable t) => t Recipe -> Set Recipe
fixMultiTarget rs = runMakeLL "fix-multy" (transformRecipesM_ f rs) where
  f r | (S.size (rtgt r)) > 1 = do
          s <- fresh
          forM_ (S.toList $ rtgt r) $ \t -> do
            ruleLL $ do
              produce t
              location (rloc r)
              depend s
              flags (rflags r)
          ruleLL $ do
            produce s
            depend (rsrc r)
            variables (rvars r)
            commands (rcmd r)
            location (rloc r)
            markIntermediate
          return ()
      | otherwise = do
          copyRecipeLL r

-- | Operate on a prerequisites which themselfs are targets of a multitarget rule. Make
-- the conversion from:
--
--    a b c : x
--    x : a
--    y : b
--
-- to
--
--    a b c : x
--    x : a b c
--    y : a b c
--
--
completeMultiTarget :: Set Recipe -> Set Recipe
completeMultiTarget rs = 
  let
    badlist = S.foldl' (\ts r -> do
      if (S.size (rtgt r)) > 1 then (rtgt r):ts else ts) [] rs
  in
    flip S.map rs $ \r ->
      L.foldl' (\r mulpack ->
        case (not . S.null) ((rsrc r)`S.intersection` mulpack) of
          True -> r { rsrc = (rsrc r) `S.union` mulpack }
          False -> r) r badlist

hasClean :: (Foldable t) => t Recipe -> Bool
hasClean rs = F.foldl' flt False rs where
  flt True _ = True
  flt False r = (Phony `S.member`(rflags r)) && ((fromFilePath "clean")`S.member`(rtgt r))

intermediateFiles :: (Foldable t) => t Recipe -> Set File
intermediateFiles rs = 
  execWriter $ do
    forM_ rs $ \r -> do
      when (not $ Phony `S.member` (rflags r)) $ do
        tell (rtgt r)

cleanRuleLL :: Set File -> MakeLL Recipe
cleanRuleLL fs =
  ruleLL $ do
    phony "clean"
    unsafeShell [cmd|-rm $(fs)|]
    unsafeShell [cmd|-rm -rf .cake3|]

-- | Define a 'clean' phony target. The rule removes all targets except phony
-- targets and the Makefile itself
defineClean :: File -> Set Recipe -> Set Recipe
defineClean mk rs =
  runMakeLL "defineClean" $ do
    let fs = intermediateFiles rs
    cleanRuleLL (fs `S.difference` (S.singleton mk))
    return ()

-- | Rule referring to the 
defaultMakefile :: File
defaultMakefile = fromFilePath ("." </> "Makefile")

addRebuildDeps :: File -> Set File -> Set Recipe -> Set Recipe
addRebuildDeps makefile deps rs = S.map mkd rs where
  mkd r | makefile `S.member` (rtgt r) = addPrerequisites deps r
        | otherwise = r

isRequiredFor :: Set Recipe -> Recipe -> File -> Bool
isRequiredFor rs r f = if f`S.member`(rtgt r) then True else godeeper where
  godeeper = or $ map (\tgt -> or $ map (\r -> isRequiredFor rs r f) (selectBySrc tgt)) (S.toList $ rtgt r)
  selectBySrc f = S.toList . fst $ S.partition (\r -> f`S.member`(rsrc r)) rs

-- | There are only 2 kind of rules: 1) rules which depend on the Makefile, and
-- 2) rules which Makefile depends on. Case-2 is known in advance (for example,
-- when the contents of a file is required to build a Makefile then Makefile
-- depends on this file). This function adds the case-1 dependencies.
addMakeDeps :: File -> Set Recipe -> Set Recipe
addMakeDeps makefile rs = S.map addMakeDeps_ rs where
  addMakeDeps_ r | not (isRequiredFor rs r makefile) = addPrerequisite makefile r
                 | otherwise = r



-- | Render the Makefile. Return either the content (Right), or error messages
-- (Left).
buildMake :: MakeState -> Either String String
buildMake ms = do

  mr <- region "MAIN" $ do
    line ""
    line "# Main section"
    line ""
    writeRules rs'
    forM_ (includes ms) $ \i -> do
      line (printf "include %s" (toMakeText i))
    line ""

  sr <- region "SERVICE" $ do
    r <- runA_ "<internal>" $ do
      produce (queryTargets (recipes ms))
      unsafeShell [cmd|-mkdir .cake3|]
      commands (rcmd $ prebuilds ms)
      unsafeShell [cmd|MAIN=1 $(make) -f $(outputFile ms) $(makecmdgoals)|]
      commands (rcmd $ postbuilds ms)
      markPhony

    line ""
    line "# Prebuild/postbuild section"
    line ""

    case hasClean (recipes ms) of
      True -> do
        writeRules $ applyPlacement (placement ms)
                   $ fixMultiTarget [r]
      False -> do
        line "ifneq ($(MAKECMDGOALS),clean)"
        line ""
        writeRules $ applyPlacement (placement ms)
                   $ fixMultiTarget [r]
        line ""
        line "endif"
        writeRules $ defineClean (outputFile ms) (recipes ms)
    line ""

  hdr <- runLines $ do
    line "# This Makefile was generated by the Cake3"
    line "# https://github.com/grwlf/cake3"
    line ""
    line "GUARD = .cake3/GUARD_$(1)_$(shell echo $($(1)) | md5sum | cut -d ' ' -f 1)"
    line ""
    
  writeRegions hdr [mr,sr]

  where
    make = extvar "MAKE"
    makecmdgoals = extvar "MAKECMDGOALS"

    rs' = applyPlacement (placement ms)
        $ fixMultiTarget
        $ completeMultiTarget
        $ addMakeDeps (outputFile ms)
        $ addRebuildDeps (outputFile ms) (makeDeps ms)
        $ recipes ms

data MakeRegion = MR {
    mrname :: String
  , mrtext :: String
}

type Lines = StateT String (Either String) ()

runLines :: Lines -> Either String String
runLines s = let e = runStateT s "" in
  case e of
    Left e -> Left e
    Right ((),st) -> Right st

writeRegions :: String -> [MakeRegion] -> Either String String
writeRegions hdr rs = mappend <$> (pure hdr) <*> (writeRegions' rs) where
  writeRegions' [] = fail "No regions are defined"
  writeRegions' (r:rs)
    | L.null rs = return (mrtext r)
    | otherwise = do
      inner <- writeRegions' rs
      runLines $ do
        line (printf "ifeq ($(%s),1)" (map toUpper $ mrname r))
        line (printf "unexport %s" (map toUpper $ mrname r))
        text (mrtext r)
        line "else"
        text inner
        line "endif"

region name mlines = do
  lines <- runLines mlines
  return $ MR { mrname = name , mrtext = lines}

writeRules rs = do
  vs <- lift $ queryVariablesE rs

  -- Variables
  forM_ vs $ \v -> case v of
    (Variable n (Just v)) -> line (printf "%s = %s" n v)
    (Variable n Nothing) -> return ()

  -- Rules
  forM_ rs $ \r -> do
    let varguard v = printf "$(call GUARD,%s)" (vname v)
    let deps = intercalate " " $ (smap toMakeText (rsrc r)) ++ (smap varguard (rvars r))
    let tgts = intercalate " " $ (smap toMakeText (rtgt r))

    when (Phony `S.member` (rflags r)) $ do
      line (printf ".PHONY: %s" tgts)
    when (Intermediate `S.member` (rflags r)) $ do
      line (printf ".INTERMEDIATE: %s" tgts)

    line (printf "%s: %s" tgts deps)
    forM_ (rcmd r) $ \c -> do
      line (printf "\t%s" (toMakeText c))

  -- Rules for variable guards
  -- FIXME: add those on the higher level
  forM_ vs $ \v -> do
    line (printf "$(call GUARD,%s):" (vname v))
    line (printf "\trm -f .cake3/GUARD_%s_*" (vname v))
    line "\ttouch $@"

