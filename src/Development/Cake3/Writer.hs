{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Development.Cake3.Writer (buildMake, makefile) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (MonadState, StateT(..), runStateT, State(..), execState, evalState, runState, modify, get, put)
import Control.Monad.Trans
import Data.List as L
import Data.Char
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.String
import Data.Foldable (Foldable(..), foldl')
import Data.Foldable (forM_)
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

instance ToMakeText [[Char]] where
  toMakeText = concat . map toMakeText

instance ToMakeText File where
  toMakeText (FileT f) = escape f where
    escape [] = []
    escape (' ':xs) = "\\ " ++ escape xs
    escape (x:xs) = (x:(escape xs))

trimE = dropWhileEnd isSpace
trimB = dropWhile isSpace

cs a b = a ++ (' ':b)

instance ToMakeText Command where
  toMakeText [x] = either toMakeText toMakeText x
  toMakeText ((Left str):(Right f):cmd) = toMakeText ((Left ((trimE str)`cs` (toMakeText f))):cmd)
  toMakeText ((Right f):(Left str):cmd) = toMakeText ((Left ((toMakeText f)`cs`(trimB str))):cmd)
  toMakeText ((Right a):(Right b):cmd) = toMakeText ((Left ((toMakeText a)`cs`(toMakeText b))):cmd)
  toMakeText ((Left s1):(Left s2):cmd) = toMakeText ((Left (s1++s2)):cmd)

smap f = map f . S.toList

line :: (MonadState String m) => String -> m ()
line s = modify $ \ws -> concat [ws, s, "\n"]

text :: (MonadState String m) => String -> m ()
text s = modify $ \ws -> concat [ws, s]

-- type Tmp a = State [Int] a

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

produceLL :: Recipe -> MakeLL ()
produceLL r = modify (\(a,b) -> (a,S.insert r b))

ruleLL :: A' MakeLL a -> MakeLL Recipe
ruleLL act = do
  (r,_) <- runA "<internal>" act
  produceLL r
  return r

applySubprojects :: Map File [Command] -> Set Recipe -> Set Recipe
applySubprojects sp rs = runMakeLL "subproject" (transformRecipesM_ f rs) where
  f r | L.null scmds = produceLL r
      | otherwise = do
          n1 <- fresh
          n2 <- fresh
          r1 <- ruleLL $ do
            produce n2
            addCommands scmds
            markPhony
          r2 <- ruleLL $ do
            produce n1
            depend (rsrc r)
            addCommands (rcmd r)
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
--    a b c : stampX
--
--    .INTERMEDIATE:stampX
--    stampX : d e f
--      cmd1
--
fixMultiTarget :: (Foldable t) => t Recipe -> Set Recipe
fixMultiTarget rs = runMakeLL "fix-multy" (transformRecipesM_ f rs) where
  f r | (S.size (rtgt r)) > 1 = do
          s <- fresh
          ruleLL $ do
            produce (rtgt r)
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
          produceLL r

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

makefile :: File
makefile = fromFilePath "Makefile"

addRebuildDeps :: Set File -> Set Recipe -> Set Recipe
addRebuildDeps deps rs = S.map mkd rs where
  mkd r | makefile `S.member` (rtgt r) = addPrerequisites deps r
        | otherwise = r

isRequiredFor :: Set Recipe -> Recipe -> File -> Bool
isRequiredFor rs r f = if f`S.member`(rtgt r) then True else godeeper where
  godeeper = or $ map (\tgt -> or $ map (\r -> isRequiredFor rs r f) (selectBySrc tgt)) (S.toList $ rtgt r)
  selectBySrc f = S.toList . fst $ S.partition (\r -> f`S.member`(rsrc r)) rs

-- | There are only 2 kind of rules: 1) ones that depend on a Makefile, and 2) ones
-- that Makefile depends on. Case-2 is known in advance (for example, when the
-- the contents of a file is required to build a Makefile then Makefile depends
-- on this file). This function adds the case-1 dependencies.
addMakeDeps :: Set Recipe -> Set Recipe
addMakeDeps rs
  | S.null (S.filter (\r -> makefile `S.member` (rtgt r)) rs) = rs
  | otherwise = S.map addMakeDeps_ rs
  where
    addMakeDeps_ r | not (isRequiredFor rs r makefile) = addPrerequisite makefile r
                   | otherwise = r



-- | Renders the (Right Makefile)
buildMake :: MakeState -> Either String String
buildMake ms = do
  mr <- mainRegion
  sr <- serviceRegion
  writeRegions [mr,sr]
  where
    make = extvar "MAKE"
    makecmdgoals = extvar "MAKECMDGOALS"

    rs' = applyPlacement (placement ms)
        $ fixMultiTarget
        $ completeMultiTarget
        $ addMakeDeps
        $ addRebuildDeps (makeDeps ms)
        $ recipes ms

    mainRegion = region "MAIN" $ do
      line "# This Makefile was generated by the Cake3"
      line "# https://github.com/grwlf/cake3"
      -- line "V := $(shell $(MAKE) PREBUILD=1)"
      line "GUARD = .GUARD_$(1)_$(shell echo $($(1)) | md5sum | cut -d ' ' -f 1)"
      writeRules rs'
      forM_ (includes ms) $ \i -> do
        line (printf "include %s" (toMakeText i))

    serviceRegion = region "SERVICE" $ do
      line "GUARD = .GUARD_$(1)_$(shell echo $($(1)) | md5sum | cut -d ' ' -f 1)"
      r <- runA_ "<internal>" $ do
        produce (queryTargets (recipes ms))
        commands (prebuilds ms)
        commands [[Left "$(MAKE) MAIN=1 $(MAKECMDGOALS)"]]
        -- shell' [cmd|$(make) MAIN=1 $(makecmdgoals)|]
        commands (postbuilds ms)
        markPhony
      writeRules $ fixMultiTarget [r]

data MakeRegion = MR {
    mrname :: String
  , mrtext :: String
}

runLines :: StateT String (Either String) () -> Either String String
runLines s = let e = runStateT s "" in
  case e of
    Left e -> Left e
    Right ((),st) -> Right st

writeRegions :: [MakeRegion] -> Either String String
writeRegions [] = fail "No regions are defined"
writeRegions (r:rs)
  | L.null rs = return (mrtext r)
  | otherwise = do
    inner <- writeRegions rs
    runLines $ do
      line (printf "ifdef %s" (map toUpper $ mrname r))
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
    line (printf "\trm -f .GUARD_%s_*" (vname v))
    line "\ttouch $@"

