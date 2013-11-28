{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Development.Cake3.Monad where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Loc
import Data.Data
import Data.Typeable
import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import Data.Map(Map)
import qualified Data.Set as S
import Data.Set(Set)
import qualified Data.String as STR
import Data.List as L hiding (foldl')
import Data.Either
import Data.Foldable (Foldable(..), foldl')
import qualified Data.ByteString.Char8 as BS
import qualified Data.Foldable as F
import qualified Data.Traversable as F
import qualified Data.Text as T
import Development.Cake3.Types
import qualified System.IO as IO
import Text.Printf
import Text.QuasiMake

import Language.Haskell.TH.Quote
import Language.Haskell.TH hiding(unsafe)
import Language.Haskell.Meta (parseExp)

import System.FilePath.Wrapper


type Location = String

data MakeState = MS {
    prebuilds :: Recipe
  , postbuilds :: Recipe
  , recipes :: Set Recipe
  , sloc :: Location
  , makeDeps :: Set File
  , placement :: [File]
  , includes :: Set File
  , errors :: String
  , warnings :: String
  }

-- Oh, such a boilerplate
initialMakeState = MS defr defr mempty mempty mempty mempty mempty mempty mempty where
  defr = emptyRecipe "<internal>"

getPlacementPos :: Make Int
getPlacementPos = L.length <$> placement <$> get

addPlacement :: Int -> File -> Make ()
addPlacement pos r = modify $ \ms -> ms { placement = r`insertInto`(placement ms) } where
  insertInto x xs = let (h,t) = splitAt pos xs in h ++ (x:t)

-- State that target Makefile depends on a File f.
addMakeDep :: File -> Make ()
addMakeDep f = modify (\ms -> ms { makeDeps = S.insert f (makeDeps ms) })

prebuild, postbuild :: (MonadMake m) => CommandGen -> m ()
prebuild cmdg = liftMake $ do
  s <- get
  pb <- fst <$> runA' (prebuilds s) (shell cmdg)
  put s { prebuilds = pb }
postbuild cmdg = liftMake $ do
  s <- get
  pb <- fst <$> runA' (postbuilds s) (shell cmdg)
  put s { postbuilds = pb }

-- State that subproject p (a directory with it's own Makefile) manages file f
-- addSubproject :: File -> [Command] -> Make ()
-- addSubproject f cmds = modify (\ms -> ms { subprojects = M.insert f cmds (subprojects ms) })

checkForEmptyTarget :: MakeState -> String
checkForEmptyTarget ms = foldl' checker mempty (recipes ms) where
  checker es r | S.null (rtgt r) = es++e
               | otherwise = es where
    e = printf "Error: Recipe without targets\n\t%s\n" (show r)

checkForTargetConflicts :: MakeState -> String
checkForTargetConflicts ms = foldl' checker mempty (groupRecipes (recipes ms)) where
  checker es rs | S.size rs > 1 = es++e
                | otherwise = es where
    e = printf "Error: Recipes share one or more targets\n\t%s\n" (show rs)


class (MonadIO m) => MonadMake m where
  liftMake :: Make a -> m a

newtype Make a = Make { unMake :: (StateT MakeState IO) a }
  deriving(Monad, Functor, Applicative, MonadState MakeState, MonadIO, MonadFix)

instance MonadMake Make where
  liftMake = id

instance (MonadMake m) => MonadMake (A' m) where
  liftMake m = A' (lift (liftMake m))

instance (MonadMake m) => MonadMake (StateT s m) where
  liftMake = lift . liftMake


-- | Returns either error or a tuple containing: 1) the set of all variables 2) the set of recipes
-- 3) the user-defined order of targets in the final Makefile
evalMake :: Make a -> IO MakeState
evalMake mk = do
  ms <- flip execStateT initialMakeState (unMake mk)
  return ms {
    errors = checkForEmptyTarget ms ++ checkForTargetConflicts ms
  }

modifyLoc f = modify $ \ms -> ms { sloc = f (sloc ms) }

addRecipe :: Recipe -> Make ()
addRecipe r = modify $ \ms -> 
  let rs = recipes ms ; k = rtgt r
  in ms { recipes = (S.insert r (recipes ms)) }

getLoc :: Make String
getLoc = sloc <$> get

includeMakefile :: (Foldable t) => t File -> Make ()
includeMakefile fs = foldl' scan (return ()) fs where
  scan a f = do
    modify $ \ms -> ms {includes = S.insert f (includes ms)}
    return ()

instance MonadLoc Make where
  withLoc l' (Make um) = Make $ do
    modifyLoc (\l -> l') >> um

newtype A' m a = A' { unA' :: StateT Recipe m a }
  deriving(Monad, Functor, Applicative, MonadState Recipe, MonadIO,MonadFix)

type A a = A' Make a

class (Monad m, Monad t) => MonadAction t m | t -> m where
  liftAction :: A' m x -> t x

instance (Monad m) => MonadAction (A' m) m where
  liftAction = id

runA' :: (Monad m) => Recipe -> A' m a -> m (Recipe, a)
runA' r act = do
  (a,r) <- runStateT (unA' act) r
  return (r,a)

runA :: (Monad m) => String -> A' m a -> m (Recipe, a)
runA loc act = runA' (emptyRecipe loc) act

runA_ :: (Monad m) => String -> A' m a -> m Recipe
runA_ loc act = runA loc act >>= return .fst

addVariable :: (Monad m) => Variable -> A' m ()
addVariable v = modify $ \r -> r { rvars = S.insert v (rvars r) }

targets :: (Applicative m, Monad m) => A' m (Set File)
targets = rtgt <$> get

prerequisites :: (Applicative m, Monad m) => A' m (Set File)
prerequisites = rsrc <$> get

markPhony :: (Monad m) => A' m ()
markPhony = modify $ \r -> r { rflags = S.insert Phony (rflags r) }

markIntermediate :: (Monad m) => A' m ()
markIntermediate = modify $ \r -> r { rflags = S.insert Intermediate (rflags r) }

readFile :: File -> A String
readFile f = do
  A' (lift $ addMakeDep f)
  liftIO (IO.readFile (toFilePath f))

readFileForMake :: (MonadMake m) => File -> m BS.ByteString
readFileForMake f = liftMake (addMakeDep f >> liftIO (BS.readFile (toFilePath f)))

-- | CommandGen is a recipe packed in the newtype to prevent partial expantion
newtype CommandGen' m = CommandGen' { unCommand :: A' m Command }
type CommandGen = CommandGen' Make

commandGen :: A Command -> CommandGen
commandGen mcmd = CommandGen' mcmd

addCommands :: (Monad m) => [Command] -> A' m ()
addCommands lines = modify (\r -> r { rcmd = (rcmd r) ++ lines })

ignoreDepends :: (Monad m) => A' m a -> A' m a
ignoreDepends action = do
  r <- get
  a <- action
  modify $ \r' -> r' { rsrc = rsrc r, rvars = rvars r }
  return a

shell :: (Monad m) => CommandGen' m -> A' m [File]
shell cmdg = do
  line <- unCommand cmdg
  addCommands [line]
  r <- get
  return (S.toList (rtgt r))

unsafeShell :: (Monad m) => CommandGen' m -> A' m [File]
unsafeShell cmdg = ignoreDepends (shell cmdg)

newtype Reference = Reference String

class ReferenceLike a where
  string :: a -> Reference

instance ReferenceLike String where
  string s = Reference s

instance ReferenceLike File where
  string f = Reference (toFilePath f)


class (Monad m) => RefMerge m x where
  refMerge :: x -> A' m Command

-- instance (Monad m) => RefMerge m Variable where
--   refMerge v@(Variable n _) = do
--     addVariable v
--     return_text $ printf "$(%s)" n

refMergeList xs = spacify $ mapM refMerge xs

-- instance (Monad m) => RefMerge m [File] where
--   refMerge xs = spacify $ map refMerge xs

-- instance (Monad m) => RefMerge m [String] where
--   refMerge xs = flap $ map refMerge xs

instance RefMerge m x => RefMerge m (Set x) where
  refMerge xs = refMergeList (S.toList xs)

instance (Monad m) => RefMerge m (CommandGen' m) where
  refMerge cg = unCommand cg

-- instance (Monad m) => RefMerge m Command where
--   refMerge = return

-- instance (Monad m) => RefMerge m [Command] where
--   refMerge = refMergeList


class (Monad m) => RefOutput m x where
  refOutput :: x -> A' m Command

instance (Monad m) => RefOutput m File where
  refOutput f = do
    modify $ \r -> r { rtgt = f `S.insert` (rtgt r)}
    return_file f

-- instance RefOutput m x => RefOutput m [x] where
--   refOutput xs = concat `liftM` mapM refOutput xs

-- FIXME: inbetween will not notice if spaces are already exists
inbetween x mx = (concat`liftM`mx) >>= \l -> return (inbetween' x l) where
  inbetween' x [] = []
  inbetween' x [a] = [a]
  inbetween' x (a:as) = a:x:(inbetween' x as)

spacify l = (CmdStr " ") `inbetween` l

instance (Monad m) => RefOutput m [File] where
  refOutput xs = spacify $ mapM refOutput (xs)

instance (Monad m) => RefOutput m (Set File) where
  refOutput xs = refOutput (S.toList xs)

instance (RefOutput m x) => RefOutput m (Maybe x) where
  refOutput mx = case mx of
    Nothing -> return mempty
    Just x -> refOutput x

-- | Data structure x may be referenced from within the command. Referal
-- class specifies side effects of such referencing. For example, referencig the
-- file leads to adding it to the prerequisites list.
class (MonadAction a m) => RefInput a m x where
  refInput :: x -> a Command

instance (MonadAction a m) => RefInput a m File where
  refInput f = liftAction $ do
    modify $ \r -> r { rsrc = f `S.insert` (rsrc r)}
    return_file f

instance (MonadAction a m) => RefInput a m Recipe where
  refInput r = refInput (rtgt r)

-- instance RefInput m x => RefInput m [x] where
--   refInput xs = concat `liftM` mapM refInput xs

instance (RefInput a m x) => RefInput a m [x] where
  refInput xs = spacify $ mapM refInput xs

instance (MonadAction a m) => RefInput a m (Set File) where
  refInput xs = refInput (S.toList xs)

instance (MonadIO a, RefInput a m x) => RefInput a m (IO x) where
  refInput mx = liftIO mx >>= refInput

instance (MonadAction a m, MonadMake a) => RefInput a m (Make Recipe) where
  refInput mr = liftMake mr >>= refInput

instance (RefInput a m x) => RefInput a m (Maybe x) where
  refInput mx = case mx of
    Nothing -> return mempty
    Just x -> refInput x

instance (MonadAction a m) => RefInput a m Variable where
  refInput v@(Variable n _) = liftAction $ do
    addVariable v
    return_text $ printf "$(%s)" n

instance (MonadAction a m) => RefInput a m Reference where
  refInput v@(Reference s) = do
    return_text s

depend :: (RefInput a m x) => x -> a ()
depend x = refInput x >> return ()

produce :: (RefOutput m x) => x -> A' m ()
produce x = refOutput x >> return ()

merge :: (RefMerge m x) => x -> A' m ()
merge x = refMerge x >> return ()

variables :: (Monad m) => (Set Variable) -> A' m ()
variables vs = modify (\r -> r { rvars = (rvars r) `mappend` vs } )

commands :: (Monad m) => [Command] -> A' m ()
commands cmds = modify (\r -> r { rcmd = (rcmd r) ++ cmds } )

location :: (Monad m) => String -> A' m ()
location l  = modify (\r -> r { rloc = l } )

flags :: (Monad m) => Set Flag -> A' m ()
flags f = modify (\r -> r { rflags = (rflags r) `mappend` f } )

-- | Has effect of a function :: QQ -> CommandGen where QQ is a string supporting
-- $VARs. Each $VAR will be dereferenced using Ref typeclass. Result will
-- be equivalent to
--
-- return Command $ do
--   s1 <- refInput "gcc "
--   s2 <- refInput (flags :: Variable)
--   s3 <- refInput " "
--   s4 <- refInput (file :: File)
--   return (s1 ++ s2 ++ s3)
--
-- Later, this command may be examined or passed to the shell function to apply
-- it to the recepi
--
cmd :: QuasiQuoter
cmd = QuasiQuoter
  { quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  , quoteExp = \s -> appE [| \x -> CommandGen' x |] (qqact s)
  } where
    qqact s = 
      let chunks = flip map (getChunks (STR.fromString s)) $ \c ->
                     case c of
                       T t -> let t' = T.unpack t in [| return_text t' |]
                       E c t -> case parseExp (T.unpack t) of
                                  Left  e -> error e
                                  Right e -> case c of
                                    '$' -> appE [| refInput |] (return e)
                                    '@' -> appE [| refOutput |] (return e)
                                    -- _   -> appE [| refMerge |] (return e)
      in appE [| \l -> L.concat <$> (sequence l) |] (listE chunks)

