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

-- | MakeState describes the state of the EDSL synthesizers during the
-- the program execution.
data MakeState = MS {
    prebuilds :: Recipe
    -- ^ Prebuild commands. targets/prerequsites of the recipe are ignored,
    -- commands are executed before any target
  , postbuilds :: Recipe
    -- ^ Postbuild commands.
  , recipes :: Set Recipe
    -- ^ The set of recipes
  , sloc :: Location
    -- ^ Current location. FIXME: fix or remove
  , makeDeps :: Set File
    -- ^ Set of files which the Makefile depends on
  , placement :: [File]
    -- ^ Placement list is the order of targets to be placed in the output file
  , includes :: Set File
    -- ^ Set of files to include in the output file (Makefile specific thing)
  , errors :: String
    -- ^ Errors found so far
  , warnings :: String
    -- ^ Warnings found so far
  , outputFile :: File
  }

-- Oh, such a boilerplate
initialMakeState mf = MS defr defr mempty mempty mempty mempty mempty mempty mempty mf where
  defr = emptyRecipe "<internal>"

getPlacementPos :: Make Int
getPlacementPos = L.length <$> placement <$> get

addPlacement :: Int -> File -> Make ()
addPlacement pos r = modify $ \ms -> ms { placement = r`insertInto`(placement ms) } where
  insertInto x xs = let (h,t) = splitAt pos xs in h ++ (x:t)

addMakeDep :: File -> Make ()
addMakeDep f = modify (\ms -> ms { makeDeps = S.insert f (makeDeps ms) })

-- | Add prebuild command
prebuild, postbuild :: (MonadMake m) => CommandGen -> m ()
prebuild cmdg = liftMake $ do
  s <- get
  pb <- fst <$> runA' (prebuilds s) (shell cmdg)
  put s { prebuilds = pb }
postbuild cmdg = liftMake $ do
  s <- get
  pb <- fst <$> runA' (postbuilds s) (shell cmdg)
  put s { postbuilds = pb }

-- | Find recipes without targets
checkForEmptyTarget :: (Foldable f) => f Recipe -> String
checkForEmptyTarget rs = foldl' checker mempty rs where
  checker es r | S.null (rtgt r) = es++e
               | otherwise = es where
    e = printf "Error: Recipe without targets\n\t%s\n" (show r)

-- | Find recipes sharing a target
checkForTargetConflicts :: (Foldable f) => f Recipe -> String
checkForTargetConflicts rs = foldl' checker mempty (groupRecipes rs) where
  checker es rs | S.size rs > 1 = es++e
                | otherwise = es where
    e = printf "Error: Recipes share one or more targets\n\t%s\n" (show rs)


-- | A Monad providing access to MakeState. TODO: not mention IO here.
class (Monad m) => MonadMake m where
  liftMake :: (Make' IO) a -> m a

newtype Make' m a = Make { unMake :: (StateT MakeState m) a }
  deriving(Monad, Functor, Applicative, MonadState MakeState, MonadIO, MonadFix)

type Make a = Make' IO a

instance MonadMake (Make' IO) where
  liftMake = id

instance (MonadMake m) => MonadMake (A' m) where
  liftMake m = A' (lift (liftMake m))

instance (MonadMake m) => MonadMake (StateT s m) where
  liftMake = lift . liftMake


-- | Returns a MakeState
evalMake :: (Monad m) => File -> Make' m a -> m MakeState
evalMake mf mk = do
  ms <- flip execStateT (initialMakeState mf) (unMake mk)
  return ms {
    errors = checkForEmptyTarget (recipes ms) ++ checkForTargetConflicts (recipes ms)
  }

modifyLoc f = modify $ \ms -> ms { sloc = f (sloc ms) }

addRecipe :: Recipe -> Make ()
addRecipe r = modify $ \ms -> 
  let rs = recipes ms ; k = rtgt r
  in ms { recipes = (S.insert r (recipes ms)) }

getLoc :: Make String
getLoc = sloc <$> get

-- | Add 'include ...' directive to the final Makefile for each input file.
includeMakefile :: (Foldable t) => t File -> Make ()
includeMakefile fs = foldl' scan (return ()) fs where
  scan a f = do
    modify $ \ms -> ms {includes = S.insert f (includes ms)}
    return ()

instance (Monad m) => MonadLoc (Make' m) where
  withLoc l' (Make um) = Make $ do
    modifyLoc (\l -> l') >> um

-- | 'A' here stands for Action. It is a State monad carrying a Recipe as its
-- state.  Various monadic actions add targets, prerequisites and shell commands
-- to this recipe. After that, @rule@ function records it to the @MakeState@.
-- After the recording, no modification is allowed for this recipe.
newtype A' m a = A' { unA' :: StateT Recipe m a }
  deriving(Monad, Functor, Applicative, MonadState Recipe, MonadIO,MonadFix)

-- | Verison of Action monad with fixed parents
type A a = A' (Make' IO) a

-- | A class of monads providing access to the underlying A monad
class (Monad m, Monad t) => MonadAction t m | t -> m where
  liftAction :: A' m x -> t x

instance (Monad m) => MonadAction (A' m) m where
  liftAction = id

-- | Run the Action monad, using already existing Recipe as input.
runA' :: (Monad m) => Recipe -> A' m a -> m (Recipe, a)
runA' r act = do
  (a,r) <- runStateT (unA' act) r
  return (r,a)

-- | Create new empty recipe and run action on it.
runA :: (Monad m)
  => String -- ^ Location string (in the Cakefile.hs)
  -> A' m a -- ^ Recipe builder
  -> m (Recipe, a)
runA loc act = runA' (emptyRecipe loc) act

-- | Version of runA discarding the result of A's computation
runA_ :: (Monad m) => String -> A' m a -> m Recipe
runA_ loc act = runA loc act >>= return .fst

-- | Get a list of targets added so far
targets :: (Applicative m, Monad m) => A' m (Set File)
targets = rtgt <$> get

-- | Get a list of prerequisites added so far
prerequisites :: (Applicative m, Monad m) => A' m (Set File)
prerequisites = rsrc <$> get

-- | Mark the recipe as 'PHONY' i.e. claim that all it's targets are not real
-- files. Makefile-specific.
markPhony :: (Monad m) => A' m ()
markPhony = modify $ \r -> r { rflags = S.insert Phony (rflags r) }

-- | Mark the recipe as 'INTERMEDIATE' i.e. claim that all it's targets may be
-- removed after the build process. Makefile-specific.
markIntermediate :: (Monad m) => A' m ()
markIntermediate = modify $ \r -> r { rflags = S.insert Intermediate (rflags r) }

-- | Obtain the contents of a File. Note, that this generally means, that
-- Makefile should be regenerated each time the File is changed.
readFileForMake :: (MonadMake m)
  => File -- ^ File to read contents of
  -> m BS.ByteString
readFileForMake f = liftMake (addMakeDep f >> liftIO (BS.readFile (toFilePath f)))

-- | CommandGen is a recipe-builder packed in the newtype to prevent partial
-- expantion of it's commands
newtype CommandGen' m = CommandGen' { unCommand :: A' m Command }
type CommandGen = CommandGen' (Make' IO)

-- | Pack the command builder into a CommandGen
commandGen :: A Command -> CommandGen
commandGen mcmd = CommandGen' mcmd

-- | Modifie the recipe builder: ignore all the dependencies
ignoreDepends :: (Monad m) => A' m a -> A' m a
ignoreDepends action = do
  r <- get
  a <- action
  modify $ \r' -> r' { rsrc = rsrc r, rvars = rvars r }
  return a

-- | Apply the recipe builder to the current recipe state. Return the list of
-- targets of the current @Recipe@ under construction
shell :: (Monad m)
  => CommandGen' m -- ^ Command builder as returned by cmd quasi-quoter
  -> A' m [File]
shell cmdg = do
  line <- unCommand cmdg
  commands [line]
  r <- get
  return (S.toList (rtgt r))

-- | Version of @shell@ which doesn't track it's dependencies
unsafeShell :: (Monad m) => CommandGen' m -> A' m [File]
unsafeShell cmdg = ignoreDepends (shell cmdg)

-- | Simple wrapper for strings, a target for various typeclass instances.
newtype CakeString = CakeString String
  deriving(Show,Eq,Ord)

-- | An alias to CakeString constructor
string :: String -> CakeString
string = CakeString

-- | Class of things which may be referenced using '\@(expr)' syntax of the
-- quasi-quoted shell expressions.
class (Monad m) => RefOutput m x where
  -- | Register the output item, return it's shell-command representation. Files
  -- are rendered using space protection quotation, variables are wrapped into
  -- $(VAR) syntax, item lists are converted into space-separated lists.
  refOutput :: x -> A' m Command

instance (Monad m) => RefOutput m File where
  refOutput f = do
    modify $ \r -> r { rtgt = f `S.insert` (rtgt r)}
    return_file f

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

-- | Class of things which may be referenced using '\$(expr)' from inside
-- of quasy-quoted shell expressions
class (MonadAction a m) => RefInput a m x where
  -- | Register the input item, return it's shell-script representation
  refInput :: x -> a Command

instance (MonadAction a m) => RefInput a m File where
  refInput f = liftAction $ do
    modify $ \r -> r { rsrc = f `S.insert` (rsrc r)}
    return_file f

instance (MonadAction a m) => RefInput a m Recipe where
  refInput r = refInput (rtgt r)

instance (RefInput a m x) => RefInput a m [x] where
  refInput xs = spacify $ mapM refInput xs

instance (MonadAction a m) => RefInput a m (Set File) where
  refInput xs = refInput (S.toList xs)

instance (MonadIO a, RefInput a m x) => RefInput a m (IO x) where
  refInput mx = liftIO mx >>= refInput

instance (MonadAction a m, MonadMake a) => RefInput a m (Make Recipe) where
  refInput mr = liftMake mr >>= refInput

instance (RefInput a m x, MonadMake a) => RefInput a m (Make x) where
  refInput mx = liftMake mx >>= refInput

instance (RefInput a m x) => RefInput a m (Maybe x) where
  refInput mx = case mx of
    Nothing -> return mempty
    Just x -> refInput x

instance (MonadAction a m) => RefInput a m Variable where
  refInput v@(Variable n _) = liftAction $ do
    variables [v]
    return_text $ printf "$(%s)" n

instance (MonadAction a m) => RefInput a m CakeString where
  refInput v@(CakeString s) = do
    return_text s

instance (MonadAction a m) => RefInput a m (CommandGen' m) where
  refInput (CommandGen' a) = liftAction a

-- | Add it's argument to the list of dependencies (prerequsites) of a current
-- recipe under construction
depend :: (RefInput a m x)
  => x -- ^ File or [File] or (Set File) or other form of dependency.
  -> a ()
depend x = refInput x >> return ()

-- | Declare that current recipe produces some producable item.
produce :: (RefOutput m x)
  => x -- ^ File or [File] or other form of target.
  -> A' m ()
produce x = refOutput x >> return ()

-- | Add variables to the list of variables referenced by the current recipe
variables :: (Foldable t, Monad m)
  => (t Variable) -- ^ A set of variables to depend the recipe on
  -> A' m ()
variables vs = modify (\r -> r { rvars = foldl' (\a v -> S.insert v a) (rvars r) vs } )

-- | Add commands to the list of commands of a current recipe under
-- construction. Warning: this function behaves like unsafeShell i.e. it doesn't
-- analyze the command text
commands :: (Monad m) => [Command] -> A' m ()
commands cmds = modify (\r -> r { rcmd = (rcmd r) ++ cmds } )

-- | Set the recipe's location in the Cakefile.hs
location :: (Monad m) => String -> A' m ()
location l  = modify (\r -> r { rloc = l } )

-- | Set additional flags
flags :: (Monad m) => Set Flag -> A' m ()
flags f = modify (\r -> r { rflags = (rflags r) `mappend` f } )

-- | Has effect of a function @QQ -> CommandGen@ where QQ is a string supporting
-- the following syntax:
--
-- * $(expr) evaluates to expr and adds it to the list of dependencies (prerequsites)
--
-- * \@(expr) evaluates to expr and adds it to the list of targets
--
-- * $$ and \@\@ evaluates to $ and \@
--
-- /Example/
--
-- > [cmd|gcc $flags -o @file|]
--
-- is equivalent to
--
-- >   return $ CommandGen $ do
-- >     s1 <- refInput "gcc "
-- >     s2 <- refInput (flags :: Variable)
-- >     s3 <- refInput " -o "
-- >     s4 <- refOutput (file :: File)
-- >     return (s1 ++ s2 ++ s3 ++ s4)
--
-- Later, this command may be examined or passed to the shell function to apply
-- it to the recipe
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
      in appE [| \l -> L.concat <$> (sequence l) |] (listE chunks)

