module Development.Cake3.Rules.UrWeb (urp) where

import Control.Applicative
import Control.Monad.Trans

import System.FilePath.Wrapper
import Development.Cake3

io = liftIO

urp' :: File -> A ()
urp' f@(File p) = do
  ps <- prerequisites
  let depend' f = when (not (f`elem`ps)) $ depend f
  depend' f
  lines <- (map words) <$> lines <$> io (readFile p)
  forM_ lines $ \l -> do
    let parseline ("library":x:[]) = urp' (File x)
        parseline (x@(c:_):[])
          | c == '$' = return ()
          | otherwise = do
              depend' ((File x) .= "ur")
              depend' ((File x) .= "urs")
        parseline _ = return ()
    parseline l

urp :: File -> A () -> Alias
urp f act = rule ((takeBaseName f) .= "exe") $ act >> urp' f  
