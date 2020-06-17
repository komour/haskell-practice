{-# LANGUAGE RankNTypes #-}

module Lenses
  ( FS (..)
  , getDirectory
  , name
  , contents
  , _Dir
  , _File
  , rmEmptyDir
  , collectNames
  , setExtension
  , file
  , cd
  , ls
  , over
  , (^..)
  , (^?)
  , (^.)
  ) where

import           Data.List             (isPrefixOf)
import           Lens.Micro
import           System.Directory      (doesDirectoryExist, doesFileExist,
                                        listDirectory)
import           System.FilePath.Posix (splitExtension, takeFileName, (<.>),
                                        (</>))
data FS = Dir
    { _name     :: FilePath
    , _contents :: [FS]
    }
    | File
    { _name :: FilePath
    }
    deriving (Eq)

-- | Helper function to draw a tree taken from `Data.Tree`.
draw :: FS -> [String]
draw (File fn       ) = [fn]
draw (Dir dn content) = lines dn ++ drawSubTrees content
  where
    drawSubTrees []       = []
    drawSubTrees [t     ] = "|" : shift "`- " "   " (draw t)
    drawSubTrees (t : ts) = "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts
    shift first other = zipWith (++) (first : repeat other)

-- | Similar to `drawTree` from `Data.Tree`
instance Show FS where
  show = unlines . draw

-- | Retrieves FS-representation of the given directory and its contents recursively.
-- Doesn't scan invisible directories and files
-- May cause `IOException` and the same exceptions as `listDirectory`
getDirectory :: FilePath -> IO FS
getDirectory path = do
    dirExists <- doesDirectoryExist path
    fileExists <- doesFileExist path
    if dirExists
    then do
      children <- listDirectory path
      let onlyVisible = filter (not . isPrefixOf ".") children
      let pathsList = fmap (path </>) onlyVisible
      scannedChildren <- traverse getDirectory pathsList
      return Dir { _name = takeFileName path, _contents = scannedChildren }
    else if fileExists
      then return File { _name = takeFileName path }
      else fail $ "\"" ++ path ++ "\" does not exist. "

name :: Lens' FS FilePath
name = lens _name (\f fileName -> f { _name = fileName })

-- | Bad lens for our ADT.
pseudoContents :: Lens' FS [FS]
pseudoContents = lens _contents (\d dirContents -> d { _contents = dirContents})

contents :: Traversal' FS [FS]
contents = _Dir . pseudoContents

_File :: Traversal' FS FS
_File f (File a)  = f $ File a
_File _ (Dir b c) = pure (Dir b c)

_Dir :: Traversal' FS FS
_Dir f (Dir a b) = f $ Dir a b
_Dir _ (File c)  = pure $ File c

-- ######### Task 6 ######### --

cd :: FilePath -> Traversal' FS FS
cd path = contents . traversed . _Dir . filtered (\someDir -> someDir ^. name == path)

file :: FilePath -> Traversal' FS FilePath
file path = contents . traversed . _File . filtered (\someFile -> someFile ^. name == path) . name

ls :: Traversal' FS FilePath
ls = contents . traversed . name

-- ######### Task 7 ######### --

-- | Lens for reading and writing to the extension (from hackage).
extension :: Lens' FilePath FilePath
extension f p = (n <.>) <$> f e
 where
  (n, e) = splitExtension p

-- | Set given extension for every file in the given directory.
-- Doesn't change anything in sub-dirs.
setExtension :: FilePath -> FS -> FS
setExtension newExt = contents . traversed . _File . name . extension .~ newExt

-- | Returns list with all names in the given FS tree.
collectNames :: FS -> [FilePath]
collectNames root = rootName : walker root
  where walker fs = fs ^.. ls ++ concatMap walker (fs ^. contents)
        rootName = root ^. name

-- | Remove given dir from the `_contents`.
-- In case of using with (^..) returns `_contents` w/o given dir.
rmDir :: FilePath -> Traversal' FS FS
rmDir dirName = contents . traversed . _Dir . filtered (\someDir -> someDir ^. name /= dirName)

-- | Remove given dir if it's empty.
-- Returns FS.
rmEmptyDir :: FilePath -> FS -> FS
rmEmptyDir nm fs = case fs ^? cd nm of
                     Nothing -> fs
                     Just dir -> if null (dir ^.. ls)
                                 then over contents (const (fs ^.. rmDir nm)) fs
                                 else fs
