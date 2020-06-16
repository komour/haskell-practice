{-# LANGUAGE RankNTypes #-}

module Lenses where
--    ( getDirectory
--    , name
--    , contents
--    ) where

import           Data.List             (foldl', isPrefixOf)
import           Lens.Micro
import           Lens.Micro.Extras
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
--    deriving Show

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

-- | Retrieves a representation of a directory and its contents recursively.
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


test = _File.name .~ "kuk" $ File { _name = "keke" }
test2 = preview _File $ File { _name = "kuka" }
test3 = preview _Dir $ File { _name = "x)" }
test4 = preview _Dir $ Dir { _name = "zulul", _contents = [File { _name = "x)" }, File { _name = "kuka" }]}

-- ######### Task 6 ######### --

-- | Some FS tree to test lenses and stuff.
testDir :: FS
testDir = Dir {_name = "test", _contents = [File {_name = "file"},Dir {_name = "1", _contents = [File {_name = "file2"},Dir {_name = "777", _contents = [File {_name = "dummyFile"},File {_name = "kekFile"}]},File {_name = "file1"},Dir {_name = "4", _contents = [File {_name = "someFile"},Dir {_name = "5", _contents = [File {_name = "anotherFile"},Dir {_name = "7", _contents = []},Dir {_name = "6", _contents = []}]}]}]},Dir {_name = "3", _contents = [File {_name = "3file3"}]},Dir {_name = "2", _contents = []}]}

cd :: FilePath -> Traversal' FS FS
cd path = contents . traversed . _Dir . filtered (\someDir -> someDir ^. name == path)

file :: FilePath -> Traversal' FS FilePath
file path = contents . traversed . _File . filtered (\someFile -> someFile ^. name == path) . name

ls :: Traversal' FS FilePath
ls = contents . traversed . name

cdTest = testDir ^? cd "1" . cd "777" . file "dummyFile"
lsTest = testDir ^.. cd "1" . cd "777" . ls

-- ######### Task 7 ######### --

-- | Lens for reading and writing to the extension (from hackage).
extension :: Lens' FilePath FilePath
extension f p = (n <.>) <$> f e
 where
  (n, e) = splitExtension p

-- | Set new extension for every file in the current directory.
changeExtension :: FilePath -> FS -> FS
changeExtension newExt = contents . traversed . _File . name . extension .~ newExt

-- | Returns list with all names in the given FS tree.
collectNames :: FS -> [FilePath]
collectNames root = (root ^. name) : walker root
  where walker fs = fs ^.. ls ++ concatMap walker (fs ^. contents)

--dir :: FilePath -> Traversal' FS FilePath
--dir dirName = contents . traversed . _Dir . filtered (\someDir -> someDir ^. name == dirName) . name

-- | Remove given dir from the `_contents`.
-- In case of (^..) returns `_contents` w/o given dir.
rmDir :: FilePath -> Traversal' FS FS
rmDir dirName = contents . traversed . _Dir . filtered (\someDir -> someDir ^. name /= dirName)

-- | Remove given dir if it's empty.
-- Returns FS.
rmEmptyDir :: FilePath -> FS -> FS
rmEmptyDir nm fs = case fs ^? cd nm of
                     Just dir -> if dir ^.. ls /= [] then fs else over contents (const (fs ^.. rmDir nm)) fs
                     Nothing -> fs

-- ######### Task 7 HARD ######### --

getPath :: FS -> FilePath -> FilePath
getPath = undefined
