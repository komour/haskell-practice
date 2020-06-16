{-# LANGUAGE RankNTypes #-}

module Lenses where
--    ( getDirectory
--    , name
--    , contents
--    ) where

import           Lens.Micro
import           Lens.Micro.Extras
import           System.Directory      (doesDirectoryExist, doesFileExist,
                                        listDirectory)
import           System.FilePath.Posix (takeFileName, (</>))

data FS = Dir
    { _name     :: FilePath
    , _contents :: [FS]
    }
    | File
    { _name :: FilePath
    }
--    deriving Show

-- | Helper function to draw a tree taken from `Data.Tree`
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
      let pathsList = fmap (path </>) children
      scannedChildren <- traverse getDirectory pathsList
      return Dir { _name = takeFileName path, _contents = scannedChildren }
    else if fileExists
     then return File { _name = takeFileName path }
     else fail $ "\"" ++ path ++ "\" does not exist. "

name :: Lens' FS FilePath
name = lens _name (\f fileName -> f { _name = fileName })

contents :: Lens' FS [FS]
contents = lens _contents (\d dirContents -> d { _contents = dirContents})

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

-- | Some FS tree to test lenses and stuff
testDir :: FS
testDir = Dir {_name = "test", _contents = [File {_name = ".DS_Store"},File {_name = "file"},Dir {_name = "1", _contents = [File {_name = ".DS_Store"},File {_name = "file2"},Dir {_name = "777", _contents = [File {_name = "dummyFile"},File {_name = ".DS_Store"},File {_name = "kekFile"}]},File {_name = "file1"},Dir {_name = "4", _contents = [File {_name = ".DS_Store"},File {_name = "someFile"},Dir {_name = "5", _contents = [File {_name = ".DS_Store"},File {_name = "anotherFile"},Dir {_name = "7", _contents = []},Dir {_name = "6", _contents = []}]}]}]},Dir {_name = "3", _contents = [File {_name = "3file3"}]},Dir {_name = "2", _contents = []}]}

cd :: FilePath -> Traversal' FS FS
cd path = _Dir . contents . traversed . filtered (\someDir -> someDir ^. _Dir . name == path)

file :: FilePath -> Traversal' FS FilePath
file path = _Dir . contents . traversed . filtered (\someFile -> someFile ^. _File . name == path) . name

ls :: Traversal' FS FilePath
ls = _Dir . contents . traversed . name

cdTest = testDir ^? cd "1" . file "file"
lsTest = testDir ^.. cd "1" . cd "777" . ls

