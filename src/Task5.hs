module Task5
    ( getDirectory
    , name
    , contents
    ) where

import           Lens.Micro
--import           Lens.Micro.Extras
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
name = lens _name (\file fileName -> file { _name = fileName })

contents :: Lens' FS [FS]
contents = lens _contents (\dir dirContents -> dir { _contents = dirContents})

_File :: Traversal' FS FilePath
_File f (File a)  = File <$> f a
_File _ (Dir b c) = pure (Dir b c)

--_Dir :: Traversal' FS ???
--_Dir f (Dir a b) = undefined
--_Dir _ (File c)  = pure $ File c
