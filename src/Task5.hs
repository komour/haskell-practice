{-# LANGUAGE FlexibleInstances #-}

module Task5
    ( getDirectory
    ) where

import           System.Directory      (doesDirectoryExist, doesFileExist,
                                        listDirectory)
import           System.FilePath.Posix (takeFileName, (</>))
--import Lens.Micro

data FS = Dir
    { name     :: FilePath
    , contents :: [FS]
    }
    | File
    { name :: FilePath
    }

-- | Helper function to draw a tree taken from `Data.Tree`
draw :: FS -> [String]
draw (File nm       ) = [nm]
draw (Dir nm content) = lines nm ++ drawSubTrees content
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
      return Dir { name = takeFileName path, contents = scannedChildren }
    else if fileExists
     then return File { name = takeFileName path }
     else fail $ "\"" ++ path ++ "\" does not exist. "
