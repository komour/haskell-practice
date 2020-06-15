{-# LANGUAGE FlexibleInstances #-}

module Task5
    ( getDirectory
    )
where

import           System.Directory      (doesDirectoryExist, doesFileExist,
                                        listDirectory)
import           System.FilePath.Posix (takeFileName)
--import Lens.Micro

data FS = Dir
    { name     :: FilePath
    , contents :: [FS]
    }
    | File
    { name :: FilePath
    }

-- |Helper function to draw a tree taken from `Data.Tree`
draw :: FS -> [String]
draw (File nm       ) = [nm]
draw (Dir nm content) = lines nm ++ drawSubTrees content
  where
    drawSubTrees []       = []
    drawSubTrees [t     ] = "|" : shift "`- " "   " (draw t)
    drawSubTrees (t : ts) = "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts
    shift first other = zipWith (++) (first : repeat other)

-- |Similar to `drawTree` from `Data.Tree`
instance Show FS where
  show = unlines . draw

-- |Shiza but practice in instance overlapping
instance {-# OVERLAPPING #-} Show (Either String FS) where
  show (Left err)   = err ++ "\n"
  show (Right tree) = show tree

-- |Retrieves a representation of a directory and its contents recursively.
-- Also contains invisible directories and files
-- Returns either an error (file/dir doesn't exist) or a `FS` tree.
getDirectory :: FilePath -> IO (Either String FS)
getDirectory path = do
    fileExists <- doesFileExist path
    dirExists <- doesDirectoryExist path
    if not (fileExists || dirExists)
    then return $ Left $ "Error: \"" ++ path ++ "\" does not exist. "
    else do
      scannedDirectory <- scanner path
      return $ Right scannedDirectory

-- |Directory scanner without any error handling
scanner :: FilePath -> IO FS
scanner path = do
    isDir <- doesDirectoryExist path
    if isDir
    then do
      children <- listDirectory path
      let pathsList = fmap ((path ++ "/") ++) children
      scannedChildren <- traverse scanner pathsList
      return Dir { name = takeFileName path, contents = scannedChildren }
    else return File { name = takeFileName path }
