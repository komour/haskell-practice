{-# LANGUAGE NumericUnderscores #-}

module AllTests where

import           Lenses
import           Task1

import           Test.Tasty
import           Test.Tasty.Hspec
import qualified CHT

allTestsTree :: IO TestTree
allTestsTree = testSpec "unit-tests" allTests

-- | Some FS tree to test lenses and stuff.
testDir0 :: FS
testDir0 = Dir {_name = "test", _contents = [File {_name = "file"},Dir {_name = "1", _contents = [File {_name = "file2"},Dir {_name = "777", _contents = [File {_name = "dummyFile"},File {_name = "kekFile"}]},File {_name = "file1"},Dir {_name = "4", _contents = [File {_name = "someFile"},Dir {_name = "5", _contents = [File {_name = "anotherFile"},Dir {_name = "7", _contents = []},Dir {_name = "6", _contents = []}]}]}]},Dir {_name = "3", _contents = [File {_name = "3file3"}]},Dir {_name = "2", _contents = []}]}

-- | Some FS tree which is smaller than `testDir0`.
testDir1 :: FS
testDir1 = Dir {_name = "test", _contents = [File {_name = "file"}, File {_name = "file2"}, File {_name = "file3"}]}

-- | Similar to `testDir1` FS tree but files have extension.
testDir2 :: FS
testDir2 = Dir {_name = "test", _contents = [File {_name = "file.hs"}, File {_name = "file2.cpp"}, File {_name = "file3.swift"}]}

testDir3 :: FS
testDir3 = Dir {_name = "kk", _contents = [Dir {_name = "2", _contents = [] }]}

allTests :: Spec
allTests = do
  describe "Task1" $ do
    it "perimeter" $ do
      perimeter [Point 0 0, Point 1 0, Point 1 1, Point 0 1] `shouldBe` 4
      perimeter [Point 0 0, Point (-2) 0, Point (-2) (-2), Point 0 (-2)] `shouldBe` 8
      perimeter [Point (-1) (-1), Point (-3) (-1), Point (-3) (-3), Point (-1) (-3)] `shouldBe` 8
    it "doubleArea" $ do
      doubleArea [Point 0 0, Point 1 0, Point 1 1, Point 0 1] `shouldBe` 2
      doubleArea [Point 0 0, Point (-2) 0, Point (-2) (-2), Point 0 (-2)] `shouldBe` 8
      perimeter [Point (-1) (-1), Point (-3) (-1), Point (-3) (-3), Point (-1) (-3)] `shouldBe` 8
  describe "Task5" $
    it "lenses and prisms" $ do
      File { _name = "kkkkk" } ^? _File `shouldBe` Just File { _name = "kkkkk" }
      File { _name = "x)" } ^? _Dir `shouldBe` Nothing
      Dir { _name = "zulul", _contents = []} ^. contents `shouldBe` []
      File { _name = "x)" } ^. contents `shouldBe` []
  describe "Task6 (cd, file, ls)" $ do
    it "existing directory && existing files" $ do
      testDir0 ^? cd "1" . cd "777" . file "dummyFile" `shouldBe` Just "dummyFile"
      testDir0 ^.. cd "1" . cd "777" . ls `shouldBe` ["dummyFile","kekFile"]
      testDir0 ^? cd "1" . cd "4" . cd "5" `shouldBe` (Just $ Dir {_name = "5", _contents = [File {_name = "anotherFile"},Dir {_name = "7", _contents = []},Dir {_name = "6", _contents = []}]})
    it "!existing directory || !existing files" $ do
      testDir0 ^? cd "1" . cd "777" . file "notExistingFile" `shouldBe` Nothing
      testDir0 ^? cd "1" . cd "777" . cd "notExist" `shouldBe` Nothing
      testDir0 ^.. cd "2" . cd "notExist" . ls `shouldBe` []
  describe "Task7" $ do
    it "set new extension" $ do
      setExtension ".hs" testDir1 `shouldBe` Dir {_name = "test", _contents = [File {_name = "file.hs"}, File {_name = "file2.hs"}, File {_name = "file3.hs"}]}
      setExtension ".kjk" testDir2 `shouldBe` Dir {_name = "test", _contents = [File {_name = "file.kjk"}, File {_name = "file2.kjk"}, File {_name = "file3.kjk"}]}
    it "collect all names" $ do
      collectNames testDir0 `shouldBe` ["test","file","1","3","2","file2","777","file1","4","dummyFile","kekFile","someFile","5","anotherFile","7","6","3file3"]
      collectNames testDir2 `shouldBe` ["test","file.hs","file2.cpp","file3.swift"]
    it "remove empty directory" $ do
      rmEmptyDir "1" testDir0 `shouldBe` testDir0
      rmEmptyDir "notExist" testDir0 `shouldBe` testDir0
      rmEmptyDir "2" testDir3 `shouldBe` over contents (const []) testDir3
  describe "HashTable" $ do
    it "insert+get+size" $ do
      ht <- CHT.newCHT
      CHT.putCHT "ke" (1337 :: Int) ht
      size <- CHT.sizeCHT ht
      size `shouldBe` 1
      CHT.putCHT "ku" 69 ht
      secondValue <- CHT.getCHT "ku" ht
      secondValue `shouldBe` Just 69