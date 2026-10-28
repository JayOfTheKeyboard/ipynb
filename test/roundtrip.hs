{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (unless)
import Data.Aeson (Value (..), eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Ipynb
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import Lens.Micro
import Lens.Micro.Aeson
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Data.Monoid

main :: IO ()
main = do
  let rtdir = "test" </> "rt-files"
  createDirectoryIfMissing False rtdir
  fs <- map (rtdir </>) . filter isIpynb <$> getDirectoryContents rtdir
  defaultMain $ testGroup "tests"
    [ testGroup "round-trip tests" $ map rtTest fs
    , nullCountTests ]

isIpynb :: FilePath -> Bool
isIpynb fp = takeExtension fp == ".ipynb"

rtTest :: FilePath -> TestTree
rtTest fp = testCase fp $ do
  inRaw <- BL.readFile fp
  let format = inRaw ^? key "nbformat" . _Number
  case format of
    Just 4 -> rtTest4 inRaw
    _      -> rtTest3 inRaw

rtTest3 :: BL.ByteString -> IO ()
rtTest3 inRaw = do
  (nb :: Notebook NbV3) <- either error return $ eitherDecode inRaw
  let outRaw = encode nb
  (nb' :: Notebook NbV3) <- either error return $ eitherDecode outRaw
  -- test that (write . read) == id
  assertEqual "write . read != read" nb nb'

rtTest4 :: BL.ByteString -> IO ()
rtTest4 inRaw = do
  (nb :: Notebook NbV4) <- either error return $ eitherDecode inRaw
  let outRaw = encode nb
  (nb' :: Notebook NbV4) <- either error return $ eitherDecode outRaw
  -- test that (write . read) == id
  assertEqual "write . read != read" nb nb'

-- execution_count may be null (v4); prompt_number is left out (v3).
nullCountTests :: TestTree
nullCountTests = testGroup "null execution count"
  [ testCase "v4 execute_result writes null" $ do
      inRaw <- BL.readFile ("test" </> "rt-files" </>
                            "test4execute_result_null.ipynb")
      (nb :: Notebook NbV4) <- either error return $ eitherDecode inRaw
      let out = encode nb ^? key "cells" . nth 0 . key "outputs" . nth 0
      assertEqual "execution_count" (Just Null)
        (out >>= (^? key "execution_count"))
  , testCase "v3 pyout omits prompt_number" $ do
      let inRaw = "{\"output_type\":\"pyout\",\"prompt_number\":null,\"metadata\":{},\"text\":\"2\"}"
      (out :: Output NbV3) <- either error return $ eitherDecode inRaw
      assertEqual "prompt_number" Nothing (encode out ^? key "prompt_number")
      (out' :: Output NbV3) <- either error return $ eitherDecode (encode out)
      assertEqual "write . read != read" out out'
  ]
