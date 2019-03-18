{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import ApproveApi.Model
import ApproveApi.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 5) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy AnswerMetadata)
      propMimeEq MimeJSON (Proxy :: Proxy CreatePromptRequest)
      propMimeEq MimeJSON (Proxy :: Proxy Error)
      propMimeEq MimeJSON (Proxy :: Proxy Prompt)
      propMimeEq MimeJSON (Proxy :: Proxy PromptAnswer)
      propMimeEq MimeJSON (Proxy :: Proxy PromptMetadata)
      propMimeEq MimeJSON (Proxy :: Proxy PromptStatus)
      
