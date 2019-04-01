{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Instances where

import ApproveApi.Model
import ApproveApi.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

-- * Models
 
instance Arbitrary AnswerMetadata where
  arbitrary =
    AnswerMetadata
      <$> arbitrary -- answerMetadataIpAddress :: Maybe Text
      <*> arbitrary -- answerMetadataBrowser :: Maybe Text
      <*> arbitrary -- answerMetadataOperatingSystem :: Maybe Text
    
instance Arbitrary CreatePromptRequest where
  arbitrary =
    CreatePromptRequest
      <$> arbitrary -- createPromptRequestUser :: Text
      <*> arbitrary -- createPromptRequestBody :: Text
      <*> arbitrary -- createPromptRequestTitle :: Maybe Text
      <*> arbitrary -- createPromptRequestApproveText :: Maybe Text
      <*> arbitrary -- createPromptRequestApproveRedirectUrl :: Maybe Text
      <*> arbitrary -- createPromptRequestRejectText :: Maybe Text
      <*> arbitrary -- createPromptRequestRejectRedirectUrl :: Maybe Text
      <*> arbitrary -- createPromptRequestLongPoll :: Maybe Bool
      <*> arbitrary -- createPromptRequestExpiresIn :: Maybe Double
      <*> arbitrary -- createPromptRequestMetadata :: Maybe PromptMetadata
      <*> arbitrary -- createPromptRequestInternalData :: Maybe (Map.Map String Text)
      <*> arbitrary -- createPromptRequestIdempotencyKey :: Maybe Text
    
instance Arbitrary Error where
  arbitrary =
    Error
      <$> arbitrary -- errorError :: Text
    
instance Arbitrary Prompt where
  arbitrary =
    Prompt
      <$> arbitrary -- promptId :: Text
      <*> arbitrary -- promptSentAt :: Double
      <*> arbitrary -- promptIsExpired :: Bool
      <*> arbitrary -- promptRequest :: CreatePromptRequest
      <*> arbitrary -- promptAnswer :: Maybe PromptAnswer
      <*> arbitrary -- promptMetadata :: Maybe PromptMetadata
    
instance Arbitrary PromptAnswer where
  arbitrary =
    PromptAnswer
      <$> arbitrary -- promptAnswerResult :: Bool
      <*> arbitrary -- promptAnswerTime :: Double
      <*> arbitrary -- promptAnswerMetadata :: Maybe AnswerMetadata
    
instance Arbitrary PromptMetadata where
  arbitrary =
    PromptMetadata
      <$> arbitrary -- promptMetadataLocation :: Maybe Text
      <*> arbitrary -- promptMetadataTime :: Maybe Text
      <*> arbitrary -- promptMetadataIpAddress :: Maybe Text
      <*> arbitrary -- promptMetadataBrowser :: Maybe Text
      <*> arbitrary -- promptMetadataOperatingSystem :: Maybe Text
    
instance Arbitrary PromptStatus where
  arbitrary =
    PromptStatus
      <$> arbitrary -- promptStatusIsAnswered :: Bool
      <*> arbitrary -- promptStatusIsExpired :: Bool
    


