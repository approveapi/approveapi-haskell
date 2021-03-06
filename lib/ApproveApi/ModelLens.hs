{-
   ApproveAPISwagger

   The simple API to request a user's approval on anything via email + sms.

   OpenAPI Version: 3.0.0
   ApproveAPISwagger API version: 1.0.1
   Contact: dev@approveapi.com
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : ApproveApi.Lens
-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-matches -fno-warn-unused-binds -fno-warn-unused-imports #-}

module ApproveApi.ModelLens where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Data, Typeable)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Time as TI

import Data.Text (Text)

import Prelude (($), (.),(<$>),(<*>),(=<<),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

import ApproveApi.Model
import ApproveApi.Core


-- * AnswerMetadata

-- | 'answerMetadataIpAddress' Lens
answerMetadataIpAddressL :: Lens_' AnswerMetadata (Maybe Text)
answerMetadataIpAddressL f AnswerMetadata{..} = (\answerMetadataIpAddress -> AnswerMetadata { answerMetadataIpAddress, ..} ) <$> f answerMetadataIpAddress
{-# INLINE answerMetadataIpAddressL #-}

-- | 'answerMetadataBrowser' Lens
answerMetadataBrowserL :: Lens_' AnswerMetadata (Maybe Text)
answerMetadataBrowserL f AnswerMetadata{..} = (\answerMetadataBrowser -> AnswerMetadata { answerMetadataBrowser, ..} ) <$> f answerMetadataBrowser
{-# INLINE answerMetadataBrowserL #-}

-- | 'answerMetadataOperatingSystem' Lens
answerMetadataOperatingSystemL :: Lens_' AnswerMetadata (Maybe Text)
answerMetadataOperatingSystemL f AnswerMetadata{..} = (\answerMetadataOperatingSystem -> AnswerMetadata { answerMetadataOperatingSystem, ..} ) <$> f answerMetadataOperatingSystem
{-# INLINE answerMetadataOperatingSystemL #-}



-- * CreatePromptRequest

-- | 'createPromptRequestUser' Lens
createPromptRequestUserL :: Lens_' CreatePromptRequest (Text)
createPromptRequestUserL f CreatePromptRequest{..} = (\createPromptRequestUser -> CreatePromptRequest { createPromptRequestUser, ..} ) <$> f createPromptRequestUser
{-# INLINE createPromptRequestUserL #-}

-- | 'createPromptRequestBody' Lens
createPromptRequestBodyL :: Lens_' CreatePromptRequest (Text)
createPromptRequestBodyL f CreatePromptRequest{..} = (\createPromptRequestBody -> CreatePromptRequest { createPromptRequestBody, ..} ) <$> f createPromptRequestBody
{-# INLINE createPromptRequestBodyL #-}

-- | 'createPromptRequestTitle' Lens
createPromptRequestTitleL :: Lens_' CreatePromptRequest (Maybe Text)
createPromptRequestTitleL f CreatePromptRequest{..} = (\createPromptRequestTitle -> CreatePromptRequest { createPromptRequestTitle, ..} ) <$> f createPromptRequestTitle
{-# INLINE createPromptRequestTitleL #-}

-- | 'createPromptRequestApproveText' Lens
createPromptRequestApproveTextL :: Lens_' CreatePromptRequest (Maybe Text)
createPromptRequestApproveTextL f CreatePromptRequest{..} = (\createPromptRequestApproveText -> CreatePromptRequest { createPromptRequestApproveText, ..} ) <$> f createPromptRequestApproveText
{-# INLINE createPromptRequestApproveTextL #-}

-- | 'createPromptRequestApproveRedirectUrl' Lens
createPromptRequestApproveRedirectUrlL :: Lens_' CreatePromptRequest (Maybe Text)
createPromptRequestApproveRedirectUrlL f CreatePromptRequest{..} = (\createPromptRequestApproveRedirectUrl -> CreatePromptRequest { createPromptRequestApproveRedirectUrl, ..} ) <$> f createPromptRequestApproveRedirectUrl
{-# INLINE createPromptRequestApproveRedirectUrlL #-}

-- | 'createPromptRequestRejectText' Lens
createPromptRequestRejectTextL :: Lens_' CreatePromptRequest (Maybe Text)
createPromptRequestRejectTextL f CreatePromptRequest{..} = (\createPromptRequestRejectText -> CreatePromptRequest { createPromptRequestRejectText, ..} ) <$> f createPromptRequestRejectText
{-# INLINE createPromptRequestRejectTextL #-}

-- | 'createPromptRequestRejectRedirectUrl' Lens
createPromptRequestRejectRedirectUrlL :: Lens_' CreatePromptRequest (Maybe Text)
createPromptRequestRejectRedirectUrlL f CreatePromptRequest{..} = (\createPromptRequestRejectRedirectUrl -> CreatePromptRequest { createPromptRequestRejectRedirectUrl, ..} ) <$> f createPromptRequestRejectRedirectUrl
{-# INLINE createPromptRequestRejectRedirectUrlL #-}

-- | 'createPromptRequestLongPoll' Lens
createPromptRequestLongPollL :: Lens_' CreatePromptRequest (Maybe Bool)
createPromptRequestLongPollL f CreatePromptRequest{..} = (\createPromptRequestLongPoll -> CreatePromptRequest { createPromptRequestLongPoll, ..} ) <$> f createPromptRequestLongPoll
{-# INLINE createPromptRequestLongPollL #-}

-- | 'createPromptRequestExpiresIn' Lens
createPromptRequestExpiresInL :: Lens_' CreatePromptRequest (Maybe Double)
createPromptRequestExpiresInL f CreatePromptRequest{..} = (\createPromptRequestExpiresIn -> CreatePromptRequest { createPromptRequestExpiresIn, ..} ) <$> f createPromptRequestExpiresIn
{-# INLINE createPromptRequestExpiresInL #-}

-- | 'createPromptRequestMetadata' Lens
createPromptRequestMetadataL :: Lens_' CreatePromptRequest (Maybe PromptMetadata)
createPromptRequestMetadataL f CreatePromptRequest{..} = (\createPromptRequestMetadata -> CreatePromptRequest { createPromptRequestMetadata, ..} ) <$> f createPromptRequestMetadata
{-# INLINE createPromptRequestMetadataL #-}

-- | 'createPromptRequestInternalData' Lens
createPromptRequestInternalDataL :: Lens_' CreatePromptRequest (Maybe (Map.Map String Text))
createPromptRequestInternalDataL f CreatePromptRequest{..} = (\createPromptRequestInternalData -> CreatePromptRequest { createPromptRequestInternalData, ..} ) <$> f createPromptRequestInternalData
{-# INLINE createPromptRequestInternalDataL #-}

-- | 'createPromptRequestIdempotencyKey' Lens
createPromptRequestIdempotencyKeyL :: Lens_' CreatePromptRequest (Maybe Text)
createPromptRequestIdempotencyKeyL f CreatePromptRequest{..} = (\createPromptRequestIdempotencyKey -> CreatePromptRequest { createPromptRequestIdempotencyKey, ..} ) <$> f createPromptRequestIdempotencyKey
{-# INLINE createPromptRequestIdempotencyKeyL #-}



-- * Error

-- | 'errorError' Lens
errorErrorL :: Lens_' Error (Text)
errorErrorL f Error{..} = (\errorError -> Error { errorError, ..} ) <$> f errorError
{-# INLINE errorErrorL #-}



-- * Prompt

-- | 'promptId' Lens
promptIdL :: Lens_' Prompt (Text)
promptIdL f Prompt{..} = (\promptId -> Prompt { promptId, ..} ) <$> f promptId
{-# INLINE promptIdL #-}

-- | 'promptSentAt' Lens
promptSentAtL :: Lens_' Prompt (Double)
promptSentAtL f Prompt{..} = (\promptSentAt -> Prompt { promptSentAt, ..} ) <$> f promptSentAt
{-# INLINE promptSentAtL #-}

-- | 'promptIsExpired' Lens
promptIsExpiredL :: Lens_' Prompt (Bool)
promptIsExpiredL f Prompt{..} = (\promptIsExpired -> Prompt { promptIsExpired, ..} ) <$> f promptIsExpired
{-# INLINE promptIsExpiredL #-}

-- | 'promptRequest' Lens
promptRequestL :: Lens_' Prompt (CreatePromptRequest)
promptRequestL f Prompt{..} = (\promptRequest -> Prompt { promptRequest, ..} ) <$> f promptRequest
{-# INLINE promptRequestL #-}

-- | 'promptAnswer' Lens
promptAnswerL :: Lens_' Prompt (Maybe PromptAnswer)
promptAnswerL f Prompt{..} = (\promptAnswer -> Prompt { promptAnswer, ..} ) <$> f promptAnswer
{-# INLINE promptAnswerL #-}

-- | 'promptMetadata' Lens
promptMetadataL :: Lens_' Prompt (Maybe PromptMetadata)
promptMetadataL f Prompt{..} = (\promptMetadata -> Prompt { promptMetadata, ..} ) <$> f promptMetadata
{-# INLINE promptMetadataL #-}



-- * PromptAnswer

-- | 'promptAnswerResult' Lens
promptAnswerResultL :: Lens_' PromptAnswer (Bool)
promptAnswerResultL f PromptAnswer{..} = (\promptAnswerResult -> PromptAnswer { promptAnswerResult, ..} ) <$> f promptAnswerResult
{-# INLINE promptAnswerResultL #-}

-- | 'promptAnswerTime' Lens
promptAnswerTimeL :: Lens_' PromptAnswer (Double)
promptAnswerTimeL f PromptAnswer{..} = (\promptAnswerTime -> PromptAnswer { promptAnswerTime, ..} ) <$> f promptAnswerTime
{-# INLINE promptAnswerTimeL #-}

-- | 'promptAnswerMetadata' Lens
promptAnswerMetadataL :: Lens_' PromptAnswer (Maybe AnswerMetadata)
promptAnswerMetadataL f PromptAnswer{..} = (\promptAnswerMetadata -> PromptAnswer { promptAnswerMetadata, ..} ) <$> f promptAnswerMetadata
{-# INLINE promptAnswerMetadataL #-}



-- * PromptMetadata

-- | 'promptMetadataLocation' Lens
promptMetadataLocationL :: Lens_' PromptMetadata (Maybe Text)
promptMetadataLocationL f PromptMetadata{..} = (\promptMetadataLocation -> PromptMetadata { promptMetadataLocation, ..} ) <$> f promptMetadataLocation
{-# INLINE promptMetadataLocationL #-}

-- | 'promptMetadataTime' Lens
promptMetadataTimeL :: Lens_' PromptMetadata (Maybe Text)
promptMetadataTimeL f PromptMetadata{..} = (\promptMetadataTime -> PromptMetadata { promptMetadataTime, ..} ) <$> f promptMetadataTime
{-# INLINE promptMetadataTimeL #-}

-- | 'promptMetadataIpAddress' Lens
promptMetadataIpAddressL :: Lens_' PromptMetadata (Maybe Text)
promptMetadataIpAddressL f PromptMetadata{..} = (\promptMetadataIpAddress -> PromptMetadata { promptMetadataIpAddress, ..} ) <$> f promptMetadataIpAddress
{-# INLINE promptMetadataIpAddressL #-}

-- | 'promptMetadataBrowser' Lens
promptMetadataBrowserL :: Lens_' PromptMetadata (Maybe Text)
promptMetadataBrowserL f PromptMetadata{..} = (\promptMetadataBrowser -> PromptMetadata { promptMetadataBrowser, ..} ) <$> f promptMetadataBrowser
{-# INLINE promptMetadataBrowserL #-}

-- | 'promptMetadataOperatingSystem' Lens
promptMetadataOperatingSystemL :: Lens_' PromptMetadata (Maybe Text)
promptMetadataOperatingSystemL f PromptMetadata{..} = (\promptMetadataOperatingSystem -> PromptMetadata { promptMetadataOperatingSystem, ..} ) <$> f promptMetadataOperatingSystem
{-# INLINE promptMetadataOperatingSystemL #-}



-- * PromptStatus

-- | 'promptStatusIsAnswered' Lens
promptStatusIsAnsweredL :: Lens_' PromptStatus (Bool)
promptStatusIsAnsweredL f PromptStatus{..} = (\promptStatusIsAnswered -> PromptStatus { promptStatusIsAnswered, ..} ) <$> f promptStatusIsAnswered
{-# INLINE promptStatusIsAnsweredL #-}

-- | 'promptStatusIsExpired' Lens
promptStatusIsExpiredL :: Lens_' PromptStatus (Bool)
promptStatusIsExpiredL f PromptStatus{..} = (\promptStatusIsExpired -> PromptStatus { promptStatusIsExpired, ..} ) <$> f promptStatusIsExpired
{-# INLINE promptStatusIsExpiredL #-}


