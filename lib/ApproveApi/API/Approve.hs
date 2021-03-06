{-
   ApproveAPISwagger

   The simple API to request a user's approval on anything via email + sms.

   OpenAPI Version: 3.0.0
   ApproveAPISwagger API version: 1.0.1
   Contact: dev@approveapi.com
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : ApproveApi.API.Approve
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module ApproveApi.API.Approve where

import ApproveApi.Core
import ApproveApi.MimeTypes
import ApproveApi.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** Approve

-- *** createPrompt

-- | @POST \/prompt@
-- 
-- Sending a prompt
-- 
-- Creates a prompt and pushes it to the user (sends via email, sms, or other supported protocols).
-- 
-- AuthMethod: 'AuthBasicApiKey'
-- 
createPrompt 
  :: (Consumes CreatePrompt MimeJSON, MimeRender MimeJSON CreatePromptRequest)
  => CreatePromptRequest -- ^ "createPromptRequest"
  -> ApproveApiRequest CreatePrompt MimeJSON Prompt MimeJSON
createPrompt createPromptRequest =
  _mkRequest "POST" ["/prompt"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicApiKey)
    `setBodyParam` createPromptRequest

data CreatePrompt 
instance HasBodyParam CreatePrompt CreatePromptRequest 

-- | @application/json@
instance Consumes CreatePrompt MimeJSON

-- | @application/json@
instance Produces CreatePrompt MimeJSON


-- *** getPrompt

-- | @GET \/prompt\/{id}@
-- 
-- Retrieve a prompt
-- 
-- Retrieve the prompt object with the given ID.
-- 
-- AuthMethod: 'AuthBasicApiKey'
-- 
getPrompt 
  :: Id -- ^ "id" -  The identifier for a pending or completed prompt. This is returned when you create a prompt.
  -> ApproveApiRequest GetPrompt MimeNoContent Prompt MimeJSON
getPrompt (Id id) =
  _mkRequest "GET" ["/prompt/",toPath id]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicApiKey)

data GetPrompt  

-- | /Optional Param/ "long_poll" - If true, the request waits (long-polls) until the user responds to the prompt or more than 10 minutes pass. Defaults to false.
instance HasOptionalParam GetPrompt LongPoll where
  applyOptionalParam req (LongPoll xs) =
    req `setQuery` toQuery ("long_poll", Just xs)

-- | @application/json@
instance Produces GetPrompt MimeJSON


-- *** getPromptStatus

-- | @GET \/prompt\/{id}\/status@
-- 
-- Check prompt status
-- 
-- Returns whether a prompt has been completed by the user. This request does not require authentication, and so can be used client-side without sharing API credentials.
-- 
getPromptStatus 
  :: Id -- ^ "id" -  The prompt identifier.
  -> ApproveApiRequest GetPromptStatus MimeNoContent PromptStatus MimeJSON
getPromptStatus (Id id) =
  _mkRequest "GET" ["/prompt/",toPath id,"/status"]

data GetPromptStatus  

-- | @application/json@
instance Produces GetPromptStatus MimeJSON

