{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | This module contains helper functions to work with JSON
--   Credits: https://github.com/klappvisor/haskell-telegram-api
module Web.FBMessenger.API.Bot.JsonExt
    (
      toJsonDrop,
      parseJsonDrop
    ) where

import           Data.Aeson
import           Data.Aeson.Types

-- | Method used to drop prefix from field name during serialization
toJsonDrop prefix = genericToJSON defaultOptions {
    fieldLabelModifier = drop prefix
  , omitNothingFields = True
  }

-- | Method used to drop prefix from field name during deserialization
parseJsonDrop prefix = genericParseJSON defaultOptions { fieldLabelModifier = drop prefix }

