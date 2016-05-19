{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | This module provides the Messenger Platform Bot API
module Web.FBMessenger.API.Bot 
  ( module Web.FBMessenger.API.Bot.API
  , module Web.FBMessenger.API.Bot.Data
  , module Web.FBMessenger.API.Bot.Responses
  , module Web.FBMessenger.API.Bot.Requests
  ) where

import Web.FBMessenger.API.Bot.API
import Web.FBMessenger.API.Bot.Data
import Web.FBMessenger.API.Bot.Responses
import Web.FBMessenger.API.Bot.Requests
