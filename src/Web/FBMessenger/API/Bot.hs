{-# LANGUAGE DataKinds         #-}
--{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
--{-# LANGUAGE TemplateHaskell   #-}

-- | This module provides the Messenger Platform Bot API
module Web.FBMessenger.API.Bot ( module X ) where

import Web.FBMessenger.API.Bot.SendAPI as X
import Web.FBMessenger.API.Bot.Responses as X
import Web.FBMessenger.API.Bot.Requests as X
