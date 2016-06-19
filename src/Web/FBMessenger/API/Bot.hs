-- |
-- Module      : Web.FBMessenger.API.Bot
-- License     : BSD3
-- Maintainer  : Marcello Seri <marcello.seri@gmail.com>
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides the Messenger Platform Bot API.
-- It re-exports the following modules:
--
--   - 'Web.FBMessenger.API.Bot.Requests'
--   - 'Web.FBMessenger.API.Bot.Responses'
--   - 'Web.FBMessenger.API.Bot.WebhookAPI'
--   - 'Web.FBMessenger.API.Bot.SendAPI'
--
module Web.FBMessenger.API.Bot ( module X ) where

import Web.FBMessenger.API.Bot.SendAPI as X
import Web.FBMessenger.API.Bot.Responses as X
import Web.FBMessenger.API.Bot.Requests as X
import Web.FBMessenger.API.Bot.WebhookAPI as X
