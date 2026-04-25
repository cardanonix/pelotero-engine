{-# LANGUAGE OverloadedStrings #-}

module Pelotero.Prelude
  ( appName
  , appVersion
  ) where

import Data.Text (Text)

appName :: Text
appName = "pelotero-engine"

appVersion :: Text
appVersion = "0.0.10.0"