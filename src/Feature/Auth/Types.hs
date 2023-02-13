{-# LANGUAGE DeriveGeneric #-}
module Feature.Auth.Types where

import ClassyPrelude


import Data.Aeson (ToJSON, FromJSON)

type Token = Text
type UserId = Integer
type CurrentUser = (Token, UserId)

data TokenError
  = TokenErrorUserIdNotFound
  | TokenErrorNotFound
  | TokenErrorExpired
  | TokenErrorMalformed String
  deriving (Eq, Show, Generic)


instance ToJSON TokenError
instance FromJSON TokenError