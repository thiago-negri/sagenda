{-# LANGUAGE OverloadedStrings #-}

module Sagenda.Data.User (
    User (User),
    userId,
    userName,
    userPassword,
    userToPublic,
    UserPublic (UserPublic),
    userPublicId,
    userPublicName
) where

import Data.Aeson ((.=), ToJSON, object, toJSON)

data User = User {
    userId :: Int32,
    userName :: Text,
    userPassword :: Text
}

data UserPublic = UserPublic {
    userPublicId :: Int32,
    userPublicName :: Text
}

userToPublic :: User -> UserPublic
userToPublic (User uid name _) = UserPublic uid name

instance ToJSON UserPublic where
    toJSON (UserPublic uid name) = object ["id" .= uid, "name" .= name]
