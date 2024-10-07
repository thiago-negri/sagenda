{-# LANGUAGE OverloadedStrings #-}

module Sagenda.Data.User (
    User (User, PublicUser),
    userId,
    userName,
    userPassword
) where

import Data.Aeson ((.=), ToJSON, object, toJSON)

data User = User Int32 Text Text
          | PublicUser Int32 Text

instance ToJSON User where
    toJSON (User uid name password) =
        object ["id" .= uid, "name" .= name, "password" .= password]
    toJSON (PublicUser uid name) =
        object ["id" .= uid, "name" .= name]

userId :: User -> Int32
userId (User uid _ _) = uid
userId (PublicUser uid _) = uid

userName :: User -> Text
userName (User _ name _) = name
userName (PublicUser _ name) = name

userPassword :: User -> Maybe Text
userPassword (User _ _ p) = Just p
userPassword _ = Nothing
