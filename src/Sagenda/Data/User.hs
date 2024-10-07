{-# LANGUAGE OverloadedStrings #-}

module Sagenda.Data.User (
    getAllUsers,
    userId,
    userName,
    userPassword
) where
    
import Data.Aeson ((.=), ToJSON, object, toJSON)
import Hasql.Connection (Connection)
import Hasql.Session as S
import Sagenda.Data.Session as SS

data User = User Int32 Text Text
          | PublicUser Int32 Text

userId :: User -> Int32
userId (User uid _ _) = uid
userId (PublicUser uid _) = uid

userName :: User -> Text
userName (User _ name _) = name
userName (PublicUser _ name) = name

userPassword :: User -> Maybe Text
userPassword (User _ _ p) = Just p
userPassword _ = Nothing

instance ToJSON User where
    toJSON (User uid name password) =
        object ["id" .= uid, "name" .= name, "password" .= password]
    toJSON (PublicUser uid name) =
        object ["id" .= uid, "name" .= name]

readPublicUser :: (Int32, Text) -> User
readPublicUser (uid, name) = PublicUser uid name

getAllUsers :: Connection -> IO [User]
getAllUsers connection = do
    -- FIXME(tnegri): Handle error
    Right rows <- S.run SS.allUsers connection
    let result = map readPublicUser $ toList rows
    return result
