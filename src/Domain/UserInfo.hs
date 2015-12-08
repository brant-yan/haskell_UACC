module Domain.UserInfo where

import Domain.Base

data UserInfo = User {ui_username::UserName
                    , ui_password::Password
                    , ui_firstname :: FirstName
                    , ui_secondname :: SecondName
                    }deriving(Show)