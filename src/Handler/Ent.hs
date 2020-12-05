{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Ent where

import Import

getEntR :: Handler Html
getEntR = do
  muser <- maybeAuthPair
  msg <- getMessages
  defaultLayout $ do
    setTitle "ログイン完了"
    $(widgetFile "ent")
