{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)

import qualified Data.Text as T
import Data.Time.LocalTime
import Yesod.Auth.Message
import qualified Yesod.Auth.Message as M
import Yesod.Auth.HashDB (authHashDBWithForm, HashDBUser(..))

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data LoginForm = LoginForm {
    unLoginFormLoginId :: Text
  , unLoginFormPasswd :: Text
} deriving(Show, Eq)

mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadUnliftIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod

        -- ログインしている場合、ログインユーザーの情報を返す
        -- Just (1, AdminUsr { adminUsrAdminId = 1, adminUsrLoginId = "adminuser",
        --                     adminUsrPassword = Just "sha256|17|05UvOtj0ZRLjX8zBjmrMVQ==|J7+ASikhb6H2EfKK/iJn5PWAvF8BsjSNRbRQg5+utVs=",
        --                     adminUsrValidFlag = True, adminUsrCreateTime = 2020-12-03 22:09:43.903239 UTC,
        --                     adminUsrUpdateTime = 2020-12-04 22:25:37.671491 UTC, adminUsrVersion = 1
        --                    })
        muser <- maybeAuthPair

        -- 表示される画面のURLのRoute型が入る
        mcurrentRoute <- getCurrentRoute

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_hashauth_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- 認証が必要な場合のリダイレクト先
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized TopR _ = return Authorized
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized

    isAuthorized EntR _ = isAuthenticated

    -- 静的コンテンツのためのパスをコンテンツから計算したハッシュ値を元に
    -- ユニークなファイル名として生成する、これによって静的コンテンツの長
    -- 期的キャッシュにも対応しする
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- 稼働環境別のログ出力レベルの調整
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = AdminUsrId

    loginDest :: App -> Route App
    loginDest _ = EntR

    logoutDest :: App -> Route App
    logoutDest _ = TopR

    -- ログイン成功時、遷移先URLを保存したい場合Trueを設定する
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniAdminUsrLoginId $ credsIdent creds
        case x of
            Just (Entity uid usr) ->
              case adminUsrValidFlag usr of
                False -> do
                  -- アカウント停止中
                  return $ UserError InvalidLogin
                True -> do
                  -- 正常なログイン
                  updateAdminLoginTime uid
                  return $ Authenticated uid
            -- yesod-auth-hashdb内部で処理してしまうのでここは通らない
            Nothing -> return $ UserError $ IdentifierNotFound "Admin user not found: "

    -- 認証バックエンドをパスワード認証へセット
    -- カスタムフォームと認証ユーザーを取得するためのユニークフィールドを指定する
    authPlugins :: App -> [AuthPlugin App]
    authPlugins _ = [authHashDBWithForm loginPage (Just . UniAdminUsrLoginId)]

    -- AuthMessageのロケールを日本語に設定する
    renderAuthMessage :: master -> [Text] -> AuthMessage -> Text
    renderAuthMessage _ _ = japaneseMessage

instance HashDBUser AdminUsr where
    userPasswordHash = adminUsrPassword
    setPasswordHash h u = u { adminUsrPassword = Just h }

-- 認証されているかのチェック
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

instance YesodAuthPersist App

instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- カスタムログインページ
loginPage :: Route App -> WidgetFor App ()
loginPage loginRoute = do
    msg <- getMessages
    (widget, _) <- liftHandler $ generateFormPost $ loginForm
    setTitle "ログイン"
    toWidgetHead
        [hamlet|
            <meta name="keywords" content="ログイン">
            <meta name="description" content="ログイン">
        |]
    $(whamletFile "templates/login.hamlet")

-- カスタムログインフォーム
loginForm :: Html -> MForm Handler (FormResult LoginForm, Widget)
loginForm extra = do
    let loginIdFieldSet = FieldSettings {
            fsLabel = "",
            fsTooltip = Nothing,
            fsId = Just "username",
            fsName = Just "username",
            fsAttrs = [
                ("class", "form-control form-control-lg rounded-0"),
                ("required", ""),
                ("autocomplete", "username"),
                ("placeholder", "ログインID")
            ]
        }
        passwdFieldSet = FieldSettings {
            fsLabel = "",
            fsTooltip = Nothing,
            fsId = Nothing,
            fsName = Just "password",
            fsAttrs = [
                ("class", "form-control form-control-lg rounded-0"),
                ("required", ""),
                ("autocomplete", "current-password"),
                ("placeholder", "パスワード")
            ]
        }
    master <- getYesod
    let loginIdLen = 10
        passwdLen = 10
    (loginRes, loginView) <- mreq (adminLoginIdField loginIdLen) loginIdFieldSet Nothing
    (passwdRes, passwdView) <- mreq (passPasswordField passwdLen) passwdFieldSet Nothing
    let loginFormParam = LoginForm <$> loginRes <*> passwdRes
    let widget = do
            [whamlet|
                #{extra}
                <div .form-group>
                    <label for="loginid">ログインID
                    ^{fvInput loginView}
                    <div .invalid-feedback>ログインIDを入力してください

                <div .form-group>
                    <label>パスワード
                    ^{fvInput passwdView}
                    <div .invalid-feedback>パスワードを入力してください
            |]
    return (loginFormParam, widget)

-- リダイレクトしてしまうためこのエラーメッセージは出力されません
errorMessage :: Text
errorMessage = "ID・パスワードが不正です"

adminLoginIdField :: Int -> Field Handler Text
adminLoginIdField loginIdLen = check (validateLoginId loginIdLen) textField

validateLoginId :: Int -> Text -> Either Text Text
validateLoginId loginIdLen loginId
    | T.length(loginId) > loginIdLen = Left errorMessage
    | otherwise = Right loginId

passPasswordField :: Int -> Field Handler Text
passPasswordField passwdLen = check (validatePasswd passwdLen) passwordField

validatePasswd :: Int -> Text -> Either Text Text
validatePasswd passwdLen passwd
    | T.length(passwd) > passwdLen = Left errorMessage
    | otherwise = Right passwd
 
updateAdminLoginTime :: Key AdminUsr -> ReaderT SqlBackend (HandlerFor App) ()
updateAdminLoginTime uid = do
  now <- liftIO $ getZonedTime >>= return . zonedTimeToUTC
  update uid [ AdminUsrUpdateTime =. now ]
  return ()

headerWidget :: Widget
headerWidget = $(widgetFile "header")

footerWidget :: Widget
footerWidget = $(widgetFile "footer")
