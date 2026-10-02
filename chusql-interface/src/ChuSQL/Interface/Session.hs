{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Interface.Session (
    Session,
    newSession,
    sessionIsAdmin,
    sessionDatabase,
    isPlainIdentifier,
    authenticateSession,
    switchDatabase,
    runStatement,
    catalog,
    databases,
    roleViews,
    accounts,
) where

import ChuSQL.Core.Protocol (Account, QueryResult (..), TableInfo)
import ChuSQL.Interface.Link (
    Client,
    clientAccounts,
    clientCatalog,
    clientDatabases,
    clientLogin,
    clientQuery,
    clientRoles,
 )
import ChuSQL.Interface.Protocol (RoleView)
import Data.Char (isAlpha, isAlphaNum)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T

-- 前端会话：本地记登录态与当前库，语句答案全部来自 server。

-- | 一个前端会话：连接、当前库与管理员标记
data Session = Session
    { ssClient :: Client
    , ssCurrent :: IORef Text
    , ssAdmin :: IORef Bool
    }

-- | 开一个前端会话：登录之前没有管理员身份，也没有选中的库
newSession :: Client -> IO Session
newSession client = Session client <$> newIORef "" <*> newIORef False

-- | 是不是管理员（由登录结果决定）
sessionIsAdmin :: Session -> IO Bool
sessionIsAdmin = readIORef . ssAdmin

-- | 当前库
sessionDatabase :: Session -> IO Text
sessionDatabase = readIORef . ssCurrent

-- | 与解析器一致的裸标识符判断
isPlainIdentifier :: Text -> Bool
isPlainIdentifier name =
    not (T.null name)
        && T.length name <= 64
        && (isAlpha (T.head name) || T.head name == '_')
        && T.all (\ch -> isAlphaNum ch || ch == '_') name

-- | 登录：身份由 server 判定，这里只把管理员标记记下来
authenticateSession :: Session -> Text -> Text -> IO (Either Text ())
authenticateSession session user password = do
    answer <- clientLogin (ssClient session) user password
    case answer of
        Left message -> pure (Left message)
        Right admin -> writeIORef (ssAdmin session) admin >> pure (Right ())

-- | 换库：名字先按解析器规矩校验，由 server 执行 USE
switchDatabase :: Session -> Text -> IO (Either Text ())
switchDatabase session rawName
    | not (isPlainIdentifier name) = pure (Left ("not a plain database name: " <> name))
    | otherwise = do
        answer <- runStatement session ("USE " <> name)
        case answer of
            Left message -> pure (Left message)
            Right _ -> pure (Right ())
  where
    -- | 规范化后的库名
    name = T.toLower (T.strip rawName)

-- | 跑一条语句；结果集里带回的当前库口径跟着一起更新
runStatement :: Session -> Text -> IO (Either Text QueryResult)
runStatement session sql = do
    answer <- clientQuery (ssClient session) sql
    case answer of
        Left message -> pure (Left message)
        Right result -> do
            case qrDatabase result of
                Just name -> writeIORef (ssCurrent session) name
                Nothing -> pure ()
            pure (Right result)

-- | 数据字典（\\dt 与 \\d 用）
catalog :: Session -> IO (Either Text [TableInfo])
catalog session = clientCatalog (ssClient session)

-- | 库清单（\\l 用）
databases :: Session -> IO (Either Text [Text])
databases session = clientDatabases (ssClient session)

-- | 角色总览（\\dr 用）
roleViews :: Session -> IO (Either Text [RoleView])
roleViews session = clientRoles (ssClient session)

-- | 账号清单（\\du 与管理页用）
accounts :: Session -> IO (Either Text [Account])
accounts session = clientAccounts (ssClient session)
