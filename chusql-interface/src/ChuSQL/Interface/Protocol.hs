{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Interface.Protocol (
    protocolVersion,
    serverName,
    ClientRequest (..),
    ServerResponse (..),
    decodeRequest,
    encodeRequest,
    encodeResponse,
    decodeResponse,
    Grant (..),
    RoleView (..),
) where

import ChuSQL.Core.Protocol (Account (..), QueryResult (..), TableInfo (..), queryResultJson)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- server 线上协议的编解码：一行一条 JSON（UTF-8）。

-- | 一条角色授权：角色 + 具体权限 + 对象，可带转授权
data Grant = Grant
    { grantRole :: Text
    , grantPrivilege :: Text
    , grantObject :: Text
    , grantable :: Bool
    }
    deriving (Show, Eq, Ord)

-- | 一个角色的全貌
data RoleView = RoleView
    { roleName :: Text
    , roleGrants :: [Grant]
    , roleMembers :: [Text]
    }
    deriving (Show, Eq)

-- | 一条授权转成 JSON
instance ToJSON Grant where
    toJSON grant =
        object
            [ "role" .= grantRole grant
            , "privilege" .= grantPrivilege grant
            , "object" .= grantObject grant
            , "grantable" .= grantable grant
            ]

-- | 从 JSON 读一条授权
instance FromJSON Grant where
    parseJSON = withObject "Grant" $ \o ->
        Grant
            <$> o .: "role"
            <*> o .: "privilege"
            <*> o .: "object"
            <*> o .:? "grantable" .!= False

-- | 一个角色转成 JSON
instance ToJSON RoleView where
    toJSON view =
        object
            [ "role" .= roleName view
            , "grants" .= roleGrants view
            , "members" .= roleMembers view
            ]

-- | 从 JSON 读一个角色
instance FromJSON RoleView where
    parseJSON = withObject "RoleView" $ \o ->
        RoleView
            <$> o .: "role"
            <*> o .:? "grants" .!= []
            <*> o .:? "members" .!= []

-- | 协议版本；对不上就报协议错误
protocolVersion :: Int
protocolVersion = 1

-- | server 名字，握手时回给客户端
serverName :: Text
serverName = "chusql-server"

-- | 客户端请求
data ClientRequest
    = ReqHello Int
    | ReqLogin Text Text
    | ReqQuery Text
    | ReqPing
    | ReqQuit
    | ReqStorage Value
    | ReqCatalog
    | ReqDatabases
    | ReqRoles
    | ReqAccounts
    | ReqPolicy
    | ReqReloadPolicy
    deriving (Eq, Show)

-- | 线上请求的原始字段
data WireRequest = WireRequest
    { wrMethod :: Text
    , wrProtocol :: Maybe Int
    , wrUser :: Maybe Text
    , wrPassword :: Maybe Text
    , wrSql :: Maybe Text
    , wrRequest :: Maybe Value
    }

-- | 从 JSON 读原始请求字段
instance A.FromJSON WireRequest where
    parseJSON = A.withObject "request" $ \o ->
        WireRequest
            <$> o A..: "method"
            <*> o A..:? "protocol"
            <*> o A..:? "user"
            <*> o A..:? "password"
            <*> o A..:? "sql"
            <*> o A..:? "request"

-- | 请求解码：方法名走 method 标签
decodeRequest :: BSC.ByteString -> Either Text ClientRequest
decodeRequest raw = case A.eitherDecodeStrict raw of
    Left _ -> Left "request is not valid JSON"
    Right wire -> case T.toLower (T.strip (wrMethod wire)) of
        "hello" -> Right (ReqHello (fromMaybe protocolVersion (wrProtocol wire)))
        "login" -> case (wrUser wire, wrPassword wire) of
            (Just user, Just password) -> Right (ReqLogin user password)
            _ -> Left "login needs both user and password"
        "query" -> maybe (Left "query needs sql") (Right . ReqQuery) (wrSql wire)
        "storage" -> maybe (Left "storage needs request") (Right . ReqStorage) (wrRequest wire)
        "catalog" -> Right ReqCatalog
        "databases" -> Right ReqDatabases
        "roles" -> Right ReqRoles
        "accounts" -> Right ReqAccounts
        "policy" -> Right ReqPolicy
        "reload-policy" -> Right ReqReloadPolicy
        "ping" -> Right ReqPing
        "quit" -> Right ReqQuit
        other -> Left ("unknown method: " <> other)

-- | 客户端侧的请求编码（一行一条 JSON，不带换行）
encodeRequest :: ClientRequest -> BL.ByteString
encodeRequest = A.encode . requestJson

-- | 客户端请求转 JSON；方法名走 method 标签
requestJson :: ClientRequest -> Value
requestJson request = case request of
    ReqHello version -> object ["method" .= ("hello" :: Text), "protocol" .= version]
    ReqLogin user password -> object ["method" .= ("login" :: Text), "user" .= user, "password" .= password]
    ReqQuery sql -> object ["method" .= ("query" :: Text), "sql" .= sql]
    ReqPing -> object ["method" .= ("ping" :: Text)]
    ReqQuit -> object ["method" .= ("quit" :: Text)]
    ReqStorage payload -> object ["method" .= ("storage" :: Text), "request" .= payload]
    ReqCatalog -> object ["method" .= ("catalog" :: Text)]
    ReqDatabases -> object ["method" .= ("databases" :: Text)]
    ReqRoles -> object ["method" .= ("roles" :: Text)]
    ReqAccounts -> object ["method" .= ("accounts" :: Text)]
    ReqPolicy -> object ["method" .= ("policy" :: Text)]
    ReqReloadPolicy -> object ["method" .= ("reload-policy" :: Text)]

-- | 服务端响应
data ServerResponse
    = RespHello Int
    | RespLogin Text Bool
    | RespResult QueryResult
    | RespPong
    | RespBye
    | RespError Text Text
    | RespStorage Value
    | RespCatalog [TableInfo]
    | RespDatabases [Text]
    | RespRoles [RoleView]
    | RespAccounts [Account]
    | RespPolicy Int Int
    deriving (Eq, Show)

-- | 响应转 JSON；结果集只多一个 status 字段
encodeResponse :: ServerResponse -> BL.ByteString
encodeResponse = A.encode . responseJson

-- | 服务端响应转 JSON；结果集只是多一个 status 字段
responseJson :: ServerResponse -> Value
responseJson response = case response of
    RespHello version -> object ["status" .= ("hello" :: Text), "protocol" .= version, "server" .= serverName]
    RespLogin user admin -> object ["status" .= ("ok" :: Text), "user" .= user, "admin" .= admin]
    RespPong -> object ["status" .= ("pong" :: Text)]
    RespBye -> object ["status" .= ("bye" :: Text)]
    RespError code message -> object ["status" .= ("error" :: Text), "code" .= code, "message" .= message]
    RespStorage value -> object ["status" .= ("storage" :: Text), "response" .= value]
    RespCatalog infos -> object ["status" .= ("catalog" :: Text), "schemas" .= infos]
    RespDatabases names -> object ["status" .= ("databases" :: Text), "databases" .= names]
    RespRoles roleList -> object ["status" .= ("roles" :: Text), "roles" .= roleList]
    RespAccounts accountList -> object ["status" .= ("accounts" :: Text), "accounts" .= accountList]
    RespPolicy minLength classes -> object ["status" .= ("policy" :: Text), "minLength" .= minLength, "classes" .= classes]
    RespResult result -> case queryResultJson result of
        A.Object fields -> A.Object (KM.insert "status" (A.String "result") fields)
        other -> other

-- | 响应解码（客户端用）
decodeResponse :: BL.ByteString -> Either Text ServerResponse
decodeResponse raw = case A.eitherDecode raw of
    Left _ -> Left "response is not valid JSON"
    Right (A.Object fields) -> case KM.lookup "status" fields of
        Just (A.String status) -> decodeStatus status fields
        _ -> Left "response is missing status"
    Right _ -> Left "response is not a JSON object"
  where
    -- | 按 status 字段分派
    decodeStatus status fields = case status of
        "hello" -> Right (RespHello (intOf "protocol" fields))
        "ok" -> Right (RespLogin (textOf "user" fields) (boolOf "admin" fields))
        "pong" -> Right RespPong
        "bye" -> Right RespBye
        "error" -> Right (RespError (textOf "code" fields) (textOf "message" fields))
        "storage" -> case KM.lookup "response" fields of
            Just value -> Right (RespStorage value)
            Nothing -> Left "storage response is missing response"
        "result" -> case A.fromJSON (A.Object fields) of
            A.Success result -> Right (RespResult result)
            A.Error message -> Left (T.pack message)
        "catalog" -> fromField "schemas" RespCatalog fields
        "databases" -> fromField "databases" RespDatabases fields
        "roles" -> fromField "roles" RespRoles fields
        "accounts" -> fromField "accounts" RespAccounts fields
        "policy" -> Right (RespPolicy (intOf "minLength" fields) (intOf "classes" fields))
        other -> Left ("unexpected status: " <> other)

    -- | 取一个字段解成目标类型
    fromField :: A.FromJSON a => Text -> (a -> ServerResponse) -> A.Object -> Either Text ServerResponse
    fromField key wrap fields = case A.fromJSON (fromMaybe A.Null (KM.lookup (AK.fromText key) fields)) of
        A.Success value -> Right (wrap value)
        A.Error message -> Left (T.pack message)

    -- | 取一个整数字段
    intOf key fields = case KM.lookup key fields of
        Just (A.Number number) -> truncate number
        _ -> 0

    -- | 取一个文本字段
    textOf key fields = case KM.lookup key fields of
        Just (A.String text) -> text
        _ -> ""

    -- | 取一个布尔字段
    boolOf key fields = case KM.lookup key fields of
        Just (A.Bool flag) -> flag
        _ -> False
