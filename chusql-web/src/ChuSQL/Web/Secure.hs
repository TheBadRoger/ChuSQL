{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Web.Secure (
    securityHeaders,
    contentSecurityPolicy,
    sameOriginOnly,
    bodyLimit,
    bodyLimitDynamic,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (
    Header,
    Method,
    hContentLength,
    hOrigin,
    methodPost,
    status403,
    status413,
 )
import Network.Wai (Middleware, Request (..), mapResponseHeaders, responseLBS)

-- 三个 WAI 安全中间件：响应头、同源校验、请求体上限。

-- | 所有响应都带上的安全头
securityHeaders :: Middleware
securityHeaders app req respond =
    app req (respond . mapResponseHeaders (\headers -> filter (\(name, _) -> name /= "Content-Security-Policy" || lookup name headers == Nothing) extraHeaders ++ headers))

-- | 只给编辑器样式发随机 nonce
contentSecurityPolicy :: Maybe Text -> Text
contentSecurityPolicy nonce =
    "default-src 'self'; script-src 'self'; style-src 'self'"
        <> maybe "" (\value -> " 'nonce-" <> value <> "'") nonce
        <> "; font-src 'self'; worker-src 'self'; img-src 'self' data:; connect-src 'self'; base-uri 'none'; form-action 'self'; frame-ancestors 'none'"

-- | 统一加的安全响应头
extraHeaders :: [Header]
extraHeaders =
    [ ("Content-Security-Policy", TE.encodeUtf8 (contentSecurityPolicy Nothing))
    , ("X-Content-Type-Options", "nosniff")
    , ("X-Frame-Options", "DENY")
    , ("Referrer-Policy", "no-referrer")
    , ("Cache-Control", "no-store")
    ]

-- | 变更类请求要求同源，无 Origin 放行
sameOriginOnly :: Middleware
sameOriginOnly app req respond =
    if isMutating (requestMethod req) && not (originAllowed req)
        then respond (responseLBS status403 [] "cross-origin request rejected")
        else app req respond

-- | 这个方法算不算会改数据的
isMutating :: Method -> Bool
isMutating m = m `elem` [methodPost, "PUT", "PATCH", "DELETE"]

-- | Origin 与 Host 是否一致
originAllowed :: Request -> Bool
originAllowed req = case lookup hOrigin (requestHeaders req) of
    Nothing -> True
    Just origin -> case lookup "Host" (requestHeaders req) of
        Nothing -> False
        Just host -> authorityOf origin == host || authorityOf origin == stripPort host

-- | 取出 host:port，去掉 scheme 与路径
authorityOf :: BS.ByteString -> BS.ByteString
authorityOf raw =
    let noScheme = case BSC.breakSubstring "//" raw of
            (_, rest) | not (BSC.null rest) -> BSC.drop 2 rest
            _ -> raw
     in fst (BSC.break (== '/') noScheme)

-- | 去掉端口 host:8080 -> host
stripPort :: BS.ByteString -> BS.ByteString
stripPort = fst . BSC.break (== ':')

-- | 按 Content-Length 拦下过大的请求体
bodyLimit :: Int -> Middleware
bodyLimit limit = bodyLimitDynamic (pure limit)

-- | 同上，但上限每次请求现取
bodyLimitDynamic :: IO Int -> Middleware
bodyLimitDynamic limitOf app req respond = do
    limit <- limitOf
    if declaredTooBig limit req
        then respond (responseLBS status413 [] "request body too large")
        else app req respond

-- | Content-Length 是否超限
declaredTooBig :: Int -> Request -> Bool
declaredTooBig limit req = case lookup hContentLength (requestHeaders req) of
    Nothing -> False
    Just v -> case BSC.readInt v of
        Just (n, rest) -> BSC.null rest && n > limit
        Nothing -> False
