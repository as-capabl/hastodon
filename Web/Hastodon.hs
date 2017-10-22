{-# LANGUAGE OverloadedStrings #-}

module Web.Hastodon
  (
    module Web.Hastodon.Option

  , HastodonClient(..)

  , Account(..)
  , Application(..)
  , Attachment(..)
  , Card(..)
  , Context(..)
  , Instance(..)
  , Mention(..)
  , Notification(..)
  , OAuthClient(..)
  , Relationship(..)
  , Report(..)
  , Results(..)
  , Status(..)
  , Tag(..)
  , StreamEvent(..)

  , mkHastodonClient
  , getAccountById
  , getCurrentAccount
  , getFollowers
  , getFollowersWithOption
  , getFollowing
  , getFollowingWithOption
  , getAccountStatuses
  , postFollow
  , postUnfollow
  , postBlock
  , postUnblock
  , postMute
  , postUnmute
  , getRelationships
  , getSearchedAccounts
  , postApps
  , getBlocks
  , getFavorites
  , getFollowRequests
  , postAuthorizeRequest
  , postRejectRequest
  , getInstance
  , getMutes
  , getNotifications
  , getNotificationsWithOption
  , getNotificationById
  , postNotificationsClear
  , getReports
  , getSearchedResults
  , getSearchedResultsWithOption
  , getStatus
  , getStatusWithOption
  , getCard
  , getContext
  , getRebloggedBy
  , getFavoritedBy
  , postStatus
  , postStatusWithOption
  , postReblog
  , postUnreblog
  , postFavorite
  , postUnfavorite
  , getHomeTimeline
  , getHomeTimelineWithOption
  , getPublicTimeline
  , getPublicTimelineWithOption
  , sinkUserTimeline
  , sourceUserTimeline
  , sinkPublicTimeline
  , sourcePublicTimeline
  , getTaggedTimeline
  , getTaggedTimelineWithOption
  ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Catch
import Control.Monad.Trans.Resource
import Data.Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as LChar8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Attoparsec.ByteString.Char8 as PC
import Data.String.Utils
import qualified Data.Map as Map
import Network.HTTP.Simple
import Network.HTTP.Types.Header
import qualified Network.HTTP.Conduit as HC
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Attoparsec as CA
import Debug.Trace

import Web.Hastodon.Option

--
-- Utility
--
utf8ToChar8 :: String -> Char8.ByteString
utf8ToChar8 = T.encodeUtf8 . T.pack

--
-- Mastodon API endpoints
--
pAccountById       = "/api/v1/accounts/:id"
pCurrentAccounts   = "/api/v1/accounts/verify_credentials"
pFollowers         = "/api/v1/accounts/:id/followers"
pFollowing         = "/api/v1/accounts/:id/following"
pAccountStatuses   = "/api/v1/accounts/:id/statuses"
pFollow            = "/api/v1/accounts/:id/follow"
pUnfollow          = "/api/v1/accounts/:id/unfollow"
pBlock             = "/api/v1/accounts/:id/block"
pUnblock           = "/api/v1/accounts/:id/unblock"
pMute              = "/api/v1/accounts/:id/mute"
pUnmute            = "/api/v1/accounts/:id/unmute"
pRelationships     = "/api/v1/accounts/relationships"
pSearchAccounts    = "/api/v1/accounts/search"
pApps              = "/api/v1/apps"
pBlocks            = "/api/v1/blocks"
pFavorites         = "/api/v1/favourites"
pFollowRequests    = "/api/v1/follow_requests"
pAuthorizeRequest  = "/api/v1/follow_requests/:id/authorize"
pRejectRequest     = "/api/v1/follow_requests/:id/reject"
pInstance          = "/api/v1/instance"
pMutes             = "/api/v1/mutes"
pNotifications     = "/api/v1/notifications"
pNotificationById  = "/api/v1/notifications/:id"
pNotificationClear = "/api/v1/notifications/clear"
pReports           = "/api/v1/reports"
pSearch            = "/api/v1/search"
pStatus            = "/api/v1/statuses/:id"
pContext           = "/api/v1/statuses/:id/context"
pCard              = "/api/v1/statuses/:id/card"
pRebloggedBy       = "/api/v1/statuses/:id/reblogged_by"
pFavoritedBy       = "/api/v1/statuses/:id/favourited_by"
pStatuses          = "/api/v1/statuses"
pDeleteStatus      = "/api/v1/statuses/:id"
pHomeTimeline      = "/api/v1/timelines/home"
pPublicTimeline    = "/api/v1/timelines/public"
pReblog            = "/api/v1/statuses/:id/reblog"
pUnreblog          = "/api/v1/statuses/:id/unreblog"
pFavorite          = "/api/v1/statuses/:id/favourite"
pUnfavorite        = "/api/v1/statuses/:id/unfavourite"
pTaggedTimeline    = "/api/v1/timelines/tag/:hashtag"
pUserStream        = "/api/v1/streaming/user"
pPublicStream      = "/api/v1/streaming/public"

data HastodonClient = HastodonClient {
  host :: String,
  token :: String
}

data OAuthResponse = OAuthResponse {
  accessToken :: String
  -- NOTE currently ignore other fields.
} deriving (Show)
instance FromJSON OAuthResponse where
  parseJSON (Object v) =
    OAuthResponse <$> (v .: T.pack "access_token")

data Account = Account {
  accountId :: Int,
  accountUsername :: String,
  accountAcct :: String,
  accountDisplayName :: String,
  accountLocked :: Bool,
  accountCreatedAt :: String,
  accountFollowersCount :: Int,
  accountFollowingCount :: Int,
  accountStatusesCount :: Int,
  accountNote :: String,
  accountUrl :: String,
  accountAvatar :: String,
  accountAvatarStatic :: String,
  accountHeader :: String,
  accountHeaderStatic :: String
} deriving (Show)
instance FromJSON Account where
  parseJSON (Object v) =
    Account <$> (v .: T.pack "id")
            <*> (v .: T.pack "username")
            <*> (v .: T.pack "acct")
            <*> (v .: T.pack "display_name")
            <*> (v .: T.pack "locked")
            <*> (v .: T.pack "created_at")
            <*> (v .: T.pack "followers_count")
            <*> (v .: T.pack "following_count")
            <*> (v .: T.pack "statuses_count")
            <*> (v .: T.pack "note")
            <*> (v .: T.pack "url")
            <*> (v .: T.pack "avatar")
            <*> (v .: T.pack "avatar_static")
            <*> (v .: T.pack "header")
            <*> (v .: T.pack "header_static")

data Application = Application {
  applicationName :: String,
  applicationWebsite :: Maybe String
} deriving (Show)
instance FromJSON Application where
  parseJSON (Object v) =
    Application <$> (v .:  T.pack "name")
                <*> (v .:? T.pack "website")

data Attachment = Attachment {
  attachmentId :: Int,
  attachmentType :: String,
  attachmentUrl :: String,
  attachmentRemoteUrl :: String,
  attachmentPreviewUrl :: String,
  attachmentTextUrl :: Maybe String
} deriving (Show)
instance FromJSON Attachment where
  parseJSON (Object v) =
    Attachment <$> (v .:  T.pack "id")
               <*> (v .:  T.pack "type")
               <*> (v .:  T.pack "url")
               <*> (v .:  T.pack "remote_url")
               <*> (v .:  T.pack "preview_url")
               <*> (v .:? T.pack "text_url")

data Card = Card {
  cardUrl :: String,
  cardTitle :: String,
  cardDescription :: String,
  cardImage :: String
} deriving (Show)
instance FromJSON Card where
  parseJSON (Object v) =
    Card <$> (v .: T.pack "url")
         <*> (v .: T.pack "title")
         <*> (v .: T.pack "description")
         <*> (v .: T.pack "image")

data Context = Context {
  contextAncestors :: [Status],
  contextDescendants :: [Status]
} deriving (Show)
instance FromJSON Context where
  parseJSON (Object v) =
    Context <$> (v .: T.pack "ancestors")
            <*> (v .: T.pack "descendants")

data Instance = Instance {
  instanceUri :: String,
  instanceTitle :: String,
  instanceDescription :: String,
  instanceEmail :: String
} deriving (Show)
instance FromJSON Instance where
  parseJSON (Object v) =
    Instance <$> (v .: T.pack "uri")
             <*> (v .: T.pack "title")
             <*> (v .: T.pack "description")
             <*> (v .: T.pack "email")

data Mention = Mention {
  mentionUrl :: String,
  mentionUsername :: String,
  mentionAcct :: String,
  mentionId :: Int
} deriving (Show)
instance FromJSON Mention where
  parseJSON (Object v) =
    Mention <$> (v .: T.pack "url")
            <*> (v .: T.pack "username")
            <*> (v .: T.pack "acct")
            <*> (v .: T.pack "id")

data Notification = Notification {
  notificationId :: Int,
  notificationType :: String,
  notificationCreatedAt :: String,
  notificationAccount :: Account,
  notificationStatus :: Maybe Status
} deriving (Show)
instance FromJSON Notification where
  parseJSON (Object v) =
    Notification <$> (v .:  T.pack "id")
                 <*> (v .:  T.pack "type")
                 <*> (v .:  T.pack "created_at")
                 <*> (v .:  T.pack "account")
                 <*> (v .:? T.pack "status")

data OAuthClient = OAuthClient {
  oauthClientId :: Int,
  oauthClientRedirectUri :: String,
  oauthClientClientId :: String,
  oauthClientClientSecret :: String
} deriving (Show)
instance FromJSON OAuthClient where
  parseJSON (Object v) =
    OAuthClient <$> (v .: T.pack "id")
                <*> (v .: T.pack "redirect_uri")
                <*> (v .: T.pack "client_id")
                <*> (v .: T.pack "client_secret")

data Relationship = Relationship {
  relationshipId :: Int,
  relationshipFollowing :: Bool,
  relationshipFollowed_by :: Bool,
  relationshipBlocking :: Bool,
  relationshipMuting :: Bool,
  relationshipRequested :: Bool
} deriving (Show)
instance FromJSON Relationship where
  parseJSON (Object v) =
    Relationship <$> (v .: T.pack "id")
                 <*> (v .: T.pack "following")
                 <*> (v .: T.pack "followed_by")
                 <*> (v .: T.pack "blocking")
                 <*> (v .: T.pack "muting")
                 <*> (v .: T.pack "requested")

data Report = Report {
  reportId :: Int,
  reportActionToken :: String
} deriving (Show)
instance FromJSON Report where
  parseJSON (Object v) =
    Report <$> (v .: T.pack "id")
           <*> (v .: T.pack "action_taken")

data Results = Results {
  resultAccounts :: [Account],
  resultStatus :: [Status],
  resultHashtags :: [String]
} deriving (Show)
instance FromJSON Results where
  parseJSON (Object v) =
    Results <$> (v .: T.pack "accounts")
            <*> (v .: T.pack "statuses")
            <*> (v .: T.pack "hashtags")

data Status = Status {
  statusId :: Int,
  statusUri :: String,
  statusUrl :: String,
  statusAccount :: Account,
  statusInReplyToId :: Maybe Int,
  statusInReplyToAccountId :: Maybe Int,
  statusReblog :: Maybe Status,
  statusContent :: String,
  statusCreatedAt :: String,
  statusReblogsCount :: Int,
  statusFavouritesCount :: Int,
  statusReblogged :: Maybe Bool,
  statusFavourited :: Maybe Bool,
  statusSensitive :: Maybe Bool,
  statusSpoilerText :: String,
  statusVisibility :: String,
  statusMediaAttachments :: [Attachment],
  statusMentions :: [Mention],
  statusTags :: [Tag],
  statusApplication :: Maybe Application
} deriving (Show)
instance FromJSON Status where
  parseJSON (Object v) =
    Status <$> (v .:  T.pack "id")
           <*> (v .:  T.pack "uri")
           <*> (v .:  T.pack "url")
           <*> (v .:  T.pack "account")
           <*> (v .:? T.pack "in_reply_to_id")
           <*> (v .:? T.pack "in_reply_to_account_id")
           <*> (v .:? T.pack "reblog")
           <*> (v .:  T.pack "content")
           <*> (v .:  T.pack "created_at")
           <*> (v .:  T.pack "reblogs_count")
           <*> (v .:  T.pack "favourites_count")
           <*> (v .:? T.pack "reblogged")
           <*> (v .:? T.pack "favourited")
           <*> (v .:? T.pack "sensitive")
           <*> (v .:  T.pack "spoiler_text")
           <*> (v .:  T.pack "visibility")
           <*> (v .:  T.pack "media_attachments")
           <*> (v .:  T.pack "mentions")
           <*> (v .:  T.pack "tags")
           <*> (v .:? T.pack "application")
  parseJSON u = error $ "parseJSON " ++ show u

data Tag = Tag {
  name :: String,
  url :: String
} deriving (Show)
instance FromJSON Tag where
  parseJSON (Object v) =
    Tag <$> (v .: T.pack "name")
        <*> (v .: T.pack "url")

data StreamEvent =
  StreamUpdate Status | StreamNotify Notification | StreamDelete Int | StreamHeartbeat
  deriving Show

-- 
-- helpers
-- 

getOAuthToken :: String -> String -> String -> String -> String -> IO (Either JSONException OAuthResponse)
getOAuthToken clientId clientSecret username password host = do
  initReq <- parseRequest $ "https://" ++ host ++ "/oauth/token"
  let reqBody = [(Char8.pack "client_id", utf8ToChar8 clientId),
                 (Char8.pack "client_secret", utf8ToChar8 clientSecret),
                 (Char8.pack "username", utf8ToChar8 username),
                 (Char8.pack "password", utf8ToChar8 password),
                 (Char8.pack "grant_type", Char8.pack "password"),
                 (Char8.pack "scope", Char8.pack "read write follow")]
  let req = setRequestBodyURLEncoded reqBody $ initReq
  res <- httpJSONEither req
  return $ (getResponseBody res :: Either JSONException OAuthResponse)

mkHastodonHeader :: String -> Request -> Request
mkHastodonHeader token =
  addRequestHeader hAuthorization $ utf8ToChar8 $ "Bearer " ++ token

mkHastodonRequestWithQuery ::
  [(Char8.ByteString, Maybe Char8.ByteString)] -> String -> HastodonClient -> IO Request
mkHastodonRequestWithQuery opt path client = do
  initReq <- parseRequest $ "https://" ++ (host client) ++ path
  return $
    mkHastodonHeader (token client) $
    setRequestQueryString opt $ initReq

mkHastodonRequest :: String -> HastodonClient -> IO Request
mkHastodonRequest = mkHastodonRequestWithQuery []

getHastodonResponseBody :: String -> HastodonClient -> IO LChar8.ByteString
getHastodonResponseBody path client = do
  req <- mkHastodonRequest path client
  res <- httpLBS req
  return $ getResponseBody res

getHastodonResponseJSONWithOption opt path client =
  mkHastodonRequestWithQuery opt path client >>= httpJSONEither

getHastodonResponseJSON path client = mkHastodonRequest path client >>= httpJSONEither

postAndGetHastodonResult path body client = do
  initReq <- mkHastodonRequest path client
  let req = setRequestBodyURLEncoded body $ initReq
  res <- httpNoBody req
  return $ (getResponseStatusCode res) == 200

postAndGetHastodonResponseJSON path body client = do
  initReq <- mkHastodonRequest path client
  let req = setRequestBodyURLEncoded body $ initReq
  trace (show req) $ httpJSONEither req

getHastodonResponseSink path client sink = do
  req <- liftIO $ mkHastodonRequest path client
  httpSink req (\rp -> pipeJSONList C.=$= sink rp)

getHastodonResponseSource path client getSource = do
  req <- liftIO $ mkHastodonRequest path client
  httpSource req (\rp -> getSource rp C.=$= pipeJSONList)

-- Throws CA.ParseError
pipeJSONList ::
  MonadThrow m => C.ConduitM Char8.ByteString StreamEvent m ()
pipeJSONList = CL.sequence $ CA.sinkParser $ ignoreSpace >> (hatb <|> evnt <|> errStr)
  where
    ignoreSpace = PC.skipMany $ (PC.space >> return ()) <|> (PC.endOfLine >> return ())
    evnt = do
      PC.string "event:"
      PC.skipSpace
      ps <- sta <|> notf <|> del
      PC.endOfLine
      PC.string "data:"
      PC.skipSpace
      rst <- ps
      PC.endOfLine
      case rst
        of
          Error err -> fail err
          Success r -> return r
    sta = PC.string "update" >> return (fmap StreamUpdate . fromJSON <$> json)
    notf = PC.string "notification" >> return (fmap StreamNotify . fromJSON <$> json)
    del = PC.string "delete" >> return (fmap StreamDelete . fromJSON <$> json)
    hatb = PC.char ':' >> oneLine >> return StreamHeartbeat
    errStr = fail "Unknown stream element."
    oneLine = PC.takeWhile1 (\x -> x /= '\n' && x /= '\r')

-- 
-- exported functions
-- 

mkHastodonClient :: String -> String -> String -> String -> String -> IO (Maybe HastodonClient)
mkHastodonClient clientId clientSecret username password host = do
  oauthRes <- getOAuthToken clientId clientSecret username password host
  case oauthRes of
    Left err -> return $ Nothing
    Right oauthData -> return $ Just $ HastodonClient host (accessToken oauthData)

getAccountById :: HastodonClient -> Int -> IO (Either JSONException Account)
getAccountById client id = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pAccountById) client
  return (getResponseBody res :: Either JSONException Account)

getCurrentAccount :: HastodonClient -> IO (Either JSONException Account)
getCurrentAccount client = do
  res <- getHastodonResponseJSON pCurrentAccounts client
  return (getResponseBody res :: Either JSONException Account)

getFollowers :: HastodonClient -> Int -> IO (Either JSONException [Account])
getFollowers client = getFollowersWithOption client mempty

getFollowersWithOption :: HastodonClient -> RangeOption -> Int -> IO (Either JSONException [Account])
getFollowersWithOption client opt id = do
  res <- getHastodonResponseJSONWithOption
           (optionAsQuery opt)
           (replace ":id" (show id) pFollowers)
           client
  return (getResponseBody res :: Either JSONException [Account])

getFollowing :: HastodonClient -> Int -> IO (Either JSONException [Account])
getFollowing client = getFollowingWithOption client mempty

getFollowingWithOption :: HastodonClient -> RangeOption -> Int -> IO (Either JSONException [Account])
getFollowingWithOption client opt id = do
  res <- getHastodonResponseJSONWithOption
           (optionAsQuery opt)
           (replace ":id" (show id) pFollowing)
           client
  return (getResponseBody res :: Either JSONException [Account])

getAccountStatuses :: HastodonClient -> Int -> IO (Either JSONException [Status])
getAccountStatuses client id = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pAccountStatuses) client
  return (getResponseBody res :: Either JSONException [Status])

getRelationships :: HastodonClient -> [Int] ->  IO (Either JSONException [Relationship])
getRelationships client ids = do
  let intIds = map (show) ids
  let params = foldl (\x y -> x ++ (if x == "" then "?" else "&") ++ "id%5b%5d=" ++ y) "" intIds
  res <- getHastodonResponseJSON (pRelationships ++ params) client
  return (getResponseBody res :: Either JSONException [Relationship])

getSearchedAccounts :: HastodonClient -> String ->  IO (Either JSONException [Account])
getSearchedAccounts client query = do
  res <- getHastodonResponseJSON (pSearchAccounts ++ "?q=" ++ query) client
  return (getResponseBody res :: Either JSONException [Account])

postFollow :: HastodonClient -> Int ->  IO (Either JSONException Relationship)
postFollow client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pFollow) [] client
  return (getResponseBody res :: Either JSONException Relationship)

postUnfollow :: HastodonClient -> Int ->  IO (Either JSONException Relationship)
postUnfollow client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pUnfollow) [] client
  return (getResponseBody res :: Either JSONException Relationship)

postBlock :: HastodonClient -> Int ->  IO (Either JSONException Relationship)
postBlock client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pBlock) [] client
  return (getResponseBody res :: Either JSONException Relationship)

postUnblock :: HastodonClient -> Int ->  IO (Either JSONException Relationship)
postUnblock client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pUnblock) [] client
  return (getResponseBody res :: Either JSONException Relationship)

postMute :: HastodonClient -> Int ->  IO (Either JSONException Relationship)
postMute client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pMute) [] client
  return (getResponseBody res :: Either JSONException Relationship)

postUnmute :: HastodonClient -> Int ->  IO (Either JSONException Relationship)
postUnmute client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pUnmute) [] client
  return (getResponseBody res :: Either JSONException Relationship)

postApps :: String -> String -> IO (Either JSONException OAuthClient)
postApps host clientName = do
  let reqBody = [(Char8.pack "client_name", utf8ToChar8 clientName),
                 (Char8.pack "redirect_uris", Char8.pack "urn:ietf:wg:oauth:2.0:oob"),
                 (Char8.pack "scopes", Char8.pack "read write follow")]
  initReq <- parseRequest $ "https://" ++ host ++ pApps
  let req = setRequestBodyURLEncoded reqBody $ initReq
  res <- httpJSONEither req
  return (getResponseBody res :: Either JSONException OAuthClient)

getBlocks :: HastodonClient -> IO (Either JSONException [Account])
getBlocks client = do
  res <- getHastodonResponseJSON pBlocks client
  return (getResponseBody res :: Either JSONException [Account])

getFavorites :: HastodonClient -> IO (Either JSONException [Status])
getFavorites client = do
  res <- getHastodonResponseJSON pFavorites client
  return (getResponseBody res :: Either JSONException [Status])

getFollowRequests :: HastodonClient -> IO (Either JSONException [Account])
getFollowRequests client = do
  res <- getHastodonResponseJSON pFollowRequests client
  return (getResponseBody res :: Either JSONException [Account])

postAuthorizeRequest :: HastodonClient -> Int ->  IO Bool
postAuthorizeRequest client id = postAndGetHastodonResult (replace ":id" (show id) pAuthorizeRequest) [] client

postRejectRequest :: HastodonClient -> Int ->  IO Bool
postRejectRequest client id = postAndGetHastodonResult (replace ":id" (show id) pRejectRequest) [] client

getInstance :: HastodonClient -> IO (Either JSONException Instance)
getInstance client = do
  res <- getHastodonResponseJSON pInstance client
  return (getResponseBody res :: Either JSONException Instance)

getMutes :: HastodonClient -> IO (Either JSONException [Account])
getMutes client = do
  res <- getHastodonResponseJSON pMutes client
  return (getResponseBody res :: Either JSONException [Account])

getNotifications :: HastodonClient -> IO (Either JSONException [Notification])
getNotifications client = getNotificationsWithOption client mempty

getNotificationsWithOption :: HastodonClient -> RangeOption -> IO (Either JSONException [Notification])
getNotificationsWithOption client opt = do
  res <- getHastodonResponseJSONWithOption (optionAsQuery opt) pNotifications client
  return (getResponseBody res :: Either JSONException [Notification])

getNotificationById :: HastodonClient -> Int ->  IO (Either JSONException Notification)
getNotificationById client id = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pNotificationById) client
  return (getResponseBody res :: Either JSONException Notification)

postNotificationsClear :: HastodonClient -> IO Bool
postNotificationsClear = postAndGetHastodonResult pNotificationClear []

getReports :: HastodonClient -> IO (Either JSONException [Report])
getReports client = do
  res <- getHastodonResponseJSON pReports client
  return (getResponseBody res :: Either JSONException [Report])

getSearchedResults :: HastodonClient -> String ->  IO (Either JSONException [Results])
getSearchedResults client = getSearchedResultsWithOption client mempty

getSearchedResultsWithOption ::
  HastodonClient -> LimitOption -> String ->  IO (Either JSONException [Results])
getSearchedResultsWithOption client opt query = do
  res <- getHastodonResponseJSONWithOption (optionAsQuery opt) (pSearch ++ "?q=" ++ query) client
  return (getResponseBody res :: Either JSONException [Results])

getStatus :: HastodonClient -> Int ->  IO (Either JSONException Status)
getStatus client = getStatusWithOption client mempty

getStatusWithOption :: HastodonClient -> GetStatusOption -> Int ->  IO (Either JSONException Status)
getStatusWithOption client opt id = do
  res <- getHastodonResponseJSONWithOption
           (optionAsQuery opt)
           (replace ":id" (show id) pStatus)
           client
  return (getResponseBody res :: Either JSONException Status)

getCard :: HastodonClient -> Int ->  IO (Either JSONException Card)
getCard client id = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pCard) client
  return (getResponseBody res :: Either JSONException Card)

getContext :: HastodonClient -> Int ->  IO (Either JSONException Context)
getContext client id = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pContext) client
  return (getResponseBody res :: Either JSONException Context)

getRebloggedBy :: HastodonClient -> Int ->  IO (Either JSONException [Account])
getRebloggedBy client id = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pRebloggedBy) client
  return (getResponseBody res :: Either JSONException [Account])

getFavoritedBy :: HastodonClient -> Int ->  IO (Either JSONException [Account])
getFavoritedBy client id = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pFavoritedBy) client
  return (getResponseBody res :: Either JSONException [Account])

postStatus :: HastodonClient -> String ->  IO (Either JSONException Status)
postStatus client status = do
  res <- postAndGetHastodonResponseJSON pStatuses [(Char8.pack "status", utf8ToChar8 status)] client
  return (getResponseBody res :: Either JSONException Status)

postStatusWithOption ::
  HastodonClient -> PostStatusOption -> String ->  IO (Either JSONException Status)
postStatusWithOption client opt status = do
  let prms = [(Char8.pack "status", utf8ToChar8 status)] ++ optionAsForm opt
  res <- postAndGetHastodonResponseJSON pStatuses prms client
  return (getResponseBody res :: Either JSONException Status)

postReblog :: HastodonClient -> Int ->  IO (Either JSONException Status)
postReblog client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pReblog) [] client
  return (getResponseBody res :: Either JSONException Status)

postUnreblog :: HastodonClient -> Int ->  IO (Either JSONException Status)
postUnreblog client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pUnreblog) [] client
  return (getResponseBody res :: Either JSONException Status)

postFavorite :: HastodonClient -> Int ->  IO (Either JSONException Status)
postFavorite client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pFavorite) [] client
  return (getResponseBody res :: Either JSONException Status)

postUnfavorite :: HastodonClient -> Int ->  IO (Either JSONException Status)
postUnfavorite client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pUnfavorite) [] client
  return (getResponseBody res :: Either JSONException Status)

getHomeTimeline :: HastodonClient -> IO (Either JSONException [Status])
getHomeTimeline client = getHomeTimelineWithOption client mempty

getHomeTimelineWithOption :: HastodonClient -> DomRangeOption -> IO (Either JSONException [Status])
getHomeTimelineWithOption client opt = do
  res <- getHastodonResponseJSONWithOption (optionAsQuery opt) pHomeTimeline client
  return (getResponseBody res :: Either JSONException [Status])


getPublicTimeline :: HastodonClient -> IO (Either JSONException [Status])
getPublicTimeline client = getPublicTimelineWithOption client mempty

getPublicTimelineWithOption :: HastodonClient -> DomRangeOption -> IO (Either JSONException [Status])
getPublicTimelineWithOption client opt = do
  res <- getHastodonResponseJSONWithOption (optionAsQuery opt) pPublicTimeline client
  return (getResponseBody res :: Either JSONException [Status])

sinkUserTimeline ::
  (MonadIO m, MonadMask m) =>
  HastodonClient -> (Response () -> C.Sink StreamEvent m a) -> m a
sinkUserTimeline = getHastodonResponseSink pUserStream

sourceUserTimeline ::
  (MonadResource m, MonadIO m) =>
  HastodonClient -> C.ConduitM i StreamEvent m ()
sourceUserTimeline client = getHastodonResponseSource pUserStream client getResponseBody

sinkPublicTimeline ::
  (MonadIO m, MonadMask m) =>
  HastodonClient -> (Response () -> C.Sink StreamEvent m a) -> m a
sinkPublicTimeline = getHastodonResponseSink pPublicStream

sourcePublicTimeline ::
  (MonadResource m, MonadIO m) =>
  HastodonClient -> C.ConduitM i StreamEvent m ()
sourcePublicTimeline client = getHastodonResponseSource pPublicStream client getResponseBody

getTaggedTimeline :: HastodonClient -> String ->  IO (Either JSONException [Status])
getTaggedTimeline client = getTaggedTimelineWithOption client mempty

getTaggedTimelineWithOption ::
  HastodonClient -> DomRangeOption -> String ->  IO (Either JSONException [Status])
getTaggedTimelineWithOption client opt hashtag = do
  res <- getHastodonResponseJSONWithOption
           (optionAsQuery opt)
           (replace ":hashtag" hashtag pTaggedTimeline)
           client
  return (getResponseBody res :: Either JSONException [Status])
