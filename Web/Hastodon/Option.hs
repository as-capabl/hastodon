module Web.Hastodon.Option
  (
    optionAsQuery
  , optionAsForm

    -- * Limit Options
  , LimitOption ()
  , IsLimitOption
  , limit

    -- * Range Options
  , RangeOption ()
  , IsRangeOption
  , maxId
  , minId

    -- * Domain & Range Options
  , DomRangeOption ()
  , IsDomRangeOption
  , instanceLocal

    -- * Status getting options
  , GetStatusOption ()
  , IsGetStatusOption
  , onlyMedia
  , excludeReplies

    -- * Status posting options
  , PostStatusOption ()
  , IsPostStatusOption
  , inReplyToId
  , mediaIds
  , sensitive
  , spoilerText
  )
where

import Data.Maybe (fromMaybe)
import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as LChar8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.String.Utils
import qualified Data.Map as Map
import Network.HTTP.Simple
import Network.HTTP.Types.Header

--
-- Utility
--
utf8ToChar8 :: String -> Char8.ByteString
utf8ToChar8 = T.encodeUtf8 . T.pack

--
-- Types and helpers for options
--

-- Left : Array parameter, Right : Single parameter
type OptionVal = Either [Char8.ByteString] (Maybe Char8.ByteString)

type OptionImpl = Map.Map Char8.ByteString OptionVal

class IsOption a where
  fromOptionImpl :: OptionImpl -> a
  toOptionImpl :: a -> OptionImpl

mkOption :: IsOption a => String -> Maybe String -> a
mkOption key val = fromOptionImpl $ Map.singleton (utf8ToChar8 key) (Right $ utf8ToChar8 <$> val)

mkArrayOption :: IsOption a => String -> [String] -> a
mkArrayOption key val = fromOptionImpl $ Map.singleton (utf8ToChar8 key) (Left $ utf8ToChar8 <$> val)

optionMempty :: IsOption a => a
optionMempty = fromOptionImpl Map.empty

optionMappend :: IsOption a => a -> a -> a
optionMappend x y = fromOptionImpl $ Map.union (toOptionImpl x) (toOptionImpl y)

optionAsQuery :: IsOption a => a -> [(Char8.ByteString, Maybe Char8.ByteString)]
optionAsQuery x = do
  (k, v) <- Map.toList $ toOptionImpl x
  let k' = k `mappend` Char8.pack "[]" -- The Rails convention of list query parameter
  case v of
    Left l -> [(k, Just x) | x <- l]
    Right r -> [(k, r)]

optionAsForm :: IsOption a => a -> [(Char8.ByteString, Char8.ByteString)]
optionAsForm opt = fmap cnv $ optionAsQuery opt
  where cnv (x, y) = (x, fromMaybe mempty y)

--
-- Limit option
--
newtype LimitOption = LimitOption { unLimitOption :: OptionImpl } deriving Show

instance IsOption LimitOption where
  fromOptionImpl = LimitOption
  toOptionImpl = unLimitOption

instance Monoid LimitOption where
  mempty = optionMempty
  mappend = optionMappend

class IsOption a => IsLimitOption a where {}

instance IsLimitOption LimitOption where {}

limit :: IsLimitOption a => Int -> a
limit i = mkOption "limit" $ Just (show i)


--
-- Range options
--
newtype RangeOption = RangeOption { unRangeOption :: OptionImpl } deriving Show

instance IsOption RangeOption where
  fromOptionImpl = RangeOption
  toOptionImpl = unRangeOption

instance Monoid RangeOption where
  mempty = optionMempty
  mappend = optionMappend

class IsOption a => IsRangeOption a where {}

instance IsLimitOption RangeOption where {}
instance IsRangeOption RangeOption where {}

minId :: IsRangeOption a => Int -> a
minId i = mkOption "max_id" $ Just (show i)

maxId :: IsRangeOption a => Int -> a
maxId i = mkOption "max_id" $ Just (show i)

--
-- Domain&Range options
--
newtype DomRangeOption = DomRangeOption { unDomRangeOption :: OptionImpl } deriving Show

instance IsOption DomRangeOption where
  fromOptionImpl = DomRangeOption
  toOptionImpl = unDomRangeOption

instance Monoid DomRangeOption where
  mempty = optionMempty
  mappend = optionMappend

class IsOption a => IsDomRangeOption a where {}

instance IsLimitOption DomRangeOption where {}
instance IsRangeOption DomRangeOption where {}
instance IsDomRangeOption DomRangeOption where {}

instanceLocal :: IsDomRangeOption a => a
instanceLocal = mkOption "local" $ Nothing

--
-- Status getting options
--
newtype GetStatusOption = GetStatusOption { unGetStatusOption :: OptionImpl } deriving Show

instance IsOption GetStatusOption where
  fromOptionImpl = GetStatusOption
  toOptionImpl = unGetStatusOption

instance Monoid GetStatusOption where
  mempty = optionMempty
  mappend = optionMappend

class IsOption a => IsGetStatusOption a where {}

instance IsLimitOption GetStatusOption where {}
instance IsRangeOption GetStatusOption where {}
instance IsGetStatusOption GetStatusOption where {}

onlyMedia :: IsGetStatusOption a => a
onlyMedia = mkOption "only_media" Nothing

excludeReplies :: IsGetStatusOption a => a
excludeReplies = mkOption "exclude_replies" Nothing

--
-- Status posting options
--
newtype PostStatusOption = PostStatusOption { unPostStatusOption :: OptionImpl } deriving Show

instance IsOption PostStatusOption where
  fromOptionImpl = PostStatusOption
  toOptionImpl = unPostStatusOption

instance Monoid PostStatusOption where
  mempty = optionMempty
  mappend = optionMappend

class IsOption a => IsPostStatusOption a where {}

instance IsLimitOption PostStatusOption where {}
instance IsRangeOption PostStatusOption where {}
instance IsPostStatusOption PostStatusOption where {}

data Visibility = V_Direct | V_Private | V_Unlisted | V_Public deriving (Eq, Show)

formatVis :: Visibility -> String
formatVis V_Direct = "direct"
formatVis V_Private = "private"
formatVis V_Unlisted = "unlisted"
formatVis V_Public = "public"

inReplyToId :: IsPostStatusOption a => Int -> a
inReplyToId i = mkOption "in_reply_to_id" (Just $ show i)

mediaIds :: IsPostStatusOption a => [Int] -> a
mediaIds l = mkArrayOption "media_ids" $ show <$> l

sensitive :: IsPostStatusOption a => a
sensitive = mkOption "sensitive" Nothing

spoilerText :: IsPostStatusOption a => String -> a
spoilerText str = mkOption "spoiler_text" (Just str)

visibility :: IsPostStatusOption a => Visibility -> a
visibility vis = mkOption "visibility" (Just $ formatVis vis)




