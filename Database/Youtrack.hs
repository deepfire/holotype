{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Youtrack
    -- (
    -- )
where


-- Base imports
import           Control.Applicative         ((<|>))
import           Control.Monad               (join)
import           Control.Monad.Reader
import           Data.List
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 ((<>))
import           GHC.Exts                    (Constraint)
import           GHC.Generics                (Generic(Rep))
import           GHC.TypeLits                (Symbol)
import           Prelude.Unicode


-- Debug imports
import           Text.Printf                 (printf)
import           Debug.Trace                 (trace)


-- External imports
import           Control.Lens         hiding (from)
import           Data.Aeson                  (FromJSON (..), Value, (.:))
import           Data.Aeson.Types            (Options (..))
import           Data.Scientific             (Scientific)
import           Data.Time.LocalTime         (LocalTime (..), utcToLocalTime, hoursToTimeZone)
import           Data.Time.Clock.POSIX       (posixSecondsToUTCTime)
import           Network.HTTP.Client         (CookieJar)
import           Network.Wreq                (FormParam((:=)))
import           OpenSSL                     (withOpenSSL)
import           OpenSSL.Session             (SSLContext)

import qualified Data.Aeson                   as AE
import qualified Data.Aeson.Types             as AE
import qualified Data.ByteString.Lazy.UTF8    as BL
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.UTF8         as BS
import qualified Data.HashMap.Strict          as HM
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import qualified Network.HTTP.Client.OpenSSL  as SSL
import qualified Network.Wreq                 as WR
import qualified OpenSSL.Session              as SSL
import qualified Options.Applicative          as O
import qualified Options.Generic              as O
import qualified System.Environment
import qualified Text.Parser.Char             as P
import qualified Text.Parser.Combinators      as P
import qualified Text.Parser.Token            as P
import qualified Text.Trifecta.Combinators    as P
import qualified Text.Trifecta.Delta          as P
import qualified Text.Trifecta.Parser         as P


-- * Credentials storage: ~/.authinfo (ex-hsauthinfo) -- TODO:
gettok ∷ (Monad p, P.TokenParsing p) ⇒ Bool → String → p a → p a
gettok lastp key val = do
  P.string key
  (P.skipSome $ P.oneOf " \t") P.<?> "non-breaking whitespace"
  ret ← val
  (if lastp then P.skipMany else P.skipSome) $ P.oneOf " \t"
  pure ret

readAuthInfoLine ∷ (Monad p, P.TokenParsing p) ⇒ p (String, String, String, Maybe String)
readAuthInfoLine = do
  P.whiteSpace
  machine  ←              gettok False "machine"  $ P.some $ P.alphaNum <|> P.oneOf "-."
  login    ←              gettok False "login"    $ P.some P.alphaNum
  mport    ← P.optional $ gettok False "port"     $ P.some P.alphaNum
  password ←              gettok True  "password" $ P.some $ P.noneOf " \t\n\r"
  pure (machine, login, password, mport)

readAuthInfo ∷ (Monad p, P.TokenParsing p) ⇒ p [(String, String, String, Maybe String)]
readAuthInfo = do
  lines ← P.sepEndBy readAuthInfoLine (P.newline)
  P.whiteSpace
  P.eof
  pure lines

readAuthInfoFile ∷ String → IO (Maybe [(String, String, String, Maybe String)])
readAuthInfoFile aipath = do
  P.parseFromFile readAuthInfo aipath

getPassword ∷ Maybe String → String → String → IO (Maybe (String, Maybe String))
getPassword maipath host user = do
  aipath ← case maipath of
             Nothing → fmap (<> "/.authinfo") $ System.Environment.getEnv "HOME"
             Just p  → pure p
  mainfo ← readAuthInfoFile aipath
  pure $ join $ fmap (fmap (\ (_,_,pass,port) -> (pass,port))
                     ∘
                      find (\ (h,u,_,_) -> h == host && u == user))
                mainfo


-- * SSL shenanigans
data SSLOptions = SSLOptions {
      optionsClientCert ∷ FilePath
    , optionsCaCert     ∷ FilePath
    } deriving (Eq, Show)

setupSSLCtx ∷ SSLOptions → IO SSLContext
setupSSLCtx (SSLOptions _ caCert) = do
  ctx ← SSL.context
  SSL.contextSetCertificateFile ctx caCert
  SSL.contextAddOption          ctx SSL.SSL_OP_NO_SSLv2
  SSL.contextAddOption          ctx SSL.SSL_OP_NO_SSLv3
  pure ctx


-- * Accessing YouTrack
data YTAccess where
    YTAccess ∷ {
      hostname ∷ String
    , ssl_opts ∷ SSLOptions
    , login    ∷ String
    , password ∷ String
    } → YTAccess

data YT where
    YT ∷ {
      access       ∷ YTAccess
    , wreq_options ∷ WR.Options
    , jar          ∷ CookieJar
    } → YT

complete_yt_access ∷ YTAccess → Maybe String → IO YTAccess
complete_yt_access yta maipath = do
    macc ← getPassword maipath (hostname yta) (login yta)
    case macc of
      Nothing        → error $ printf "ERROR: no password for username '%s' on host '%s'." (login yta) (hostname yta)
      Just (pass, _) → pure $ yta { password = pass }

yt_connect ∷ YTAccess → Maybe String → IO YT
yt_connect access maipath = do
  full_access ← complete_yt_access access maipath
  let (YTAccess host ssl_opts login password) = full_access
  let mkOpts c  = WR.defaults & ( (WR.manager .~ Left (SSL.opensslManagerSettings c))
                                ∘ (WR.auth ?~ WR.basicAuth (BS.fromString login) (BS.fromString password)))
      wreq_opts = mkOpts (setupSSLCtx ssl_opts)
  withOpenSSL $ do
    r ← WR.postWith wreq_opts
        ("https://" <> host <> "/rest/user/login")
        ["login" := login, "password" := password]
    pure $ YT access wreq_opts $ r ^. WR.responseCookieJar


-- * Leaf types
newtype_from_JSON,recdrop1_from_JSON ∷ (Generic a, AE.GFromJSON (Rep a)) ⇒ Value → AE.Parser a
newtype_from_JSON  = AE.genericParseJSON (AE.defaultOptions { unwrapUnaryRecords = True })
recdrop1_from_JSON = AE.genericParseJSON (AE.defaultOptions { fieldLabelModifier = drop 1 })

newtype URLPath      = URLPath      { fromURLPath      ∷ String } deriving Show

newtype Hours        = Hours      { from ∷ Int }     deriving (Generic, Show, Num, FromJSON)
newtype Filter       = Filter     { from ∷ String }  deriving (Generic, Show)
newtype Field        = Field      { field ∷ String } deriving (Generic, Show)

newtype PName        = PName      { from ∷ String } deriving (Generic, Show)
instance FromJSON      PName                        where parseJSON = newtype_from_JSON
newtype PAlias       = PAlias     { from ∷ String } deriving (Generic, Show, O.ParseRecord, O.ParseFields, O.ParseField, Read)
instance FromJSON      PAlias                       where parseJSON = newtype_from_JSON
newtype PVersion     = PVersion   { from ∷ String } deriving (Generic, Show)
instance FromJSON      PVersion                     where parseJSON = newtype_from_JSON
newtype PVersionList = PVersionList [String]        deriving (Generic, Show)
instance FromJSON      PVersionList                 where
    parseJSON (AE.Null)      = pure $ PVersionList []
    parseJSON v@(AE.Array _) = AE.genericParseJSON AE.defaultOptions v
    parseJSON v              = fail $ printf "unexpected value for a version list: %s" (show v)
interpret_strdate ∷ T.Text → LocalTime
interpret_strdate = utcToLocalTime (hoursToTimeZone 4) ∘ posixSecondsToUTCTime ∘ fromIntegral ∘ floor @Double @Int ∘ (/ 1000.0) ∘ read @Double ∘ T.unpack
newtype PDate        = PDate      { from ∷ LocalTime } deriving (Generic, Show)
instance FromJSON      PDate                        where parseJSON = AE.withText "date" $ \n → do
                                                                      pure ∘ PDate $ interpret_strdate n
newtype ICreated     = ICreated   { from ∷ LocalTime } deriving (Generic, Show)
instance FromJSON      ICreated                     where parseJSON = AE.withText "created date" $ \n → do
                                                                      pure ∘ ICreated $ interpret_strdate n
newtype IId          = IId        { from ∷ String } deriving (Generic, Show)
instance FromJSON      IId                          where parseJSON = newtype_from_JSON
newtype IPriority    = IPriority  { from ∷ String } deriving (Generic, Show)
instance FromJSON      IPriority                    where parseJSON = newtype_from_JSON
newtype IResolved    = IResolved  { from ∷ LocalTime } deriving (Generic, Show)
instance FromJSON      IResolved                    where parseJSON = AE.withText "resolved date" $ \n → do
                                                                        pure ∘ IResolved $ interpret_strdate n
newtype IState       = IState     { from ∷ String } deriving (Generic, Show)
instance FromJSON      IState                       where parseJSON = newtype_from_JSON
newtype ITag         = ITag       { from ∷ String } deriving (Generic, Show) -- XXX: should be derivable
instance FromJSON      ITag                         where parseJSON = AE.withObject "issue tag" $ \o →
                                                                      ITag <$> o .: "value"
newtype ITitle       = ITitle     { from ∷ String } deriving (Generic, Show)
instance FromJSON      ITitle                       where parseJSON = newtype_from_JSON
newtype IType        = IType      { from ∷ String } deriving (Generic, Show)
instance FromJSON      IType                        where parseJSON = newtype_from_JSON
newtype IUpdated     = IUpdated   { from ∷ LocalTime } deriving (Generic, Show)
instance FromJSON      IUpdated                     where parseJSON = AE.withText "updated date" $ \n → do
                                                                        pure ∘ IUpdated $ interpret_strdate n
newtype MLogin       = MLogin     { from ∷ String } deriving (Generic, Show, Eq)
instance FromJSON      MLogin where
    parseJSON (AE.String s) = pure ∘ MLogin $ T.unpack s
    -- ,("author",Object (fromList [("ringId",String "58694c6b-d9cd-4ab1-bae4-e01db8516d44"),("url",String "https://gra-tracker.ptsecurity.com/rest/admin/user/skovalev"),("login",String "skovalev")]))
    parseJSON (AE.Object o) =
      case HM.lookup "login" o <|> HM.lookup "value" o of
        Just (AE.String s) → pure ∘ MLogin $ T.unpack s
        _                  → fail $ printf "Not a ∈ login object: %s" $ show o
    parseJSON o             = fail $ printf "Not a ∈ login object: %s" $ show o
newtype MFullName    = MFullName  { from ∷ String } deriving (Generic, Show) -- XXX: should be derivable
instance FromJSON      MFullName                    where parseJSON = AE.withObject "∈ full name" $ \o →
                                                                      MFullName <$> o .: "value"
-- ("worktype",Object (fromList [("url",String "https://gra-tracker.ptsecurity.com/rest/admin/timetracking/worktype/38-3"),("name",String "Bughunt"),("id",String "38-3"),("autoAttached",Bool False)]))
newtype WType        = WType      { from ∷ String } deriving (Generic, Show)
instance FromJSON      WType                        where parseJSON = AE.withObject "work type" $ \o →
                                                                      WType <$> o .: "name"

interpret_scidate ∷ Scientific → LocalTime
interpret_scidate = utcToLocalTime (hoursToTimeZone 4) ∘ posixSecondsToUTCTime ∘ fromIntegral ∘ floor @Scientific @Integer ∘ (/ 1000)
newtype WDate        = WDate      { from ∷ LocalTime } deriving (Generic, Show, Eq, Ord)
instance FromJSON      WDate                        where parseJSON = AE.withScientific "work date" $ \n → do
                                                                        pure ∘ WDate $ interpret_scidate n
                                                                      -- WDate <$> (interpret_wdate $ o .: "name")

data ILink =
    ILink { _value ∷ String
          , _role  ∷ String
          , _type  ∷ String }
    deriving (Generic, Show)
instance FromJSON   ILink                       where parseJSON = recdrop1_from_JSON

abbrev_object ∷ Value → Value
abbrev_object (AE.Object map) = AE.Object $ (flip HM.mapWithKey) map
                                            (\k v → case k of
                                                      "name" → v
                                                      _      → abbrev_object v)
abbrev_object (AE.String s)   = AE.String $ T.take 3 s
abbrev_object (AE.Array  xs)  = AE.Array $ fmap abbrev_object xs
abbrev_object x               = x


-- * Data model

data Project =
    Project {
      _alias               ∷ PAlias
    , _name                ∷ PName
    , _members             ∷ [Member] -- initially empty
    } deriving (Generic, Show)

data Member =
    Member {
      _login               ∷ MLogin
    , _name                ∷ MFullName
    } deriving (Generic, Show)

-- XXX: hard-coded assumptions about project issue structure
--      ..to be resolved by "2014 Bahr - Composing and Decomposing Data Types"
data Issue =
    Issue {
      _id                  ∷ IId
    , _summary             ∷ ITitle
    , _type                ∷ IType        --
    , _priority            ∷ IPriority    --
    , _author              ∷ Member
    , _assignee            ∷ Maybe Member --
    , _state               ∷ IState       --
    , _created             ∷ LocalTime
    , _resolved            ∷ LocalTime
    , _updated             ∷ LocalTime
    , _estimation          ∷ Maybe Hours  --
    , _description         ∷ Maybe String
    , _votes               ∷ Int
    , _links               ∷ [ILink]
    , _tags                ∷ [ITag]
    } deriving (Generic, Show)

data WorkItem =
    WorkItem {
      _id                  ∷ String
    , _type                ∷ Maybe WType
    , _date                ∷ LocalTime
    , _author              ∷ Member
    , _duration            ∷ Hours
    , _description         ∷ Maybe String
    } deriving (Generic, Show)

-- data Label (l ∷ Symbol) = Get
-- class Has a l b | a l → b where
--   fromL ∷ a → Label l → b
-- data Point = Point Int Int deriving Show
-- instance Has Point "x" Int where from (Point x _) _ = x
-- instance Has Point "y" Int where from (Point _ y) _ = y
-- example = from (Point 1 2) (Get :: Label "x")


-- * FromJSON

instance FromJSON    Project where { parseJSON
    = AE.withObject "Project" $
      \o → do
        _alias               ← o .: "shortName"
        _name                ← o .: "name"
        logins ∷ [MLogin]    ← o .: "assigneesLogin"
        names  ∷ [MFullName] ← o .: "assigneesFullName"
        let _members = fmap (\(l, n) → Member l n) $ zip logins names
        pure Project{..}; }

value_map_lookup ∷ String → HM.HashMap T.Text AE.Value → T.Text → AE.Value
value_map_lookup desc hm key =
    HM.lookupDefault (error $ printf "missing %s: %s" desc $ show key) key hm

value_lookup ∷ String → AE.Value → T.Text → AE.Value
value_lookup desc (AE.Object o) key =
    value_map_lookup desc o key
value_lookup desc  val          key =
    error $ printf "while looking for key %s in %s: got non-object %s" (show key) desc $ show val

lookup_member ∷ [Member] → MLogin → Member
lookup_member ms ((flip find) ms ∘ (\l m → l ≡ _login m) → Just m) = m
lookup_member _ ml = error $ printf "Couldn't find project ∈ with login name '%s'." $ from (ml ∷ MLogin)

issue_fields_lookup ∷ V.Vector AE.Value → T.Text → Maybe AE.Value
issue_fields_lookup xs key =
    let getf ∷ HM.HashMap T.Text AE.Value → T.Text → AE.Value
        getf o f = HM.lookupDefault (error $ printf "missing field element: %s" $ show f) f o
    in (flip $ (flip V.foldl) Nothing) xs $
       \mr o@(AE.Object _) →
           case (mr, value_lookup "field name" o "name", value_lookup "field value" o "value") of
             (r@(Just _), _, _)                    → r
             (_, AE.String name, val) | name ≡ key → Just val
             _                                     → Nothing

issue_value_field ∷ Value → T.Text → Maybe AE.Value
issue_value_field o@(AE.Object _) fld =
    let AE.Array xs = value_lookup "issue list fields" o "field"
    in issue_fields_lookup xs fld

instance FromJSON ([Reader Project Issue]) where
    parseJSON = AE.withObject "Issue wrapper" $
       \o → case value_map_lookup "Issue list" o "issue" of
              AE.Array xs → forM (V.toList xs) parseJSON
              erx         → error $ printf "Not an issue list in the 'value' element of issue list."

instance FromJSON (Reader Project Issue) where
    parseJSON = AE.withObject "Issue" $
       \o → do
        let fields ∷ V.Vector AE.Value
            fields = case value_map_lookup "issue field list" o "field" of
                       AE.Array xs → xs
                       erx         → error $ printf "Not a field list in the 'field' issue element, got %s instead." $ show erx
            missing ∷ String → String → String
            missing t v = printf "missing Issue field %s: %s" t v
            field ∷ T.Text → (T.Text, Maybe Value)
            field fld = (,) fld $ issue_fields_lookup fields fld
            mget ∷ (FromJSON a) ⇒ (T.Text, Maybe Value) → AE.Parser (Maybe a)
            mget (fld, mvalue) =
                case mvalue of
                  Just val
                      → case AE.fromJSON val of
                          AE.Success res' → pure res'
                          AE.Error   e    → fail $ printf "while parsing field %s: %s, obj=%s" fld e (show val)
                  _   → pure Nothing
            get ∷ (FromJSON a) ⇒ (T.Text, Maybe Value) → AE.Parser a
            get (fld, mvalue) =
                case mvalue of
                  Just val
                      → case AE.fromJSON val of
                          AE.Success res' → pure res'
                          AE.Error   e    → fail $ printf "while parsing field %s: %s, obj=%s" fld e (show val)
                  _   → fail $ missing "field" $ T.unpack fld
        (iid ∷ String) ← get $ field "numberInProject"
        _summary       ← get $ field "summary"
        types          ← mget $ field "Type"
        let _type      = head $ (fromMaybe [] types) <|> [IType "No Type"] -- XXX: per-project defaulting
        priority       ← get $ field "Priority"
        let _priority  = head $ priority <|> [IPriority "No Priority"] -- XXX: per-project defaulting
        author   ∷ String
                       ← get $ field "reporterName"
        massignee ∷ Maybe String
                       ← mget $ field "Assignee"
        state          ← get $ field "State"
        let _state     = head $ state <|> [IState "No State"] -- XXX: per-project defaulting
        PDate _created ← get $ field "created"
        PDate _resolved ← get $ field "resolved"
        PDate _updated ← get $ field "updated"
        _description   ← mget $ field "description"
        _estimation    ← mget $ field "Estimation"
        votes          ← mget $ field "votes"
        let _votes     = fromMaybe 0 votes
        links          ← mget $ field "links"
        let _links     = fromMaybe [] links
        _tags          ← o .: "tag"
        pure $ do
              Project{ _alias, _members } ← ask
              let _id        = IId $ printf "%s-%s" (from (_alias ∷ PAlias)) iid
              let _author    = lookup_member _members $ MLogin author
              let _assignee  = fmap (lookup_member _members ∘ MLogin) massignee
              pure Issue{..};
        -- let fields = constrFields $ head $ dataTypeConstrs (dataTypeOf ((⊥) ∷ IFields))

instance FromJSON    (Reader Project WorkItem) where { parseJSON
    = AE.withObject "WorkItem" $
      \o → do
        _id              ← o .: "id"
        _type            ← o .: "worktype"
        WDate _date      ← o .: "date"
        author           ← o .: "author"
        (duration ∷ Int) ← o .: "duration"
        let _duration    = Hours $ floor @Double @Int $ fromIntegral duration / 60.0
        _description   ← o .: "description"
        pure $ do
             Project{ _members } ← ask
             let _author = lookup_member _members author
             pure WorkItem{..}; }


-- * Generic Exchange/RR (request/response) machinery
class (ShowC (Request q), JSONC (Response q)) ⇒ YTExchange (q ∷ YTExchanges) where
    data Request  q   ∷ *
    type Response q   ∷ *
    request_urlpath   ∷ Request q → URLPath
    request_params    ∷ Request q → WR.Options → WR.Options
    request_params _  = id
type family   JSONC c ∷ Constraint
type instance JSONC c = (Generic c, FromJSON c)
type family   ShowC c ∷ Constraint
type instance ShowC c = (Show c)

data YTExchanges
    = EProjectAll
    | EIssueByProject
    | EIssue
    | EIssueTTWItem


-- * /project/all?{verbose}
--   https://confluence.jetbrains.com/display/YTD65/Get+Accessible+Projects
instance YTExchange EProjectAll where
    data   Request  EProjectAll  = RProjectAll deriving Show
    type   Response EProjectAll  = [Project]
    request_urlpath RProjectAll =
        URLPath "/project/all"
    request_params  RProjectAll params =
        params & WR.param "verbose" .~ ["true"]


-- * /issue/byproject/{project}?{filter}&{after}&{max}&{updatedAfter}&{wikifyDescription}
--   https://confluence.jetbrains.com/display/YTD65/Get+Issues+in+a+Project
--   Note, this seems a bit useless, because it doesn't allow to get custom fields
--   in one request.

-- instance YTExchange EIssueByProject where
--     data   Request  EIssueByProject = RIssueByProject PAlias Filter deriving Show
--     type   Response EIssueByProject = [Issue]
--     request_urlpath (RIssueByProject (PAlias alias) query) =
--         URLPath $ "/issue/byproject/" <> alias
--     request_params  (RIssueByProject (PAlias alias) (Filter query)) params =
--         params & if not $ Data.List.null query
--                  then WR.param "query" .~ [T.pack query]
--                  else id


-- * /issue?{filter}&{with}&{max}&{after}
--   https://confluence.jetbrains.com/display/YTD65/Get+the+List+of+Issues
newtype RIssueWrapper = RIssueWrapper { issue ∷ [Reader Project Issue] } deriving (Generic)
instance FromJSON       RIssueWrapper where parseJSON = newtype_from_JSON
instance YTExchange EIssue where
    data   Request  EIssue = RIssue Filter {-ignored-} Int {-ignored-} [Field] deriving Show
    type   Response EIssue = [Reader Project Issue]
    request_urlpath (RIssue _ _ _) =
        URLPath $ "/issue"
    request_params  (RIssue (Filter query) max fields) params =
        params & WR.param "filter" .~ [T.pack query]
               & WR.param "max"    .~ [T.pack $ printf "%d" max]
               & WR.param "with"   .~ case fmap (T.pack ∘ field) fields of
                                        [] → [""]
                                        xs → xs

-- * /issue/{issue}/timetracking/workitem/
--   https://confluence.jetbrains.com/display/YTD65/Get+Available+Work+Items+of+Issue
instance YTExchange EIssueTTWItem where
    data   Request  EIssueTTWItem  = RIssueTTWItem IId deriving Show
    type   Response EIssueTTWItem  = [Reader Project WorkItem]
    request_urlpath (RIssueTTWItem (IId iid)) =
        URLPath $ "/issue/" <> iid <> "/timetracking/workitem/"



-- * Request IO machinery
yt_req_raw ∷ (YTExchange y) ⇒ YT → Request y → IO BL.ByteString
yt_req_raw (YT (YTAccess hostname ssl_opts _ _) wreq_opts jar) req = do
  let url    = "https://" <> hostname <> "/rest" <> (fromURLPath $ request_urlpath req)
      params = request_params req
      opts   = wreq_opts & (WR.header "Accept" .~ ["application/json"]) & params
  printf "--> %s %s\n" url (show $ opts ^. WR.params )
  r ← withOpenSSL (WR.getWith opts url)
  printf "<-- %s\n" $ show $ r ^. WR.responseBody
  pure $ r ^. WR.responseBody

yt_req ∷ (YTExchange y, FromJSON (Response y)) ⇒ YT → Request y → IO (Response y)
yt_req yt req = do
  resp ← yt_req_raw yt req
  case AE.eitherDecode resp of
    Left e     → error e
    Right resp → pure resp

yt_req_save ∷ (YTExchange y) ⇒ YT → Request y → IO ()
yt_req_save yt req = do
  bs ← yt_req_raw yt req
  BL.writeFile (take 64 $ "yt-request-" <> show req) bs

yt_req_load ∷ (YTExchange y, FromJSON r) ⇒ Request y → IO r
yt_req_load req = do
  resp ← BL.readFile (take 64 $ "yt-request-" <> show req)
  case AE.eitherDecode resp of
    Left e     → error e
    Right resp → pure resp

yt_req_load_value ∷ (YTExchange y) ⇒ Request y → IO Value
yt_req_load_value = yt_req_load
yt_req_load_response ∷ (YTExchange y, FromJSON (Response y)) ⇒ Request y → IO (Response y)
yt_req_load_response = yt_req_load
