{-# LANGUAGE Arrows #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE SignatureSections #-} 8.2
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}


-- Base imports
import           Data.Data
import           Data.Map as Map
import           GHC.Exts                    (Constraint)
import           GHC.Generics                (Generic(Rep))
import           GHC.OverloadedLabels
import           Prelude.Unicode


-- Debug imports
import           Text.Printf                 (printf)
import           Debug.Trace                 (trace)


-- External imports
import           Control.Applicative
import           Control.Lens         hiding (from)
import           Control.Monad
import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.Reader
import           Data.Aeson                  (FromJSON (..), Value, (.:))
import qualified Data.Aeson as AE
import           Data.Aeson.Types            (Options (..))
import qualified Data.Aeson.Types as AE
import           Data.Aeson.Lens             (_String, key)
import           Data.Attoparsec.Text        ((<?>), char, skipSpace, peekChar, peekChar', decimal)
import qualified Data.ByteString.Lazy.UTF8    as BL
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.UTF8 as BS
import           Data.Char
import qualified Data.Csv as CSV
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.String                 (IsString)
import           Data.Scientific             (Scientific)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Calendar          (toGregorian)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.LocalTime
import qualified Data.Vector as V
--import           Data.String.Interpolate
import           Network
import           Network.HTTP.Client         (CookieJar)
import qualified Network.HTTP.Client.OpenSSL as SSL
import           Network.Wreq                (FormParam((:=)))
import           Network.Wreq.Types          (Postable)
import           OpenSSL                     (withOpenSSL)
import           OpenSSL.Session             (SSLContext)
import qualified OpenSSL.Session as SSL
import           Options.Applicative
import           Options.Generic
import qualified System.Authinfo
import qualified System.Environment

import qualified Text.Parser.Char as P
import qualified Text.Parser.Combinators as P
import qualified Text.Parser.Token as P
import qualified Text.Trifecta.Combinators as P
import qualified Text.Trifecta.Delta as P
import qualified Text.Trifecta.Parser as P


-- Local imports
import Database.Mood.Types
import Database.Youtrack


-- P.parseString (do { P.sepBy readAuthInfoLine (P.newline); }) (P.Columns 0 0) "machine m login l port p password pw\n"
-- abbrev_object r
-- let {(AE.Object v) = (ar V.! 0); (AE.Array xs) = HM.lookupDefault undefined "field" v; } in xs V.! 0
-- issue_value_field (ar V.! 0) "Priority"
-- AE.parse (const (AE.genericParseJSON (AE.defaultOptions { unwrapUnaryRecords = True }) (AE.String $ T.pack "lol"))) () :: AE.Result IPriority

-- * Full go: Request → IO
-- (flip yt_req)      (RIssueByProject (PAlias "GRA") (Filter "")) =<< yt_connect gra_yt_access Nothing
-- (flip yt_req)      (RIssue          (Filter "") 1)              =<< yt_connect gra_yt_access Nothing

-- * Save: Request → IO ()
-- (flip yt_req_save) (RProjectAll)                                =<< yt_connect gra_yt_access Nothing
-- (flip yt_req_save) (RIssueByProject (PAlias "GRA") (Filter "")) =<< yt_connect gra_yt_access Nothing
-- (flip yt_req_save) (RIssue (Filter "") 1)                       =<< yt_connect gra_yt_access Nothing
-- (flip yt_req_save) (RIssueTTWItem (IId "GRA-20"))  =<< yt_connect gra_yt_access Nothing

-- * Load: Request → IO Value
-- v             <- yt_req_load_value $ RProjectAll
-- (AE.Array ar) <- yt_req_load_value $ RIssueByProject (PAlias "GRA") (Filter "")
-- v             <- yt_req_load_value $ RIssue (Filter "") 1

-- * Load: Request → IO Response
--                  yt_req_load_response $ RProjectAll
--                  yt_req_load_response $ RIssueByProject (PAlias "GRA") $ Filter ""
--                  yt_req_load_response $ RIssue (Filter "") 1
--                  yt_req_load_response $ RIssueTTWItem $ IId "GRA-20"

-- * Interpretation: Value → internal value
-- (AE.fromJSON $ ar V.! 0 :: AE.Result (RespElem RIssueByProject))
-- (AE.fromJSON v :: AE.Result (Response ProjectAll))
-- (AE.fromJSON v :: AE.Result (Response Issue))


data ReportParams where
    ReportParams
        ∷ {
    -- Access
      yt_hostname          ∷ String
    , yt_sslcacert_file    ∷ String
    , yt_login             ∷ String
    , yt_authinfo_file     ∷ Maybe String
    -- Project
    , project_id           ∷ String
    , project_areapath     ∷ String
    , project_product      ∷ String
    -- Report
    , report_period        ∷ Maybe String
    , report_resolvedstate ∷ Maybe String
    -- Output
    , tasks_output         ∷ Maybe FilePath
    , workitems_output     ∷ Maybe FilePath
    } → ReportParams
    deriving (Generic, Show)
instance ParseRecord ReportParams

instance CSV.ToField LocalTime where toField lt = CSV.toField $ show lt

data ReportDeliveredWorkItem where
    ReportDeliveredWorkItem ∷
     { rdwi_id             ∷ String         -- issue         _id                  ;+
     , rdwi_performed_date ∷ String         -- workitem      _date                ;+
     , rdwi_performed_by   ∷ String         -- workitem      _author              ;+
     , rdwi_completed_work ∷ Int            -- workitem      _duration            ;+
     } → ReportDeliveredWorkItem
    deriving (Generic, Show)
instance CSV.ToNamedRecord  ReportDeliveredWorkItem
instance CSV.DefaultOrdered ReportDeliveredWorkItem

data ReportDeliveredIssue where
    ReportDeliveredIssue ∷
     { rdi_id             ∷ String          -- issue         _id                  ;+
     , rdi_type           ∷ String          -- issue         _Type                ;+
     , rdi_created_date   ∷ String          -- issue         _created             ;+
     , rdi_created_by     ∷ String -- Full! -- issue         _reporterName        ;+
     , rdi_start_date     ∷ String          -- 1st workitem  _date                ;i
     , rdi_closed_date    ∷ String          -- issue         _resolved            ;+
     , rdi_release_date   ∷ Maybe LocalTime -- issue         ???                  ;c?
     , rdi_title          ∷ String          -- issue         _summary             ;+
     , rdi_state          ∷ String          -- params        report_resolvedstate ;+
     , rdi_area_path      ∷ String          -- params        project_areapath     ;+
     , rdi_product        ∷ String          -- params        project_product      ;+
     , rdi_estimation     ∷ Maybe Int       -- issue         ???                  ;r?
     , rdi_completed_work ∷ Int             -- workitem      _duration            ;+
     , rdi_feature_id     ∷ Maybe String    -- issue         ???                  ;c?
     } → ReportDeliveredIssue
    deriving (Generic, Show)
instance CSV.ToNamedRecord  ReportDeliveredIssue
instance CSV.DefaultOrdered ReportDeliveredIssue

print_localtime_date ∷ LocalTime → String
print_localtime_date (toGregorian ∘ localDay → (y, m, d)) =
    printf "%02d.%02d.%04d" d m y

login_fullname ∷ [(MLogin, MFullName)] → String → String
login_fullname usermap login =
    from ((fromMaybe (error $ printf "Unknown user: %s" $ show login) $ Data.List.lookup (MLogin login)
                     usermap)
          ∷ MFullName)

issue_report ∷ ReportParams
             → Project
             → Issue
             → [WorkItem]
             → (,) ReportDeliveredIssue [ReportDeliveredWorkItem]
issue_report ReportParams{..} Project{_alias} Issue{..} wis =
    let fld ∷ String → Maybe a → a
        fld desc = fromMaybe (error $ printf "%s: missing field '%s'" (from (_id ∷ IId)) desc)
        wis_sorted = sortBy (\x y → compare (_date (x ∷ WorkItem)) (_date (y ∷ WorkItem)))
                     wis
        report_wis = [ ReportDeliveredWorkItem
                       (from (_id ∷ IId))
                       (print_localtime_date date)
                       (from (_name (author ∷ Member) ∷ MFullName))
                       (from (dur ∷ Hours))
                     | WorkItem _ _ date author dur _
                         ← wis_sorted ]
        started_date = case wis_sorted of
                         x:_ → _date (x ∷ WorkItem)
                         []  → _created
    in (,)
       (ReportDeliveredIssue
        (from (_id ∷ IId))
        (from (_type ∷ IType))
        (print_localtime_date _created)
        (from (_name (_author ∷ Member) ∷ MFullName))
        (print_localtime_date started_date)
        (print_localtime_date _resolved)
        Nothing                                          -- release
        (from (_summary ∷ ITitle))
        (from (_state ∷ IState))
        project_areapath
        project_product
        Nothing                                          -- estimation
        (sum $ fmap rdwi_completed_work report_wis)
        Nothing)
       report_wis

-- :main --yt_hostname gra-tracker.ptsecurity.ru --yt_sslcacert_file /home/deepfire/.cert/PTRootCA.crt --yt_login skosyrev --project_id GRA --project_areapath Gvandra --project_product Gvandra
main ∷ IO ()
main = do
  args   ← getRecord @IO @ReportParams "YouTrack report extractor"
  now    ← getCurrentTime
  let ReportParams{..} = args
      access = YTAccess { hostname = yt_hostname
                        , ssl_opts = SSLOptions "" yt_sslcacert_file
                        , login    = yt_login }
      (year, month, day) = toGregorian $ utctDay now
      period = fromMaybe (printf "%d-%02d" year (month - 1)) report_period
      resolved_state = fromMaybe "Closed" report_resolvedstate

  printf "-- args: %s\n" $ show args

  putStrLn "-- 0: authorizing.."
  yt ← yt_connect access yt_authinfo_file

  putStrLn "-- 1: obtaining project info.."
  projects ← yt_req yt RProjectAll
  let proj = fromMaybe (error $ printf "server %s doesn't have project nicknamed '%s' among: %s"
                        yt_hostname project_id
                        (show $ fmap (\Project{..} → from (_alias ∷ PAlias)) projects)) $
             find (\Project{..} → project_id ≡ from (_alias ∷ PAlias)) projects
      Project{..} = proj
  printf "--- total %d users:\n" $ length _members
  forM_ _members $ \(Member {_login, _name}) → do
         printf "%12s: %s\n" (from (_login ∷ MLogin)) (from (_name ∷ MFullName))

  putStrLn "-- 2: saving issue list.."
  -- (flip yt_req) (RIssue (Filter $ "resolved date: " <> "2016-02") 100 []) =<< yt_connect (YTAccess { hostname ="gra-tracker.ptsecurity.ru", ssl_opts = SSLOptions "" "/home/deepfire/.cert/PTRootCA.crt", login = "skosyrev" }) Nothing
  let req         = RIssue (Filter $ "resolved date: " <> period) 100 $ fmap Field
                    ["numberInProject", "created", "updated", "description", "Estimation", "reporterName", "resolved", "State", "summary", "Type", "Priority"]
  yt_req_save yt req

  putStrLn "-- 3: interpreting issue list.."
  isrs ∷ [Reader Project Issue] ← yt_req_load_response req
  let iss = fmap ((flip runReader) proj) isrs
  printf "--- got %d issues\n" $ length iss

  putStrLn "-- 4: querying workitems.."
  wilistrs ← forM iss $ \i@(Issue{ _id }) → do
               wirs ∷ [Reader Project WorkItem] ← yt_req yt $ RIssueTTWItem _id
               pure (i, wirs)
  let wilists = [ (i, fmap ((flip runReader) proj) wsrs) | (i, wsrs) ← wilistrs]

  putStrLn "-- 5: building report.."
  let report = [ issue_report args proj i wis
               | (i, wis) ← wilists ]
      report_issues    = fmap fst report
      report_workitems = concat $ fmap snd report

  putStrLn "-- 6: CSV.."
  let handle_output Nothing      bytes = putStrLn ∘ BL.toString $ bytes
      handle_output (Just fname) bytes = BL.writeFile fname bytes

  handle_output tasks_output     $ CSV.encodeDefaultOrderedByName report_issues
  handle_output workitems_output $ CSV.encodeDefaultOrderedByName report_workitems

  putStrLn "-- inf: all done."
