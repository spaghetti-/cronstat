{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Snap
import Snap.Snaplet.PostgresqlSimple
import Control.Lens
import Data.ByteString
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Time.LocalTime

data App = App
    { _db :: Snaplet Postgres }

data Job = Job
    { _id :: T.Text
    , name :: T.Text
    , description :: T.Text
    , schedule :: Int
    , lastRun :: Maybe ZonedTime
    }

instance FromRow Job where
    fromRow = Job <$> field <*> field <*> field <*> field <*> field

instance Show Job where
    show (Job _id name description schedule lastRun) =
        "Job { id: " ++ T.unpack _id ++ ", title: " ++ T.unpack name ++
        ", description: " ++ T.unpack description ++
        ", schedule: " ++ show schedule ++ ", lastRun: " ++ show lastRun ++ "}"

makeLenses ''App

instance HasPostgres (Handler b App) where
    getPostgresState = with db get
    setLocalPostgresState s = local (set (db . snapletValue) s)

routes :: [(ByteString, Handler App App ())]
routes = [ ("/", writeText "crontracker")
         , ("/list", listHandler)
         , ("/ping", pingHandler)
         ]

listHandler :: Handler App App ()
listHandler = do
    results <- query_ "select * from cronstatus"
    liftIO $ print (results :: [Job])

pingHandler :: Handler App App ()
pingHandler = do
    zt <- liftIO getZonedTime
    name <- getPostParam "name"
    cur <- execute "update cronstatus set \"lastRun\" = ? where name = ?" (zt, name)
    writeText $ T.pack . show $ cur

app :: SnapletInit App App
app = makeSnaplet "app" "Tracks status of repetitive jobs." Nothing $ do
    d <- nestSnaplet "db" db pgsInit
    addRoutes routes
    return $ App d

main :: IO ()
main = serveSnaplet defaultConfig app
