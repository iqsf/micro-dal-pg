{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Data.DAL.KeyValue.Postgres
( PGEngine(..)
, PGEngineOpts(..)
, createEngine
, withEngine
) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.Either
import Data.Int
import Data.Proxy
import Data.Store
import Data.String (IsString(..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word
import Database.PostgreSQL.Simple as PGSimple
import Safe
import Text.InterpolatedString.Perl6 (qc)

import qualified Data.Set as S

import Data.DAL


newtype PGKey a = PGKey Text
  deriving (Eq, Ord, Show)

unPGKey :: PGKey a -> Text
unPGKey (PGKey k) = k

data PGEngine = PGEngine
  { pgEngine'conn     :: Connection
  , pgEngine'nsexists :: MVar (S.Set Text)
  }
conn = pgEngine'conn

nsUnpackNorm :: NS a -> String
nsUnpackNorm = nsNorm . nsUnpack

nsNorm :: String -> String
nsNorm = replaceSyms "/-" '_'
  where
    replaceSyms what replacement = fmap $ \c -> if c `elem` what then replacement else c

data PGEngineOpts = PGEngineOpts
                        { pgHost      :: Text
                        , pgPort      :: Word16
                        , pgDbName    :: Text
                        , pgUser      :: Text
                        , pgPassword  :: Text
                        }
    deriving (Eq, Ord, Show)

createEngine :: PGEngineOpts -> IO PGEngine
createEngine PGEngineOpts {..} = do
    pgEngine'conn <- connect $ ConnectInfo
        { connectHost     = cs pgHost
        , connectPort     = pgPort
        , connectUser     = cs pgUser
        , connectPassword = cs pgPassword
        , connectDatabase = cs pgDbName
        }
    pgEngine'nsexists <- newMVar mempty
    pure PGEngine {..}

withEngine :: PGEngineOpts -> (PGEngine -> IO a) -> IO a
withEngine opts = bracket (createEngine opts) closeEngine
  where
    closeEngine :: PGEngine -> IO ()
    closeEngine = close . conn

instance (Store a, HasKey a) => SourceListAll a IO PGEngine where
  listAll :: PGEngine -> IO [a]
  listAll e = do
    rows <- withCreateTable e table $
        query_ (conn e) [qc|select v from {table}|] :: IO [Only (Binary ByteString)]
    pure $ rights $ fmap (\(Only x) -> decode @a (fromBinary x)) rows
    where
      table = nsUnpackNorm (ns @a)

instance (Store a, HasKey a) => SourceListOffsetLimit a IO PGEngine where
  listOffsetLimit :: PGEngine -> Int -> Int -> IO [a]
  listOffsetLimit e ofs lmt = do
    rows <- withCreateTable e table $
        query_ (conn e) [qc|select v from {table} limit {ofs} offset {lmt}|] :: IO [Only (Binary ByteString)]
    pure $ rights $ fmap (\(Only x) -> decode @a (fromBinary x)) rows
    where
      table = nsUnpackNorm (ns @a)

instance (Store a, Store (KeyOf a), HasKey a) => SourceStore a IO PGEngine where
  load :: PGEngine -> KeyOf a -> IO (Maybe a)
  load e k = do
    bs <- withCreateTable e table $
        query (conn e) [qc|select v from {table} where k = ?|] (Only (Binary $ encode k)) :: IO [Only (Binary ByteString)]
    case headMay bs of
      Just (Only v) -> pure $ either (const Nothing) (Just) (decode @a (fromBinary v))
      _             -> pure Nothing
    where
      table = nsUnpackNorm (ns @a)

  store :: PGEngine -> a -> IO (KeyOf a)
  store e v = do
    withCreateTable e table $
      execute (conn e) [qc|insert into {table} (k,v) values(?,?) on conflict (k) do update set v=excluded.v|] (bkey,bval)
    pure (key v)
    where
      table = nsUnpackNorm (ns @a)
      bkey  = Binary $ encode (key v)
      bval  = Binary $ encode v

withCreateTable :: PGEngine -> String -> IO a -> IO a
withCreateTable eng table ioa = do
    ensureTableExists eng table
    catch ioa $ \case
        SqlError {sqlState = "42P01"} -> do
            createTable eng table
            ioa
        e -> throwIO e
    where
      ensureTableExists :: PGEngine -> String -> IO ()
      ensureTableExists e table = do
          tables <- readMVar (pgEngine'nsexists e)
          when (not $ (cs table) `S.member` tables) $ do
              createTable e table
              modifyMVar_ (pgEngine'nsexists e) $ pure . S.insert (cs table)

createTable :: PGEngine -> String -> IO ()
createTable e table = void $ execute_ (conn e) $ fromString
    [qc|create table if not exists {table} (k bytea primary key, v bytea)|]

instance (Store a, Store (KeyOf a), HasKey a) => SourceDeleteByKey a IO PGEngine where
  delete :: PGEngine -> KeyOf a -> IO ()
  delete e k = do
    void $ execute (conn e) [qc|delete from {table} where k = ?|] (Only (Binary $ encode k))
    where
      table = nsUnpackNorm (ns @a)

instance forall a. (Store a, Store (KeyOf a), HasKey a) => SourceDeleteAll a IO PGEngine where
  deleteAll :: Proxy a -> PGEngine -> IO ()
  deleteAll _ e = do
    void $ withCreateTable e table $
        execute_ (conn e) [qc|delete from {table}|]
    where
      table = nsUnpackNorm (ns @a)

instance forall a. (Store a, Store (KeyOf a), HasKey a) => SourceCountAll a IO PGEngine where
   countAll :: Proxy a -> PGEngine -> IO Int64
   countAll _ e = do
     let conn = pgEngine'conn e
     getCount $ query_ conn [qc|select count(*) from {table}|]
     where
       table = nsUnpackNorm (ns @a)

       getCount :: IO [Only Int64] -> IO Int64
       getCount ioa = do
         [Only countVal] <- catch ioa $ \SqlError {..} -> pure [Only 0]
         pure countVal


instance SourceTransaction a IO PGEngine where
  withTransaction e = PGSimple.withTransaction (conn e)
