{-# LANGUAGE OverloadedStrings #-}

module DevelMain where

import YesodContactForm
import ClassyPrelude.Yesod
import Control.Concurrent (ThreadId, forkIO, killThread)
import Foreign.Store
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Vhost (vhost)

-- | Start or restart the server.
-- newStore is from foreign-store.
-- A Store holds onto some data across ghci reloads
update :: IO ()
update = do
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    -- no server running
    Nothing -> do
      done <- storeAction doneStore newEmptyMVar
      tid <- start done
      _ <- storeAction (Store tidStoreNum) (newIORef tid)
      return ()
    -- server is already running
    Just tidStore -> restartAppInNewThread tidStore
  where
    doneStore :: Store (MVar ())
    doneStore = Store 0

    -- shut the server down with killThread and wait for the done signal
    restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
    restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
      killThread tid
      withStore doneStore takeMVar
      readStore doneStore >>= start

    -- | Start the server in a separate thread.
    start :: MVar () -> IO ThreadId
    start done = do
      (port', _site, app) <- getApplicationRepl
      forkIO (finally (runSettings (setPort port' defaultSettings) $ vhost [] app)
          (putMVar done ()))

-- | kill the server
shutdown :: IO ()
shutdown = do
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    -- no server running
    Nothing -> putStrLn "no Yesod app running"
    Just tidStore -> do
      withStore tidStore $ readIORef >=> killThread
      putStrLn "Yesod app is shutdown"

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
  v <- readIORef ref
  f v >>= writeIORef ref
