-- nicovideo like twitter plugin

module NicoTwitter(
  nicoTwitter
  ) where

import qualified Codec.Binary.UTF8.String as U8
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans
import Data.IORef
import Data.List
import qualified Text.JSON as JSON

import MonadPoint.Presentation as P
import MonadPoint.Rendering as R

import Network.HTTP.Easy

nicoTwitter :: String -> Presentation (Rendering ())
nicoTwitter tag = liftIO $ do
  cc <- newIORef 0
  ss <- newIORef []
  cs <- newIORef []
  
  mv <- newEmptyMVar
  
  let url = "http://search.twitter.com/search.json"
      
  forkIO $ forever $ do
    res <- wget url [("q", U8.encodeString $ tag)] []
    case res of
      Nothing -> return ()
      Just jsons -> do
        -- putStrLn jsons
        case JSON.decode jsons of
          JSON.Error err -> putStrLn err -- return ()
          JSON.Ok res -> do
            case "results" `JSON.valFromObj` res of
              JSON.Error err -> return ()
              JSON.Ok ls -> do
                forM_ ls $ \dat -> do
                  case "text" `JSON.valFromObj` dat of
                    JSON.Error err -> putStrLn err -- return ()
                    JSON.Ok txt -> do
                      putMVar mv (txt :: String)
        return ()
    threadDelay $ 10*1000*1000
  
  return $ process cc ss cs mv
  
  where
    process cc ss cs mv = do
      cnt  <- liftIO $ readIORef cc
      sts  <- liftIO $ readIORef ss
      curs <- liftIO $ readIORef cs
      
      ns <- liftIO $ extMV mv
      ns <- return $ ns \\ sts
      
      cnt <- return $! cnt+1
      sts <- return $ ns ++ sts
      curs <- return $ curs ++ map (\i -> (0, i)) ns
      
      forM_ (zip [0..] curs) $ \(i, (pos, str)) -> preserving $ do
        transl(1-pos) (0.9-i*0.1)
        scale2 5 0.1
        P.render $ TextComponent False AlignLeft str
      
      curs <- return $ map (\(i, j) -> (i+1/120, j)) curs
      curs <- return $ filter ((<10).fst) curs

      liftIO $ writeIORef cc $ cnt
      liftIO $ writeIORef ss $ sts
      liftIO $ writeIORef cs $ curs
      
    extMV mv = do
      r <- tryTakeMVar mv
      case r of
        Nothing -> return []
        Just s -> do
          ss <- extMV mv
          return $ s:ss
