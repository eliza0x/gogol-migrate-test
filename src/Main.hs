{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Lens           ((&), (.~), (<&>), (?~), (^.))
import qualified Control.Lens as L
import qualified Network.Google as G
import qualified Network.Google.Storage as Storage
import qualified Network.Google.Storage.Types as ST
import System.IO              (stdout)
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Conduit as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Resource (ResourceT, liftResourceT)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
 
key = "goat.webp"
bkt = "nobanashi-images"

-- uploadExample :: ResourceT IO (ConduitM () ByteString (ResourceT IO) ())
-- uploadExample :: IO [ByteString]
uploadExample :: IO ST.Objects
uploadExample = do
    lgr  <- G.newLogger G.Debug stdout                                               -- (1)
    env  <- G.newEnv <&> (G.envLogger .~ lgr) . (G.envScopes .~ Storage.storageReadWriteScope) -- (2) (3)
    body <- G.sourceBody "goat.webp"
    C.runResourceT . G.runGoogle env $ do
        -- G.upload (Storage.objectsInsert bkt Storage.object' & Storage.oiName ?~ key) body

        -- stream <- G.download $ Storage.objectsGet bkt key
	-- l <- liftResourceT $ runConduit (stream .| CC.sinkList)
	-- return l

        stream <- G.send $ Storage.objectsList bkt 
	return stream

main :: IO ()
main = do
  output <- uploadExample 
  putStrLn "----"
  mapM_ (print . L.view ST.objName) $ output ^. ST.oItems 

