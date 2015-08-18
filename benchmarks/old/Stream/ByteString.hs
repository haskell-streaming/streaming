{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables #-}
module Stream.ByteString where
import           Stream.Types
import qualified Stream.Folding.Prelude as FP
import qualified Stream.Folding.ByteString as FB
import           Control.Monad hiding (filterM, mapM)
import           Data.Functor.Identity
import           Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Internal (foldrChunks, defaultChunkSize)
import           Data.ByteString (ByteString) 
import qualified System.IO as IO
import           Prelude hiding (map, filter, drop, take, sum
                        , iterate, repeat, replicate, splitAt
                        , takeWhile, enumFrom, enumFromTo)

fromLazy  = buildStream . FB.fromLazy

stdinLn ::  Stream (Of ByteString) IO ()
stdinLn = fromHandleLn IO.stdin
{-# INLINE stdinLn #-}

fromHandleLn ::  IO.Handle -> Stream (Of ByteString) IO ()
fromHandleLn = buildStream . FB.fromHandleLn
{-# INLINABLE fromHandleLn #-}

stdin :: Stream (Of ByteString) IO ()
stdin = fromHandle IO.stdin
{-# INLINE stdin #-}

fromHandle :: IO.Handle -> Stream (Of ByteString) IO ()
fromHandle = hGetSome defaultChunkSize
{-# INLINABLE fromHandle #-}

hGetSome :: Int -> IO.Handle -> Stream (Of ByteString) IO ()
hGetSome size = buildStream . FB.hGetSome size
{-# INLINABLE hGetSome #-}

hGet :: Int -> IO.Handle -> Stream (Of ByteString) IO ()
hGet size = buildStream . FB.hGet size
{-# INLINABLE hGet #-}

splitAt :: (Monad m) 
         => Int 
         -> Stream (Of ByteString) m r 
         -> Stream (Of ByteString) m (Stream (Of ByteString) m r)
splitAt n =
  buildStream
  . fmap buildStream
  . FB.splitAt n
  . foldStream
