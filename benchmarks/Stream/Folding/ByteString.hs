{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables #-}
module Stream.Folding.ByteString where
import           Stream.Types
import           Stream.Folding.Prelude hiding (fromHandle)
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
import Foreign.C.Error (Errno(Errno), ePIPE)
import qualified GHC.IO.Exception as G
import Control.Exception (throwIO, try)
import Data.Word
fromLazy bs = Folding (\construct wrap done -> 
  foldrChunks (kurry construct) (done ()) bs)

stdinLn ::  Folding (Of ByteString) IO ()
stdinLn = fromHandleLn IO.stdin
{-# INLINABLE stdinLn #-}

fromHandleLn ::  IO.Handle -> Folding (Of ByteString) IO ()
fromHandleLn h = Folding $ \construct wrap done -> 
  wrap $ let go = do eof <- IO.hIsEOF h
                     if eof then return (done ()) 
                            else do bs <- B.hGetLine h 
                                    return (construct (bs :> wrap go))
         in go
{-# INLINABLE fromHandleLn #-}


stdin :: Folding (Of ByteString) IO ()
stdin = fromHandle IO.stdin

fromHandle :: IO.Handle -> Folding (Of ByteString) IO ()
fromHandle = hGetSome defaultChunkSize
{-# INLINABLE fromHandle #-}

hGetSome :: Int -> IO.Handle -> Folding (Of ByteString) IO ()
hGetSome size h = Folding $ \construct wrap done -> 
  let go = do bs <- B.hGetSome h size
              if B.null bs then return (done ())
                           else liftM (construct . (bs :>)) go
  in wrap go
{-# INLINABLE hGetSome #-}


hGet :: Int -> IO.Handle -> Folding (Of ByteString) IO ()
hGet size h =  Folding $ \construct wrap done -> 
  let go = do bs <- B.hGet h size
              if B.null bs then return (done ())
                           else liftM (construct . (bs :>)) go
  in wrap go
{-# INLINABLE hGet #-}

stdout :: MonadIO m => Folding (Of ByteString) m () -> m ()
stdout (Folding phi) = 
  phi (\(bs :> rest) -> 
         do x  <- liftIO (try (B.putStr bs))
            case x of
                Left (G.IOError { G.ioe_type  = G.ResourceVanished
                                , G.ioe_errno = Just ioe })
                     | Errno ioe == ePIPE
                         -> return ()
                Left  e  -> liftIO (throwIO e)
                Right () -> rest)
       join
       (\_ -> return ())
{-# INLINABLE stdout #-}

toHandle :: MonadIO m => IO.Handle -> Folding (Of ByteString) m () -> m ()
toHandle h (Folding phi) =
  phi (\(bs :> rest) -> liftIO (B.hPut h bs) >> rest)
      join 
      (\_ -> return ())
{-# INLINE toHandle #-}



-- span
--     :: Monad m
--     => (Word8 -> Bool)
--     -> Lens' (Producer ByteString m x)
--              (Producer ByteString m (Producer ByteString m x))
-- span_ :: Monad m 
--       => Folding_ (Of ByteString) m r 
--       -> (Word8 -> Bool) 
--       -- span_ :: Folding_ (Of ByteString) m r
--       --          -> (Word8 -> Bool)
--                -> (Of ByteString r' -> r')
--                -> (m r' -> r')
--                -> (Folding (Of ByteString) m r -> r')
--                -> r'
--       
-- span_ :: Monad m 
--      => Folding (Of ByteString) m r 
--      -> (Word8 -> Bool) -> Folding (Of ByteString) m (Folding (Of ByteString) m r)


-- ------------------------
-- span_  (Folding phi) p = Folding $ \construct wrap done -> 
--    getFolding (phi 
--         (\(bs :> rest) -> undefined)
--         (\mf -> undefined)
--         (\r c w d -> getFolding r c w d))
--         construct wrap done
-- ------------------------                                       

     -- (\(bs :> Folding rest) -> Folding $ \c w d -> 
     --               let (prefix, suffix) = B.span p bs
     --               in if B.null suffix
     --                    then  getFolding  (rest c w d )
     --                    else  c (prefix :>  d rest)     
         -- (\mpsi -> Folding $ \c w d -> 
         --             w $ mpsi >>= \(Folding psi) -> return (psi c w d))
         -- (\r -> Folding $ \c w d ->  getFolding (d r))
         -- 


    
    
-- Folding $ \c w d -> wrap $ mpsi >>= \(Folding psi) -> return (psi c w d)
  -- where
  --   go p = do
  --       x <- lift (next p)
  --       case x of
  --           Left   r       -> return (return r)
  --           Right (bs, p') -> do
  --               let (prefix, suffix) = BS.span predicate bs
  --               if (BS.null suffix)
  --                   then do
  --                       yield bs
  --                       go p'
  --                   else do
  --                       yield prefix
  --                       return (yield suffix >> p')
-- # INLINABLE span #-}
-- break predicate = span (not . predicate)
-- 
-- nl :: Word8
-- nl = fromIntegral (ord '\n')
-- 
-- _lines
--     :: Monad m => Producer ByteString m x -> FreeT (Producer ByteString m) m x
-- _lines p0 = PG.FreeT (go0 p0)
--   where
--     go0 p = do
--         x <- next p
--         case x of
--             Left   r       -> return (PG.Pure r)
--             Right (bs, p') ->
--                 if (BS.null bs)
--                 then go0 p'
--                 else return $ PG.Free $ go1 (yield bs >> p')
--     go1 p = do
--         p' <- p^.line
--         return $ PG.FreeT $ do
--             x  <- nextByte p'
--             case x of
--                 Left   r       -> return (PG.Pure r)
--                 Right (_, p'') -> go0 p''
-- {-# INLINABLE _lines #-}
-- 
-- _unlines
--     :: Monad m => FreeT (Producer ByteString m) m x -> Producer ByteString m x
-- _unlines = concats . PG.maps addNewline
--   where
--     addNewline p = p <* yield (BS.singleton nl)
-- {-# INLINABLE _unlines #
-- 
-- 
splitAt :: (Monad m) 
         => Int 
         -> Folding (Of ByteString) m r 
         -> Folding (Of ByteString) m (Folding (Of ByteString) m r)
splitAt n0 (Folding phi) = 
  phi 
  (\(bs :> nfold) n -> 
    let len = fromIntegral (B.length bs)
        rest = joinFold (nfold (n-len))
    in if n > 0
       then if n > len 
            then Folding $ \construct wrap done -> construct $
                       bs :> getFolding (nfold (n-len)) construct wrap done
            else let (prefix, suffix) =  B.splitAt (fromIntegral n) bs
                 in Folding $ \construct wrap done -> construct $
                       if B.null suffix 
                       then prefix :> done rest
                       else prefix :> done (cons suffix rest)
       else Folding $ \construct wrap done ->  done $
                        bs `cons` rest
  )
  (\m n -> Folding $ \construct wrap done -> wrap $
                       liftM (\f -> getFolding (f n) construct wrap done) m
  )
  (\r n ->  Folding $ \construct wrap done -> done $
                        Folding $ \c w d -> d r
  )
  n0

