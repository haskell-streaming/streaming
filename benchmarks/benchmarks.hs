{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Monad (void, forever)
import Criterion.Main
import Stream.Types
import Stream.Combinators
import Stream.Prelude 
import qualified Stream.FreeT.Prelude as F
import qualified Stream.Prelude.Direct as N 
import qualified Stream.Producer.Prelude as Pr 
import qualified Stream.List.Prelude as L
import qualified Control.Monad.Trans.Free as F
-- import qualified Remorse.FreeT.Prelude as E
-- import qualified Remorse.FreeT as E

import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate, splitAt
                      , takeWhile, enumFrom, enumFromTo
                      , mapM, scanr, span)
import qualified Prelude as P
import Data.Functor.Identity
import Pipes hiding (yield)
import qualified Pipes 

import qualified Pipes.Prelude as PP
import qualified Data.Vector.Unboxed as V
value :: Int
value = 100
big :: Int
big = 10000000

-- -------------------
-- long composition
-- -------------------

long_fused :: Int -> Int
long_fused  n = runIdentity $ sum ( 
             (take n
              (drop 100
                (map (\x -> 3*x + 1)
                (filter even
               ((iterate (\x -> x+1) (10 :: Int) ) :: Stream (Of Int) Identity ())
              )))))  
{-# INLINE long_fused  #-}

long_fused_free :: Int -> Int
long_fused_free  n = runIdentity $ F.sum ( 
             (F.take n
              (F.drop 100
                (F.map (\x -> 3*x + 1)
                (F.filter even
               ((F.iterate (\x -> x+1) (10 :: Int) ) :: F.FreeT (Of Int) Identity ())
              )))))  
{-# INLINE long_fused_free  #-}

long_fused_pipes :: Int -> Int
long_fused_pipes  n = runIdentity $ Pr.sum ( 
             (Pr.take n
              (Pr.drop 100
                (Pr.map (\x -> 3*x + 1)
                (Pr.filter even
               ((Pr.iterate (\x -> x+1) (10 :: Int) ) :: Producer Int Identity ())
              )))))
{-# INLINE long_fused_pipes  #-}
--
long_fused_list :: Int -> Int
long_fused_list  n =  L.sum ( 
             (L.take n
              (L.drop 100
                (L.map (\x -> 3*x + 1)
                (L.filter even
               ((L.iterate (\x -> x+1) (10 :: Int) ) :: [Int])
              )))))
              
{-# INLINE long_fused_list  #-}
long_naive  :: Int -> Int
long_naive  n = runIdentity $ N.sum (
    (N.take n
     (N.drop 100
       (N.map (\x -> 3*x + 1)
       (N.filter even
      ((N.iterate (\x -> x+1) (10 :: Int) ) )
     ))))) 
{-# INLINE long_naive  #-}

long_list  :: Int -> Int
long_list  n = P.sum (
    (P.take n
     (P.drop 100
       (P.map (\x -> 3*x + 1)
       (P.filter even
      ((P.iterate (\x -> x+1) (10 :: Int) ) )
     ))))) 
{-# INLINE long_list  #-}

long_vector :: Int -> Int
long_vector n =  V.sum (
    (V.take n
     (V.drop 100
       (V.map (\x -> 3*x + 1)
       (V.filter even
      ((V.iterateN (n*2+300) (\x -> x+1) (10 :: Int) ) )
     ))))) 
{-# INLINE long_vector #-}

pipe_naive :: Int -> Int
pipe_naive n = runIdentity $ 
         PP.sum $ each (P.iterate (\x -> x+1) (10 :: Int) ) 
                  >-> PP.filter even
                  >-> PP.map (\x -> 3*x + 1)
                  >-> PP.drop 100
                  >-> PP.take n
{-# INLINE pipe_naive #-}
-- long_fused_remorse :: Int -> Int
-- long_fused_remorse  n = runIdentity $ E.sum_ (
--              (E.take n
--               (E.drop 100
--                 (E.map (\x -> 3*x + 1)
--                 (E.filter even
--                ((E.iterate (\x -> x+1) (10 :: Int) ) :: E.FreeT (E.Of Int) Identity ())
--               )))))
-- {-# INLINE long_fused_remorse  #-}
--
-- -- -------------------
-- longish compositions
-- -------------------
longish_naive  :: Int -> Int
longish_naive  n = runIdentity $ N.sum (
    (N.take n
     (N.drop 100
       (N.map (\x -> 3*x + 1)
      ((N.iterate (\x -> x+1) (10 :: Int) ) )
     ))))
{-# INLINE longish_naive  #-}

longish_list  :: Int -> Int
longish_list  n = P.sum (
    (P.take n
     (P.drop 100
       (P.map (\x -> 3*x + 1)
      ((P.iterate (\x -> x+1) (10 :: Int) ) )
     ))))
{-# INLINE longish_list  #-}

longish_vector :: Int -> Int
longish_vector n =  V.sum (
    (V.take n
     (V.drop 100
       (V.map (\x -> 3*x + 1)
      ((V.iterateN (n*2+300) (\x -> x+1) (10 :: Int) ) )
     ))))
{-# INLINE longish_vector #-}

longish_pipe :: Int -> Int
longish_pipe n = runIdentity $ 
         PP.sum $ each (P.iterate (\x -> x+1) (10 :: Int) ) 
                  >-> PP.map (\x -> 3*x + 1)
                  >-> PP.drop 100
                  >-> PP.take n
{-# INLINE longish_pipe #-}

-- longish_remorse :: Int -> Int
-- longish_remorse  n = runIdentity $ E.sum_ (
--              (E.take n
--               (E.drop 100
--                 (E.map (\x -> 3*x + 1)
--                ((E.iterate (\x -> x+1) (10 :: Int) ) :: E.FreeT (E.Of Int) Identity ())
--               ))))
-- {-# INLINE longish_remorse  #-}
-- --

-- -------------------
-- shortish compositions
-- -------------------
shortish_naive  :: Int -> Int
shortish_naive  n = runIdentity $ N.sum (
    (N.take n
       (N.map (\x -> 3*x + 1)
      ((N.iterate (\x -> x+1) (10 :: Int) ) )
     )))
{-# INLINE shortish_naive  #-}

shortish_list  :: Int -> Int
shortish_list  n = P.sum (
    (P.take n
       (P.map (\x -> 3*x + 1)
      ((P.iterate (\x -> x+1) (10 :: Int) ) )
     )))
{-# INLINE shortish_list  #-}

shortish_vector :: Int -> Int
shortish_vector n =  V.sum (
    (V.take n
       (V.map (\x -> 3*x + 1)
      ((V.iterateN (n*2+300) (\x -> x+1) (10 :: Int) ) )
     )))
{-# INLINE shortish_vector #-}

shortish_pipe :: Int -> Int
shortish_pipe n = runIdentity $ 
         PP.sum $ each (P.iterate (\x -> x+1) (10 :: Int) ) 
                  >-> PP.map (\x -> 3*x + 1)
                  >-> PP.take n
{-# INLINE shortish_pipe #-}

-- shortish_remorse :: Int -> Int
-- shortish_remorse  n = runIdentity $ E.sum_ (
--              (E.take n
--                 (E.map (\x -> 3*x + 1)
--                ((E.iterate (\x -> x+1) (10 :: Int) ) :: E.FreeT (E.Of Int) Identity ())
--               )))
-- {-# INLINE shortish_remorse  #-}
-- -------------------
-- shorter composition
-- -------------------
short_naive :: Int -> Int
short_naive = \n -> runIdentity $ N.sum (N.take n (N.iterate (\x -> x+1) (10 :: Int) :: Stream (Of Int) Identity ()))
{-# INLINE short_naive #-}
short_free :: Int -> Int
short_free = \n -> runIdentity $ F.sum (F.take n (F.iterate (\x -> x+1) (10 :: Int) :: F.FreeT (Of Int) Identity ()))
{-# INLINE short_free #-}
short_fused :: Int -> Int
short_fused = \n -> runIdentity $ sum (take n (iterate (\x -> x+1) (10 :: Int) :: Stream (Of Int) Identity ()))
{-# INLINE short_fused #-}
short_producer :: Int -> Int
short_producer = \n -> runIdentity $ Pr.sum (Pr.take n (Pr.iterate (\x -> x+1) (10 :: Int) :: Producer Int Identity ()))
{-# INLINE short_producer #-}
short_fused_list  :: Int -> Int
short_fused_list  = \n ->  L.sum (L.take n 
   ( L.iterate (\x -> x+1) (10 :: Int)  ))

{-# INLINE short_fused_list #-}
short_list :: Int -> Int
short_list = \n -> P.sum (P.take n (P.iterate (\x -> x+1) (10 :: Int)))
{-# INLINE short_list #-}

short_vector :: Int -> Int
short_vector = \n -> V.sum (V.take n (V.iterateN (n*2) (\x -> x+1) (10 :: Int)))
{-# INLINE short_vector #-}

short_producer_naive :: Int -> Int 
short_producer_naive = \n -> runIdentity $ 
           PP.sum (each (P.iterate (\x -> x+1) (10 :: Int) ) 
                   >-> PP.take n
                   )
{-# INLINE short_producer_naive #-}                

short_remorse :: Int -> Int
short_remorse = \n -> runIdentity $ 
  E.sum_ (E.take n (E.iterate (\x -> x+1) (10 :: Int) :: E.FreeT (E.Of Int) Identity ()))
{-# INLINE short_remorse #-}



-- -------------------
-- simple sum
-- -------------------

rN :: Int -> Int 
rN n = runIdentity (N.sum (N.replicate n 1))
{-# INLINE rN #-}    


rF :: Int -> Int 
rF n = runIdentity (sum (replicate n 1))
{-# INLINE rF #-}

rFr :: Int -> Int 
rFr n = runIdentity (F.sum (F.replicate n 1))
{-# INLINE rFr #-}

rPr :: Int -> Int 
rPr n = runIdentity (Pr.sum (Pr.replicate n 1))
{-# INLINE rPr #-}

rPrN :: Int -> Int 
rPrN n = runIdentity (PP.sum (each (P.replicate n 1)))
{-# INLINE rPrN #-}
rL :: Int -> Int
rL n =  P.sum (P.replicate n 1)
{-# INLINE rL #-}
rl :: Int -> Int
rl n =  L.sum (L.replicate n 1)
{-# INLINE rl #-}
rV :: Int -> Int
rV n = V.sum (V.replicate n 1)
{-# INLINE rV #-}

rFu :: Int -> Int 
rFu n = runIdentity (E.sum_ (E.replicate n 1))
{-# INLINE rFu #-}
-- -----
-- enum
-- -----

enumFromStep_ :: Int -> Int -> Int -> Producer Int Identity ()
enumFromStep_ n1 s n2 = loop n1 n2
    where
        loop !step 0 = return ()
        loop step n = do
                Pipes.yield $! step
                loop  (step + s) $! (n - 1)
{-# INLINABLE enumFromStep_ #-}

z :: Int
z = 0

enum_naive n = runIdentity (N.sum (N.map (+7) (N.enumFromStepN z 10 (n*3))))
{-# INLINE enum_naive #-}
enum_free n = runIdentity (F.sum (F.map (+7) (F.enumFromStepN z 10 (n*3))))
{-# INLINE enum_free #-}
enum_pipe n = runIdentity (Pr.sum (Pr.map (+7) (Pr.enumFromStepN z 10 (n*3))))
{-# INLINE enum_pipe #-}
enum_pipe_naive n = runIdentity (PP.sum (enumFromStep_ z 10 (n*30) >-> PP.map (+7)))
{-# INLINE enum_pipe_naive #-}
enum_fused n = runIdentity (sum (map (+7) (enumFromStepN z 10 (n*3))))
{-# INLINE enum_fused #-}
enum_vector n = V.sum (V.map (+7) (V.enumFromStepN z 10 (n*3)))
{-# INLINE enum_vector #-}
enum_list n = P.sum (P.map (+7) (P.take (n*3) [z, 10 ..]))
{-# INLINE enum_list #-}
enum_list_fused n = L.sum (L.map (+7) (L.take (n*3) (L.enumFromStepN z 10 (n*3))))
{-# INLINE enum_list_fused #-}
--
enum_naive_dot  = runIdentity . N.sum . N.map (+7) . N.enumFromStepN z 10 . (*3)
{-# INLINE enum_naive_dot #-}
enum_free_dot  = runIdentity . F.sum . F.map (+7) . F.enumFromStepN z 10 . (*3)
{-# INLINE enum_free_dot #-}
enum_pipe_dot  = runIdentity . Pr.sum . Pr.map (+7) . Pr.enumFromStepN z 10 . (*3)
{-# INLINE enum_pipe_dot #-}
enum_fused_dot = runIdentity . sum . map (+7) . enumFromStepN z 10 . (*3)
{-# INLINE enum_fused_dot #-}
enum_vector_dot = V.sum . V.map (+7) . V.enumFromStepN z 10 . (*3)
{-# INLINE enum_vector_dot #-}
enum_list_dot = P.sum . P.map (+7) . (\n -> [z, 10 .. n*30])
{-# INLINE enum_list_dot #-}
enum_list_fused_dot = L.sum . L.map (+7) . L.enumFromStepN z 10 . (*3)
{-# INLINE enum_list_fused_dot #-}
goo :: Int -> Int
goo = runIdentity . sum . take 1000 . forever . yield 
{-# INLINE goo #-}
gooo :: Int -> Int
gooo = runIdentity . N.sum . N.take 1000 . forever . (\a -> Construct (a :> Done ()))
{-# INLINE gooo #-}
goooo :: Int -> Int
goooo = runIdentity . sum . replicate 1000
{-# INLINE goooo #-}

main :: IO ()
main =
  defaultMain
    [
   --  bgroup "sum.map.enumFromTo"
   --      [ bench "vector" $ whnf enum_vector value
   --      , bench "series"  $ whnf enum_naive value
   -- --     , bench "pipes"  $ whnf enum_pipe_naive value
   --      , bench "list"   $ whnf enum_list value
   --      , bench "FOLDING/series"  $ whnf enum_fused value      
   --      , bench "FOLDING/freet"  $ whnf enum_free value
   --      , bench "FOLDING/pipes" $ whnf enum_pipe value
   --      , bench "FOLDING/list" $ whnf enum_list_fused value
   --      ]
   --  , bgroup "sum.map.enumFromTo.pointfree"
   --      [ bench "vector" $ whnf enum_vector_dot value
   --      , bench "series"  $ whnf enum_naive_dot value
   --      , bench "list"   $ whnf enum_list_dot value
   --      , bench "FOLDING/series"  $ whnf enum_fused_dot value
   --      , bench "FOLDING/freet"  $ whnf enum_fused_dot value
   --      , bench "FOLDING/pipes"  $ whnf enum_pipe_dot value
   --      , bench "FOLDING/list"  $ whnf enum_list_fused_dot value
   --       ]
  -- , 
   bgroup "sum.replicate"
       [ bench "vector" $ whnf rV value
       , bench "series"  $ whnf rN value
       , bench "pipes" $ whnf rPrN value
       , bench "list"   $ whnf rL value
--    , bench "remorse"   $ whnf rFu value

       , bench "FOLDING/series"  $ whnf rF value
       , bench "FOLDING/freet"  $ whnf rFr value
       , bench "FOLDING/list"  $ whnf rl value

       -- , bench "FOLDING/pipes" $ whnf rPr value
       ]
   , bgroup "sum.take.iterate"
       [ bench "vector" $ whnf short_vector value
       , bench "series"  $ whnf short_naive value
--       , bench "pipes"  $ whnf short_producer_naive value
       , bench "list"   $ whnf short_list value
--       , bench "remorse"   $ whnf short_remorse value

       , bench "FOLDING/series"  $ whnf short_fused value
       , bench "FOLDING/freet"  $ whnf short_free value
       , bench "FOLDING/list"  $ whnf short_fused_list value

       -- , bench "FOLDING/pipes"  $ whnf short_producer value
        ]
      ,  bgroup "sum.take.map.iterate"
          [bench "vector" $ whnf shortish_vector value
          , bench "series"  $ whnf shortish_naive value  
--          , bench "remorse" $ whnf shortish_remorse value
          , bench "list"   $ whnf shortish_list value
          , bench "FOLDING/series"  $ whnf short_fused value
          , bench "FOLDING/list"  $ whnf short_fused_list value
--          , bench "FOLDING/freet"  $ whnf short_fused_free value
          -- , bench "FOLDING/pipes" $ whnf short_fused_pipes value
          ]
    ,  bgroup "sum.take.map.drop.iterate"
        [bench "vector" $ whnf longish_vector value
        , bench "series"  $ whnf longish_naive value  
--        , bench "remorse" $ whnf longish_remorse value
        , bench "list"   $ whnf longish_list value
        , bench "FOLDING/series"  $ whnf long_fused value
        , bench "FOLDING/list"  $ whnf long_fused_list value
        , bench "FOLDING/freet"  $ whnf long_fused_free value
        -- , bench "FOLDING/pipes" $ whnf long_fused_pipes value
        ]
    ,  bgroup "sum.take.map.drop.filter.iterate"
        [bench "vector" $ whnf long_vector value
        , bench "series"  $ whnf long_naive value  
--        , bench "remorse" $ whnf long_fused_remorse value
        , bench "list"   $ whnf long_list value
        , bench "FOLDING/series"  $ whnf long_fused value
--        , bench "FOLDING/list"  $ whnf long_fused_list value
        , bench "FOLDING/freet"  $ whnf long_fused_free value
        -- , bench "FOLDING/pipes" $ whnf long_fused_pipes value
        ]
     ]