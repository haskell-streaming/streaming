import Series.Types
import Series.Combinators
-- import qualified Series.ByteString as SB

import Series.List.Prelude 
import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate
                      , splitAt, mapM, takeWhile, scanr)
import qualified Prelude as P
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Morph
import System.Environment
import System.IO 
import qualified Data.ByteString as B
import Pipes

-- 
main =  print $ number (10 :: Int)
-- number = sum2 
--                . P.take 10000000
--                . drop 100
--                . map2 (\x -> 3*x + 1)
--                . P.filter even
--                . (iterate (\x -> x+1))
               
-- {-# INLINE number #-}
number n = P.sum 
               (P.take 10000000
                (P.drop 100
                  (P.map (\x -> 3*x + 1)
                  (P.filter even
                 (P.iterate (\x -> x+1) (n )  :: [Int])
                ))))
  
  
-- main = print $ P.sum 
--                . P.take 10000000
--                . P.drop 100
--                . P.map (\x -> 3*x + 1)
--                . P.filter even
--                $ (P.iterate (\x -> x+1) (10 :: Int)  :: [Int])
--          
-- {-# INLINE long_fused  #-}
-- main = print =<< sum2 ( map B.length SB.stdinLn) 
-- main = print $ runIdentity $ foldl' (+) 0 (replicate 30 1)
-- main = getFolding (foldMonadPlus $ (replicate 10 1 :: ListT IO Int) )
                          -- (\(str :> x) -> print str >> x) 
                          -- join 
                          -- return
                          --        
-- main = getFolding (foldSeries stdinLn)
--                    (\(str :> x) -> putStrLn str >> x) 
--                    join 
--                    return
                   
                   
{-
gain = do (a:sn:_) <- getArgs -- pain where
          let n = 1000 * read sn :: Int
          case a of  "f" -> f n
                     "g" -> g n
                     "x" -> x n
                     "l" -> l n
                     _      -> putStrLn "f g x l [Int]" 
  where
    f :: Int -> IO ()
    f n = sumF ( 
                 (takeWhileF (< n)
                  (dropF 100
                    (mapF (\x ->  3*x + 1)
                    (filterF even
                   (iterateF (\x -> x+1) (10 :: Int) )
                 )))) :: Series (Of Int) IO ())  >>= print

    g :: Int -> IO ()
    g n = print $ sumG ( 
                 (takeWhileG (< n)
                  (dropG 100
                    (mapG (\x ->  3*x + 1)
                    (filterG even
                   (iterateG (\x -> x+1) (10 :: Int) )
                 )))) :: Series (Of Int) Identity ()) 

    x :: Int -> IO ()
    x n = sum ( 
                 (takeWhile (< n)
                  (drop 100
                    (map (\x -> 3*x + 1)
                    (filter even
                   ((iterate (\x -> x+1) (10 :: Int) ) :: Series (Of Int) IO ())
                  )))))  >>= print

    l n = print $ P.sum (
      (P.takeWhile (< n)
       (P.drop 100
         (P.map (\x -> 3*x + 1)
         (P.filter even
        ((P.iterate (\x -> x+1) (10 :: Int) ) )
       )))))  + 1


b = mapF show $ takeWhileF (< 14) $ dropF 1 
              $ filterF even $ iterateF (\x -> x+1) (0 :: Int) 
a = mapG show $ takeWhileG (< 14) $ dropG 1 
              $ filterG even $ iterateG (\x -> x+1) (0 :: Int) 
              
-}