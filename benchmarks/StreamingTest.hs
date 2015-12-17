module Main where

import qualified Streaming.Prelude as Str
import qualified System.IO.Streams as IOS
import           Conduit.Simple as S
import           Control.Exception
import           Criterion.Main
import           Data.Conduit as C
import           Data.Conduit.Combinators as C
--
-- import           Fusion as F hiding ((&))
-- import           Data.Function ((&))

import           Pipes as P
import qualified Pipes.Prelude as P
import           Test.Hspec
import           Test.Hspec.Expectations

main :: IO ()
main = do
    -- hspec $ do
    --     describe "basic tests" $
    --         it "passes tests" $ True `shouldBe` True

    defaultMain [
        bgroup "basic" [ bench "stream"         $ nfIO stream_basic
                       , bench "iostreams"      $ nfIO iostreams_basic
                       , bench "pipes"          $ nfIO pipes_basic
                       , bench "conduit"        $ nfIO conduit_basic
                    --   , bench "simple-conduit" $ nfIO simple_conduit_basic
                    --   , bench "fusion"         $ nfIO fusion_basic
                       
                       ]
        ]

pipes_basic :: IO Int
pipes_basic = do
    xs <- P.toListM $ P.each [1..1000000]
      >-> P.filter even
      >-> P.map (+1)
      >-> P.drop 1000
      >-> P.map (+1)
      >-> P.filter (\x -> x `mod` 2 == 0)
    assert (Prelude.length xs == 499000) $
        return (Prelude.length xs)

conduit_basic :: IO Int
conduit_basic = do
    xs <-   C.yieldMany [1..1000000]
      C.$= C.filter even
      C.$= C.map ((+1) :: Int -> Int)
      C.$= (C.drop 1000 >> C.awaitForever C.yield)
      C.$= C.map ((+1) :: Int -> Int)
      C.$= C.filter (\x -> x `mod` 2 == 0)
      C.$$ C.sinkList
    assert (Prelude.length xs == 499000) $
        return (Prelude.length (xs :: [Int]))

simple_conduit_basic :: IO Int
simple_conduit_basic = do
    xs <-   S.sourceList [1..1000000]
      S.$= S.filterC even
      S.$= S.mapC ((+1) :: Int -> Int)
      S.$= S.dropC 1000
      S.$= S.mapC ((+1) :: Int -> Int)
      S.$= S.filterC (\x -> x `mod` 2 == 0)
      S.$$ S.sinkList
    assert (Prelude.length xs == 499000) $
        return (Prelude.length (xs :: [Int]))



fusion_basic :: IO Int
fusion_basic = do
    xs <- F.toListM $ F.each [1..1000000]
      & F.filter even
      & F.map (+1)
      & F.drop 1000
      & F.map (+1)
      & F.filter (\x -> x `mod` 2 == 0)
    assert (Prelude.length xs == 499000) $
        return (Prelude.length (xs :: [Int]))

stream_basic :: IO Int
stream_basic = do
    xs <- Str.toListM $ Str.each [1..1000000]
      & Str.filter even
      & Str.map (+1)
      & Str.drop 1000
      & Str.map (+1)
      & Str.filter (\x -> x `mod` 2 == 0)
    assert (Prelude.length xs == 499000) $
        return (Prelude.length (xs :: [Int]))


iostreams_basic :: IO Int
iostreams_basic = do
  s0 <- IOS.fromList [1..1000000]
  s1 <- IOS.filter even s0
  s2 <- IOS.map (+1) s1
  s3 <- IOS.drop 1000 s2
  s4 <- IOS.map (+1) s3
  s5 <- IOS.filter (\x -> x `mod` 2 == 0) s4
  xs <- IOS.toList s5
  assert (Prelude.length xs == 499000) $
      return (Prelude.length (xs :: [Int]))

        