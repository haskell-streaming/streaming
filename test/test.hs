module Main where

import qualified Data.Foldable as Foldable
import Data.Functor.Identity
import Data.Ord
import qualified Streaming.Prelude as S
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main =
  hspec $ do
    describe "slidingWindowMin" $ do
      it "works with a few simple cases" $ do
        runIdentity (S.toList_ (S.slidingWindowMin 2 (S.each [1, 3, 9, 4, 6, 4]))) `shouldBe` [1, 3, 4, 4, 4]
        runIdentity (S.toList_ (S.slidingWindowMin 3 (S.each [1, 3, 2, 6, 3, 7, 8, 9]))) `shouldBe` [1, 2, 2, 3, 3, 7]
      it "produces no results with empty streams" $
        property $ \k ->
          runIdentity (S.toList_ (S.slidingWindowMin k (mempty :: S.Stream (S.Of Int) Identity ()))) `shouldBe` []
      it "behaves like a (S.map Foldable.minimum) (slidingWindow) for non-empty streams" $
        property $ \(NonEmpty xs) k -- we use NonEmpty because Foldable.minimum crashes on empty lists
         ->
          runIdentity (S.toList_ (S.slidingWindowMin k (S.each xs))) ===
          runIdentity (S.toList_ (S.map Foldable.minimum (S.slidingWindow k (S.each (xs :: [Int])))))
      it "behaves like identity when window size is 1" $
        property $ \xs -> runIdentity (S.toList_ (S.slidingWindowMin 1 (S.each (xs :: [Int])))) === xs
      it "produces a prefix when the stream elements are sorted" $
        property $ \(Sorted xs) k ->
          (length xs >= k) ==>
          (runIdentity (S.toList_ (S.slidingWindowMin k (S.each (xs :: [Int])))) === take (length xs - (k - 1)) xs)
    describe "slidingWindowMinBy" $ do
      it "prefers earlier elements when several elements compare equal" $ do
        runIdentity (S.toList_ (S.slidingWindowMinBy (comparing fst) 2 (S.each [(1, 1), (2, 2), (2, 3), (2, 4)]))) `shouldBe`
          [(1, 1), (2, 2), (2, 3)]
    describe "slidingWindowMax" $ do
      it "produces no results with empty streams" $
        property $ \k ->
          runIdentity (S.toList_ (S.slidingWindowMax k (mempty :: S.Stream (S.Of Int) Identity ()))) `shouldBe` []
      it "behaves like a (S.map Foldable.maximum) (slidingWindow n s) for non-empty streams" $
        property $ \(NonEmpty xs) k -- we use NonEmpty because Foldable.maximum crashes on empty lists
         ->
          runIdentity (S.toList_ (S.slidingWindowMax k (S.each xs))) ===
          runIdentity (S.toList_ (S.map Foldable.maximum (S.slidingWindow k (S.each (xs :: [Int])))))
      it "behaves like identity when window size is 1" $
        property $ \xs -> runIdentity (S.toList_ (S.slidingWindowMax 1 (S.each (xs :: [Int])))) === xs
      it "produces a suffix when the stream elements are sorted" $
        property $ \(Sorted xs) k ->
          (length xs >= k) ==>
          (runIdentity (S.toList_ (S.slidingWindowMax k (S.each (xs :: [Int])))) === drop (k - 1) xs)
    describe "slidingWindowMaxBy" $ do
      it "prefers later elements when several elements compare equal" $ do
        runIdentity (S.toList_ (S.slidingWindowMaxBy (comparing fst) 2 (S.each [(1, 1), (2, 2), (2, 3), (2, -900)]))) `shouldBe`
          [(2, 2), (2, 3), (2, -900)]
