module Main where

import qualified Data.Foldable as Foldable
import Data.Functor.Identity
import Data.Ord
import qualified Streaming.Prelude as S
import Test.Hspec
import Test.QuickCheck

toL :: S.Stream (S.Of a) Identity b -> [a]
toL = runIdentity . S.toList_

main :: IO ()
main =
  hspec $ do
    describe "slidingWindowMin" $ do
      it "works with a few simple cases" $ do
        toL (S.slidingWindowMin 2 (S.each [1, 3, 9, 4, 6, 4])) `shouldBe` [1, 3, 4, 4, 4]
        toL (S.slidingWindowMin 3 (S.each [1, 3, 2, 6, 3, 7, 8, 9])) `shouldBe` [1, 2, 2, 3, 3, 7]
      it "produces no results with empty streams" $
        property $ \k -> toL (S.slidingWindowMin k (mempty :: S.Stream (S.Of Int) Identity ())) `shouldBe` []
      it "behaves like a (S.map Foldable.minimum) (slidingWindow) for non-empty streams" $
        property $ \(NonEmpty xs) k -- we use NonEmpty because Foldable.minimum crashes on empty lists
         ->
          toL (S.slidingWindowMin k (S.each xs)) ===
          toL (S.map Foldable.minimum (S.slidingWindow k (S.each (xs :: [Int]))))
      it "behaves like identity when window size is 1" $
        property $ \xs -> toL (S.slidingWindowMin 1 (S.each (xs :: [Int]))) === xs
      it "produces a prefix when the stream elements are sorted" $
        property $ \(Sorted xs) k ->
          (length xs >= k) ==> (toL (S.slidingWindowMin k (S.each (xs :: [Int]))) === take (length xs - (k - 1)) xs)
    describe "slidingWindowMinBy" $ do
      it "prefers earlier elements when several elements compare equal" $ do
        toL (S.slidingWindowMinBy (comparing fst) 2 (S.each [(1, 1), (2, 2), (2, 3), (2, 4)])) `shouldBe`
          [(1, 1), (2, 2), (2, 3)]
      it "behaves like a (S.map (Foldable.minimumBy f)) (slidingWindow) for non-empty streams" $ do
        property $ \(NonEmpty xs) k -- we use NonEmpty because Foldable.minimumBy crashes on empty lists
         ->
          toL (S.slidingWindowMinBy (comparing fst) k (S.each xs)) ===
          toL (S.map (Foldable.minimumBy (comparing fst)) (S.slidingWindow k (S.each (xs :: [(Int, Int)]))))
    describe "slidingWindowMinOn" $ do
      it "behaves like a (S.map (Foldable.minimumBy (comparing p))) (slidingWindow) for non-empty streams" $ do
        property $ \(NonEmpty xs) k -- we use NonEmpty because Foldable.minimumBy crashes on empty lists
         ->
          toL (S.slidingWindowMinOn fst k (S.each xs)) ===
          toL (S.map (Foldable.minimumBy (comparing fst)) (S.slidingWindow k (S.each (xs :: [(Int, Int)]))))
      it "does not force the projected value to WHNF" $
        property $ \xs k ->
          (length xs >= k) ==>
          (toL (S.slidingWindowMinOn (const (undefined :: UnitWithLazyEq)) k (S.each (xs :: [Int]))) ===
           take (length xs - (k - 1)) xs)
    describe "slidingWindowMax" $ do
      it "produces no results with empty streams" $
        property $ \k -> toL (S.slidingWindowMax k (mempty :: S.Stream (S.Of Int) Identity ())) `shouldBe` []
      it "behaves like a (S.map Foldable.maximum) (slidingWindow n s) for non-empty streams" $
        property $ \(NonEmpty xs) k -- we use NonEmpty because Foldable.maximum crashes on empty lists
         ->
          toL (S.slidingWindowMax k (S.each xs)) ===
          toL (S.map Foldable.maximum (S.slidingWindow k (S.each (xs :: [Int]))))
      it "behaves like identity when window size is 1" $
        property $ \xs -> toL (S.slidingWindowMax 1 (S.each (xs :: [Int]))) === xs
      it "produces a suffix when the stream elements are sorted" $
        property $ \(Sorted xs) k ->
          (length xs >= k) ==> (toL (S.slidingWindowMax k (S.each (xs :: [Int]))) === drop (k - 1) xs)
    describe "slidingWindowMaxBy" $ do
      it "prefers later elements when several elements compare equal" $ do
        toL (S.slidingWindowMaxBy (comparing fst) 2 (S.each [(1, 1), (2, 2), (2, 3), (2, -900)])) `shouldBe`
          [(2, 2), (2, 3), (2, -900)]
      it "behaves like a (S.map (Foldable.maximumBy f)) (slidingWindow) for non-empty streams" $ do
        property $ \(NonEmpty xs) k -- we use NonEmpty because Foldable.maximumBy crashes on empty lists
         ->
          toL (S.slidingWindowMaxBy (comparing fst) k (S.each xs)) ===
          toL (S.map (Foldable.maximumBy (comparing fst)) (S.slidingWindow k (S.each (xs :: [(Int, Int)]))))
    describe "slidingWindowMaxOn" $ do
      it "behaves like a (S.map (Foldable.maximumBy (comparing p))) (slidingWindow) for non-empty streams" $ do
        property $ \(NonEmpty xs) k -- we use NonEmpty because Foldable.maximumBy crashes on empty lists
         ->
          toL (S.slidingWindowMaxOn fst k (S.each xs)) ===
          toL (S.map (Foldable.maximumBy (comparing fst)) (S.slidingWindow k (S.each (xs :: [(Int, Int)]))))
      it "does not force the projected value to WHNF" $
        property $ \xs k ->
          (length xs >= k) ==>
          (toL (S.slidingWindowMaxOn (const (undefined :: UnitWithLazyEq)) k (S.each (xs :: [Int]))) === drop (k - 1) xs)

data UnitWithLazyEq = UnitWithLazyEq

instance Eq UnitWithLazyEq where
  _ == _ = True

instance Ord UnitWithLazyEq where
  compare _ _ = EQ
