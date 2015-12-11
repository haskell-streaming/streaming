{-#LANGUAGE ScopedTypeVariables #-}
-- import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property (Property(..))
import Test.Tasty.QuickCheck as QC
import Test.Tasty
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (SomeException, bracket, bracket_, evaluate, try)
import Control.Monad (when)
import Debug.Trace (trace)  
import qualified Streaming.Prelude as S
import Streaming
import Data.Functor.Identity
import Safe
import qualified Data.List as List
import Data.Either
import qualified Data.List.Split as Split
import Control.Exception

main = catch (defaultMain tests) (\(SomeException e) -> print e)

tests :: TestTree
tests = testGroup "Tests" [list_like_props]

-- properties :: TestTree
-- properties =  qcProps


list_like_props = testGroup "(checked by QuickCheck)"
  ["map (+n)"        /// (\(n,ns) -> map_test (+n) ns)
  , "drop"           /// uncurry drop_test 
  , "dropWhile (<n)" /// (\(n,ns) -> dropWhile_test (<n) ns)
  , "take"           /// uncurry take_test 
  , "takeWhile (<n)" /// (\(n,ns) -> takeWhile_test (<n) ns)
  , "filter (<n)"    /// (\(n,ns) -> filter_test (< n) ns)
  , "break even"     |||  break_test even
  , "break (<n)"     /// (\(n,ns) -> break_test (<n) ns)
  , "concat"        |||| concat_test 
  , "cons n"         /// uncurry cons_test 
  , "intersperse n"  /// uncurry intersperse_test 
  , "all even"       ||| all_test even
  , "any even"       ||| any_test even
  , "elem n"         /// uncurry elem_test
  , "fold ((+).(*n))" /// (\(n,ns) -> fold_test (\x y -> n*x+y) 0 ns)
  , "head"           ||| head_test
  , "last"           ||| last_test
  , "sum"            ||| sum_test
  , "product"        ||| product_test
  , "group"          |||| group_test
  , "groupBy mod=="  /// (\(n,ns) -> groupBy_test (mod' n) ns)
  , "chunksOf 2"    |||| chunksOf_test 1
  , "chunksOf 17"   |||| chunksOf_test 16
  , "nub"            ||| nub_test
  , "scan"           /// uncurry (scan_test (+))
  , "repeat"         |||| repeat_test
  , "iterate succ"   ||||| iterate_test succ
  , "replicate n"    //// replicate_test 
  , "replicate 0"    ||| replicate0_test
  , "unzip"          /|/ unzip_test
  , "partitionEithers" /||/ partitionEithers_test
  , "partition even" ||| partition_test even
  
  ]

mkpair (a :> (b :> r)) = (b,a)
mod' n x y = let nn = abs n + 1 in mod x nn == mod y nn
-- [a] -> [a]
map_test f          = eq_str (map f) (S.map f) 
drop_test n         = eq_str (drop n) (S.drop n)
dropWhile_test thus = eq_str (dropWhile thus) (S.dropWhile thus)
take_test n         = eq_str (take n) (S.take n)
takeWhile_test thus = eq_str (takeWhile thus) (S.takeWhile thus)
filter_test f       = eq_str (filter f) (S.filter f)
break_test thus     = eq_str (fst . break thus) (S.drained . S.break thus)
concat_test         = eq_str concat (S.concat)
cons_test a         = eq_str (a :) (S.cons a)
intersperse_test a  = eq_str (List.intersperse a) (S.intersperse a)
nub_test            = eq_str (List.nub) (S.nub)
scan_test op seed   = eq_str (List.scanl op seed) (S.scan op seed id)
unzip_test          = eq_pr  unzip S.unzip
partitionEithers_test = eq_pr partitionEithers S.partitionEithers
partition_test p    = eq_pr (List.partition p) (S.partition p)

-- [a] -> x
all_test thus       = eq_fold (all thus) (S.all thus)
any_test thus       = eq_fold (any thus) (S.any thus)
elem_test a         = eq_fold (elem a) (S.elem a)
fold_test op seed   = eq_fold (List.foldl' op seed) (S.fold op seed id)
head_test           = eq_fold headMay S.head
last_test           = eq_fold lastMay S.last
length_test         = eq_fold length S.length
sum_test            = eq_fold sum S.sum
product_test        = eq_fold product S.product
-- [a] -> [[a]]
group_test          = eq_strs List.group S.group
groupBy_test cmp    = eq_strs (List.groupBy cmp) (S.groupBy cmp)
chunksOf_test n     = eq_strs (Split.chunksOf m) (chunksOf m) where m = abs n + 1

-- x -> [a]
iterate_test step   = eq_unfold (take 100 . iterate step) (S.take 100 . S.iterate step)
repeat_test         = eq_unfold (take 100 . repeat ) (S.take 100 . S.repeat)    
replicate_test (n,m) = eq_unfold (replicate n) (S.replicate n) m
replicate0_test     = eq_unfold (replicate 0) (S.replicate 0)


eq_fold f g ls = f ls =^= (S.fst' (runIdentity (g (S.each ls))))
eq_str f g ls  = f ls =^= runIdentity (S.toList_ (g (S.each ls)))
eq_strs f g ls = f ls =^= runIdentity (S.toList_ (mapped S.toList (g (S.each ls))))
eq_pr f g ls = f ls =^= mkpair (runIdentity ((S.toList (S.toList (g (S.each ls))))))
-- eq_fold f g ls = f ls =^= (S.fst' (runIdentity (g (S.mapM return $ S.each ls))))
-- eq_str f g ls = f ls =^= runIdentity (S.toList_ (g (S.mapM return $ S.each ls)))
-- eq_strs f g ls = f ls =^= runIdentity (S.toList_ (mapped S.toList (g (S.mapM return $ S.each ls))))
eq_unfold f g seed = f seed =^= runIdentity (S.toList_ (g seed))


(|||) txt t   =  QC.testProperty txt (qq t)
(||||) txt t  =  QC.testProperty txt (qqq t)
(|||||) txt t =  QC.testProperty txt (q t)
(///) txt t   =  QC.testProperty txt (qw t)
(////) txt t  =  QC.testProperty txt (qy t)
(/|/) txt t   =  QC.testProperty txt (qq' t)
(/||/) txt t   =  QC.testProperty txt (qq'' t)


qy :: ((Int,Int) -> Bool) -> Property
qy = forAll arbitrary
qw :: ((Int,[Int]) -> Bool) -> Property
qw = forAll arbitrary
q :: (Int -> Bool) -> Property
q = forAll arbitrary
qq :: ([Int] -> Bool) -> Property
qq = forAll arbitrary
qq' :: ([(Int,Int)] -> Bool) -> Property
qq' = forAll arbitrary
qq'' :: ([Either Int Int] -> Bool) -> Property
qq'' = forAll arbitrary
qqq :: ([[Int]] -> Bool) -> Property
qqq = forAll arbitrary


-- ------------------------------------------------
-- from the text test machinery, modified for tasty
-- ------------------------------------------------

-- Do two functions give the same answer?
eq :: (Eq a, Show a) => (t -> a) -> (t -> a) -> t -> Bool
eq a b s  = a s =^= b s 
  -- Ensure that two potentially bottom values (in the sense of crashing
  -- for some inputs, not looping infinitely) either both crash, or both
  -- give comparable results for some input.
(=^=) :: (Eq a, Show a) => a -> a -> Bool
i =^= j = unsafePerformIO $ do
  x <- try (evaluate i)
  y <- try (evaluate j)
  case (x,y) of
    (Left (_ :: SomeException), Left (_ :: SomeException))
                       -> return True
    (Right a, Right b) -> return (a == b)
    e                  -> trace ("*** Divergence: " ++ show e) return False
infix 4 =^=
{-# NOINLINE (=^=) #-}

