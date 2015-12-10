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
import qualified Data.List.Split as Split
import Control.Exception
main = catch_ (defaultMain props) (\(SomeException e) -> print e)
 where catch_ a b = a 

-- tests :: TestTree
-- tests = testGroup "Tests" [properties]
--
-- properties :: TestTree
-- properties =  qcProps


props = testGroup "(checked by QuickCheck)"
  ["map succ" ||| map_test succ
  , "drop 3" ||| drop_test 3
  , "dropWhile (<10)" ||| dropWhile_test (<10)
  , "filter even" ||| filter_test even
  , "break even" ||| break_test even
  , "concat" |||| concat_test 
  , "cons 17" ||| cons_test 17
  , "intersperse 17" ||| intersperse_test 17
  , "all even" ||| all_test even
  , "any even" ||| any_test even
  , "elem 17" ||| elem_test 17
  , "fold (\\x y -> 2*x+1) 17" ||| fold_test (\x y -> 2*x+1) 17
  , "head" ||| head_test
  , "last" ||| last_test
  , "sum" ||| sum_test
  , "product" ||| product_test
  , "group" |||| group_test
  , "groupBy signum==" ||| groupBy_test (\x y -> signum x == signum y)
  , "chunksOf 2" |||| chunksOf_test 1
  , "nub" ||| nub_test
  , "scan" ||| scan_test (+) 0
  , "repeat" |||| repeat_test
  , "iterate succ" ||||| iterate_test succ
  ]


-- [a] -> [a]
map_test f          = eq_str (map f) (S.map f) 
drop_test n         = eq_str (drop n) (S.drop n)
dropWhile_test thus = eq_str (dropWhile thus) (S.dropWhile thus)
filter_test f       = eq_str (filter f) (S.filter f)
break_test thus     = eq_str (fst . break thus) (S.drained . S.break thus)
concat_test         = eq_str concat (S.concat)
cons_test a         = eq_str (a :) (S.cons a)
intersperse_test a  = eq_str (List.intersperse a) (S.intersperse a)
nub_test            = eq_str (List.nub) (S.nub)
scan_test op seed   = eq_str (List.scanl' op seed) (S.scan op seed id)

-- [a] -> x
all_test thus       = eq_fold (all thus) (S.all thus)
any_test thus       = eq_fold (any thus) (S.any thus)
elem_test a         = eq_fold (elem a) (S.elem a)
fold_test op seed   = eq_fold (List.foldl' op seed) (S.fold op seed id)
head_test           = eq_fold headMay S.head
last_test           = eq_fold lastMay S.last
length_test         = eq_fold (length :: [Int] -> Int) S.length
sum_test            = eq_fold sum S.sum
product_test        = eq_fold product S.product

-- [a] -> [[a]]
group_test          = eq_strs List.group S.group
groupBy_test cmp    = eq_strs (List.groupBy cmp) (S.groupBy cmp)
chunksOf_test n     = eq_strs (Split.chunksOf m) (chunksOf m) where m = abs n + 1

-- x -> [a]
iterate_test step   = eq_unfold (take 100 . iterate step) (S.take 100 . S.iterate step)
repeat_test         = eq_unfold (take 100 . repeat ) (S.take 100 . S.repeat)    

eq_fold f g ls = f ls =^= (S.fst' (runIdentity (g (S.each ls))))
eq_str f g ls = f ls =^= runIdentity (S.toList_ (g (S.each ls)))
eq_strs f g ls = f ls =^= runIdentity (S.toList_ (mapped S.toList (g (S.each ls))))
-- eq_fold f g ls = f ls =^= (S.fst' (runIdentity (g (S.mapM return $ S.each ls))))
-- eq_str f g ls = f ls =^= runIdentity (S.toList_ (g (S.mapM return $ S.each ls)))
-- eq_strs f g ls = f ls =^= runIdentity (S.toList_ (mapped S.toList (g (S.mapM return $ S.each ls))))
eq_unfold f g seed = f seed =^= runIdentity (S.toList_ (g seed))


(|||) str t =  QC.testProperty str (qq t)
(||||) str t =  QC.testProperty str (qqq t)
(|||||) str t = QC.testProperty str (q t)
q :: (Int -> Bool) -> Property
q = forAll arbitrary
qq :: ([Int] -> Bool) -> Property
qq = forAll arbitrary
qqq :: ([[Int]] -> Bool) -> Property
qqq = forAll arbitrary

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

