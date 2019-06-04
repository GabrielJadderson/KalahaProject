{-# LANGUAGE TemplateHaskell #-}
module Main where

import Kalaha
-- import Game

import Data.Map (Map)
import Data.List
import qualified Data.Map as M
import Data.Maybe


import Test.QuickCheck
import Test.QuickCheck.All


instance Arbitrary Kalaha where arbitrary = sized $ \n -> sized $ \m -> return $ Kalaha (n+1) (m+1)

symKalaha = sized $ \n -> return $ Kalaha (n+1) (n+1)
anyKalaha = sized $ \n -> sized $ \m -> return $ Kalaha (n+1) (m+1)

arbitraryState :: Kalaha -> Gen KState
arbitraryState (Kalaha n m)   = do  xs <- sequence $ replicate (2*n*m) $ choose (0, 2*n - 1)
                                    return [ length $ filter (==i) xs | i <- [0..2*n + 1]] 

arbitraryMove g p s = case xs of
    []  -> return Nothing
    _   -> do
      i <- choose (0, length xs - 1)
      return $ Just $ xs !! i
  where xs = movesImpl g p s 

prop_startStateLen :: Kalaha -> Bool
prop_startStateLen g@(Kalaha n m)     = (length $ startStateImpl g) == 2 * (n+1) 


prop_startStateSum :: Kalaha -> Bool
prop_startStateSum g@(Kalaha n m)     = (sum $ startStateImpl g) == 2 * n * m


prop_startStateValue :: Kalaha -> Bool 
prop_startStateValue g@(Kalaha n m)   = and [ (if (i `mod` (n+1) == n) then 0 else m) == x | (i,x) <- [0..] `zip` startStateImpl g ]

getSymmetricState :: Kalaha -> KState -> KState
getSymmetricState (Kalaha n _) xs = case splitAt (n+1) xs of (xs1,xs2) -> xs2 ++ xs1

prop_startStateSymmetric :: Kalaha -> Bool
prop_startStateSymmetric g = (getSymmetricState g $ startStateImpl g) == startStateImpl g


prop_valueSymmetric = forAll anyKalaha $ \g@(Kalaha n _) -> 
  forAll (arbitraryState g) $ \s -> 
  valueImpl g s == - valueImpl g (getSymmetricState g s)




prop_movesSymmetric = forAll anyKalaha $ \g@(Kalaha n _) -> 
                      forAll (arbitrary :: Gen Bool) $ \p ->
                      forAll (arbitraryState g) $ \s -> 
                      movesImpl g p s == (map (\i -> (i+n+1) `mod` (2*n + 2) ) $ movesImpl g (not p) (getSymmetricState g s))

prop_moveSymmetric = forAll anyKalaha $ \g@(Kalaha n _) -> 
  forAll (arbitrary :: Gen Bool) $ \p ->
  forAll (arbitraryState g) $ \s -> 
  forAll (arbitraryMove g p s) $ \iOpt ->
    case iOpt of
      Nothing -> True
      Just i  -> let  (_, s1) = moveImpl g p s i
                      (_, s2) = moveImpl g (not p) (getSymmetricState g s) ((i+n+1) `mod` (2*n+2) )
                 in   s1 == getSymmetricState g s2


prop_moveDoesntChangeSum = forAll anyKalaha $ \g -> 
                                        forAll (arbitrary :: Gen Bool) $ \p ->
                                        forAll (arbitraryState g) $ \s -> 
                                        forAll (arbitraryMove g p s) $ \iOpt ->
                                          case iOpt of
                                            Nothing -> True
                                            Just i  -> let  (_, s') = moveImpl g p s i
                                                       in   sum s == sum s'

prop_specificMoves = let g = Kalaha 6 6 in and [
    moveImpl g False [6,6,6,6,6,6,0,6,6,6,6,6,6,0] 0 == (False,[0,7,7,7,7,7,1,6,6,6,6,6,6,0]),
    moveImpl g False [0,7,7,7,7,7,1,6,6,6,6,6,6,0] 3 == (True,[0,7,7,0,8,8,2,7,7,7,7,6,6,0]),
    moveImpl g True [0,7,7,0,8,8,2,7,7,7,7,6,6,0] 9 == (False,[1,8,8,0,8,8,2,7,7,0,8,7,7,1]),
    moveImpl g False [1,8,8,0,8,8,2,7,7,0,8,7,7,1] 1 == (True,[1,0,9,1,9,9,3,8,8,1,8,7,7,1]),
    moveImpl g True [1,0,9,1,9,9,3,8,8,1,8,7,7,1] 10 == (False,[2,1,10,2,10,9,3,8,8,1,0,8,8,2]),
    moveImpl g False [2,1,10,2,10,9,3,8,8,1,0,8,8,2] 4 == (True,[3,2,10,2,0,10,4,9,9,2,1,9,9,2]),
    moveImpl g True [3,2,10,2,0,10,4,9,9,2,1,9,9,2] 9 == (False,[3,2,10,2,0,10,4,9,9,0,2,10,9,2]),
    moveImpl g False [3,2,10,2,0,10,4,9,9,0,2,10,9,2] 3 == (True,[3,2,10,0,1,11,4,9,9,0,2,10,9,2]),
    moveImpl g True [3,2,10,0,1,11,4,9,9,0,2,10,9,2] 7 == (False,[4,3,11,0,1,11,4,0,10,1,3,11,10,3]),
    moveImpl g False [4,3,11,0,1,11,4,0,10,1,3,11,10,3] 5 == (True,[5,4,12,0,1,0,8,1,11,0,4,12,11,3]),
    moveImpl g True [5,4,12,0,1,0,8,1,11,0,4,12,11,3] 7 == (False,[5,4,12,0,1,0,8,0,12,0,4,12,11,3]),
    moveImpl g False [5,4,12,0,1,0,8,0,12,0,4,12,11,3] 1 == (True,[5,0,13,1,2,0,9,0,12,0,4,12,11,3]),
    moveImpl g True [5,0,13,1,2,0,9,0,12,0,4,12,11,3] 8 == (False,[6,1,14,2,3,0,9,0,0,1,5,13,12,6]),
    moveImpl g False [6,1,14,2,3,1,9,1,0,1,5,13,12,4] 5 == (False,[6,1,14,2,3,0,10,1,0,1,5,13,12,4]),
    moveImpl g False [6,1,14,2,3,0,10,1,0,1,5,13,12,4] 0 == (False,[0,2,15,3,4,1,11,1,0,1,5,13,12,4]),
    moveImpl g False [0,2,15,3,4,1,11,1,0,1,5,13,12,4] 5 == (False,[0,2,15,3,4,0,12,1,0,1,5,13,12,4]),
    moveImpl g False [0,2,15,3,4,0,12,1,0,1,5,13,12,4] 3 == (False,[0,2,15,0,5,1,13,1,0,1,5,13,12,4]),
    moveImpl g False [0,2,15,0,5,1,13,1,0,1,5,13,12,4] 5 == (False,[0,2,15,0,5,0,14,1,0,1,5,13,12,4]),
    moveImpl g False [0,2,15,0,5,0,14,1,0,1,5,13,12,4] 2 == (True,[1,3,1,2,7,1,15,2,1,2,6,14,13,4]),
    moveImpl g True [1,3,1,2,7,1,15,2,1,2,6,14,13,4] 12 == (False,[0,4,2,3,8,2,15,3,2,3,7,15,0,8]),
    moveImpl g False [0,4,2,3,8,2,15,3,2,3,7,15,0,8] 3 == (False,[0,4,2,0,9,3,16,3,2,3,7,15,0,8]),
    moveImpl g False [0,4,2,0,9,3,16,3,2,3,7,15,0,8] 4 == (True,[0,4,2,0,0,4,19,4,3,4,8,16,0,8]),
    moveImpl g True [0,4,2,0,0,4,19,4,3,4,8,16,0,8] 9 == (True,[0,4,2,0,0,4,19,4,3,0,9,17,1,9]),
    moveImpl g True [0,4,2,0,0,4,19,4,3,0,9,17,1,9] 12 == (True,[0,4,2,0,0,4,19,4,3,0,9,17,0,10]),
    moveImpl g True [0,4,2,0,0,4,19,4,3,0,9,17,0,10] 11 == (False,[2,6,3,1,1,5,19,5,4,1,10,1,2,12]),
    moveImpl g False [2,6,3,1,1,5,19,5,4,1,10,1,2,12] 0 == (True,[0,7,4,1,1,5,19,5,4,1,10,1,2,12]),
    moveImpl g True [0,7,4,1,1,5,19,5,4,1,10,1,2,12] 8 == (False,[0,7,4,1,1,5,19,5,0,2,11,2,3,12]),
    moveImpl g False [0,7,4,1,1,5,19,5,0,2,11,2,3,12] 2 == (False,[0,7,0,2,2,6,20,5,0,2,11,2,3,12]),
    moveImpl g False [0,7,0,2,2,6,20,5,0,2,11,2,3,12] 1 == (True,[0,0,1,3,3,7,21,6,1,2,11,2,3,12]),
    moveImpl g True [0,0,1,3,3,7,21,6,1,2,11,2,3,12] 11 == (True,[0,0,1,3,3,7,21,6,1,2,11,0,4,13]),
    moveImpl g True [0,0,1,3,3,7,21,6,1,2,11,0,4,13] 7 == (True,[0,0,1,3,3,7,21,0,2,3,12,1,5,14]),
    moveImpl g True [0,0,1,3,3,7,21,0,2,3,12,1,5,14] 10 == (False,[1,1,2,4,4,8,21,1,3,4,0,2,6,15]),
    moveImpl g False [1,1,2,4,4,8,21,1,3,4,0,2,6,15] 5 == (True,[2,1,2,4,4,0,22,2,4,5,1,3,7,15]),
    moveImpl g True [2,1,2,4,4,0,22,2,4,5,1,3,7,15] 8 == (False,[2,1,2,4,4,0,22,2,0,6,2,4,8,15]),
    moveImpl g False [2,1,2,4,4,0,22,2,0,6,2,4,8,15] 3 == (True,[2,1,2,0,5,1,23,3,0,6,2,4,8,15]),
    moveImpl g True [2,1,2,0,5,1,23,3,0,6,2,4,8,15] 9 == (False,[3,2,2,0,5,1,23,3,0,0,3,5,9,16]),
    moveImpl g False [3,2,2,0,5,1,23,3,0,0,3,5,9,16] 5 == (False,[3,2,2,0,5,0,24,3,0,0,3,5,9,16]),
    moveImpl g False [3,2,2,0,5,0,24,3,0,0,3,5,9,16] 4 == (True,[3,2,2,0,0,1,25,4,1,1,3,5,9,16]),
    moveImpl g True [3,2,2,0,0,1,25,4,1,1,3,5,9,16] 10 == (True,[3,2,2,0,0,1,25,4,1,1,0,6,10,17]),
    moveImpl g True [3,2,2,0,0,1,25,4,1,1,0,6,10,17] 9 == (False,[3,2,0,0,0,1,25,4,1,0,0,6,10,20]),
    moveImpl g False [3,2,0,0,0,1,25,4,1,0,0,6,10,20] 5 == (False,[3,2,0,0,0,0,26,4,1,0,0,6,10,20]),
    moveImpl g False [3,2,0,0,0,0,26,4,1,0,0,6,10,20] 1 == (True,[3,0,1,0,0,0,27,4,1,0,0,6,10,20]),
    moveImpl g True [3,0,1,0,0,0,27,4,1,0,0,6,10,20] 8 == (False,[3,0,1,0,0,0,27,4,0,0,0,6,10,21]),
    moveImpl g False [3,0,1,0,0,0,27,4,0,0,0,6,10,21] 2 == (True,[3,0,0,0,0,0,28,4,0,0,0,6,10,21]),
    moveImpl g True [3,0,0,0,0,0,28,4,0,0,0,6,10,21] 7 == (False,[3,0,0,0,0,0,28,0,1,1,1,7,10,21]),
    moveImpl g False [3,0,0,0,0,0,28,0,1,1,1,7,10,21] 0 == (True,[0,1,1,0,0,0,30,0,1,0,1,7,10,21]),
    moveImpl g True [0,1,1,0,0,0,30,0,1,0,1,7,10,21] 11 == (False,[1,2,2,1,1,0,30,0,1,0,1,0,11,22]),
    moveImpl g False [1,2,2,1,1,0,30,0,1,0,1,0,11,22] 3 == (True,[1,2,2,0,2,0,30,0,1,0,1,0,11,22]),
    moveImpl g True [1,2,2,0,2,0,30,0,1,0,1,0,11,22] 8 == (False,[1,2,2,0,2,0,30,0,0,0,1,0,11,23]),
    moveImpl g False [1,2,2,0,2,0,30,0,0,0,1,0,11,23] 1 == (True,[1,0,3,0,2,0,31,0,0,0,1,0,11,23]),
    moveImpl g True [1,0,3,0,2,0,31,0,0,0,1,0,11,23] 10 == (False,[1,0,3,0,2,0,31,0,0,0,0,0,11,24]),
    moveImpl g False [1,0,3,0,2,0,31,0,0,0,0,0,11,24] 4 == (False,[1,0,3,0,0,1,32,0,0,0,0,0,11,24]),
    moveImpl g False [1,0,3,0,0,1,32,0,0,0,0,0,11,24] 5 == (False,[1,0,3,0,0,0,33,0,0,0,0,0,11,24]),
    moveImpl g False [1,0,3,0,0,0,33,0,0,0,0,0,11,24] 2 == (True,[1,0,0,1,1,0,34,0,0,0,0,0,11,24]),
    moveImpl g True [1,0,0,1,1,0,34,0,0,0,0,0,11,24] 12 == (False,[2,1,0,2,2,1,34,1,1,1,0,0,0,27]),
    moveImpl g False [2,1,0,2,2,1,34,1,1,1,0,0,0,27] 5 == (False,[2,1,0,2,2,0,35,1,1,1,0,0,0,27]),
    moveImpl g False [2,1,0,2,2,0,35,1,1,1,0,0,0,27] 4 == (False,[2,1,0,2,0,1,36,1,1,1,0,0,0,27]),
    moveImpl g False [2,1,0,2,0,1,36,1,1,1,0,0,0,27] 5 == (False,[2,1,0,2,0,0,37,1,1,1,0,0,0,27]),
    moveImpl g False [2,1,0,2,0,0,37,1,1,1,0,0,0,27] 3 == (True,[2,1,0,0,1,0,39,0,1,1,0,0,0,27]),
    moveImpl g True [2,1,0,0,1,0,39,0,1,1,0,0,0,27] 8 == (False,[2,1,0,0,1,0,39,0,0,2,0,0,0,27]),
    moveImpl g False [2,1,0,0,1,0,39,0,0,2,0,0,0,27] 1 == (True,[2,0,0,0,1,0,40,0,0,2,0,0,0,27]),
    moveImpl g True [2,0,0,0,1,0,40,0,0,2,0,0,0,27] 9 == (False,[2,0,0,0,1,0,40,0,0,0,1,0,0,28]),
    moveImpl g False [2,0,0,0,1,0,40,0,0,0,1,0,0,28] 0 == (True,[0,0,0,0,0,0,44,0,0,0,0,0,0,28])
  ]

-- Move doesn't change the sum of 

return []
main = $quickCheckAll
