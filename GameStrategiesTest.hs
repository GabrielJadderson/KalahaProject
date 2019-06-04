{-# LANGUAGE TypeFamilies,FlexibleContexts,TemplateHaskell #-}
module Main where

-- import Game
-- import GameStrategies
import Kalaha
import Data.Bits
import Test.QuickCheck
import Test.QuickCheck.All

-- We implement the well-studied game Nim (http://en.wikipedia.org/wiki/Nim) to use as test game the minimax related functions.

type MaxDepth = Int
data Nim = Nim MaxDepth [Int] deriving Show

nimGame :: Nim -> Game (Int,[Int]) (Int,Int)
nimGame game@(Nim dmax xs) = Game {
    startState = (0,xs),
    move = \player (d,xs) (i,v) -> (not player, (d+1,[if i==j then x-v else x | (j,x) <- zip [0..] xs])),
    moves = \player (d,state) -> [ (i,v) | (i,x) <- zip [0..] state, v <- [1..x]] ,
    value = \player (d,state) -> case (dmax==0 || d <= dmax) of
      True -> theoreticalNimWinner (Nim 0 state) player
      False -> error $ "Value should not be evaluated below level " ++ show dmax,
    showGame = show
  }

-- Theorem. In a normal Nim game, the player making the first move has a winning strategy if and only if the nim-sum of the sizes of the heaps is nonzero. Otherwise, the second player has a winning strategy
theoreticalNimWinner (Nim _ xs) p = (if (foldl xor 0 xs > 0) then 1 else -1) * (if p then 1 else -1)
maxTestNimListsum = 9 :: Int

anyNimGame = do
  a   <- choose (1,maxTestNimListsum)
  xs  <- genlist a
  return $ Nim 0 xs

genlist 0 = return []
genlist m = do
  x   <- choose (1,m)
  xs  <- genlist (m-x)
  return $ x:xs

prop_minimaxPicksWinningStrategyForNim = forAll (arbitrary :: Gen Bool) $ \startingPlayer ->
                   forAll anyNimGame $ \game ->
                   theoreticalNimWinner game startingPlayer == snd (minimax (startTree  (nimGame game) startingPlayer))

prop_alphabetaSolutionShouldEqualMinimaxSolution =   forAll (arbitrary :: Gen Bool) $ \startingPlayer ->
                                        forAll anyNimGame $ \game ->
                                        (snd $ minimax (startTree (nimGame game) startingPlayer)) == (snd $ minimaxAlphaBeta (-1.0/0.0, 1.0/0.0) (startTree  (nimGame game) startingPlayer))

prop_boundedAlphabetaIsOptimalForNim =  forAll (choose (1,5) :: Gen Int) $ \d ->
                                        forAll (arbitrary :: Gen Bool) $ \startingPlayer ->
                                        forAll anyNimGame $ \game@(Nim _ xs) ->
                                        minimaxAlphaBeta (-1.0/0.0, 1.0/0.0) (takeTree d $ startTree (nimGame game) startingPlayer) == minimaxAlphaBeta (-1.0/0.0, 1.0/0.0) (startTree  (nimGame game) startingPlayer)


prop_pruningShouldBeDoneCorrectlyForStaticTestTrees = [minimaxAlphaBeta (-1.0/0.0, 1.0/0.0) (startTree  (staticGame game) True)  | game <- enumFrom (toEnum 0 :: StaticGame)] == [(Just 2,5),(Just 1,6),(Just 0,3),(Just 0,3),(Just 2,13),(Just 0,4),(Just 0,15),(Just 1,7),(Just 0,5),(Just 1,6)]





--- Static game tests:

data StaticTree = StaticNode [StaticTree] | StaticLeaf Double deriving (Eq,Show)
data StaticGame = StaticGame1 | StaticGame2 | StaticGame3 | StaticGame4 | StaticGame5 | StaticGame6 | StaticGame7  | StaticGame8 | StaticGame9 | StaticGame10 deriving (Enum,Show)


staticGame :: StaticGame -> Game [Int] Int
staticGame game = Game {
     startState = [],
     moves = \_ g -> case getStaticNode (staticTree game) g of
                            StaticLeaf _ -> []
                            StaticNode cs -> [0..length cs-1],
     value = \_ g -> case (getStaticNode (staticTree game) g) of
                            StaticNode xs -> error "This should not happen!"
                            StaticLeaf v -> v,
     move = \p g m -> (not p,m:g),
     showGame = show
  }

-- Hjælpehalløj
getStaticNode :: StaticTree -> [Int] -> StaticTree
getStaticNode t [] = t
getStaticNode t (x:xs) = case (getStaticNode t xs) of
                    StaticNode cs -> cs!!x
                    StaticLeaf _  -> error "This should not happen!"


staticTree :: StaticGame -> StaticTree
staticTree StaticGame1 = StaticNode [
    StaticNode [
      StaticNode [
        StaticLeaf 4,
        StaticLeaf 3
      ],
      StaticNode [
        StaticLeaf 6,
        StaticLeaf 2
      ]
    ],
    StaticNode [
      StaticNode [
        StaticLeaf 2,
        StaticLeaf 1
      ],
      StaticNode [
        StaticLeaf 9,
        StaticLeaf 5
      ],
      StaticNode [
        StaticLeaf 3,
        StaticLeaf 1
      ]
    ],
    StaticNode [
      StaticNode [
        StaticLeaf 5,
        StaticLeaf 4
      ],
      StaticNode [
        StaticLeaf 7,
        StaticLeaf 5
      ]
    ]
  ]

-- http://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning
staticTree StaticGame2 = StaticNode [
    StaticNode [
      StaticNode [
        StaticNode [
          StaticLeaf 5,
          StaticLeaf 6
        ],
        StaticNode [
          StaticLeaf 7,
          StaticLeaf 4,
          StaticLeaf 5
        ]
      ],
      StaticNode [
        StaticNode [
          StaticLeaf 3
        ]
      ]
    ],
    StaticNode [
      StaticNode [
        StaticNode [
          StaticLeaf 6
        ],
        StaticNode [
          StaticLeaf 6,
          StaticLeaf 9
        ]
      ],
      StaticNode [
        StaticNode [
          StaticLeaf 7
        ]
      ]
    ],
    StaticNode [
      StaticNode [
        StaticNode [
          StaticLeaf 5
        ]
      ],
      StaticNode [
        StaticNode [
          StaticLeaf 9,
          StaticLeaf 8
        ],
        StaticNode [
          StaticLeaf 6
        ]
      ]
    ]
  ]

staticTree StaticGame9 = StaticNode [
        StaticNode [
          StaticLeaf 5,
          StaticLeaf 6
        ],
        StaticNode [
          StaticLeaf 7,
          StaticLeaf 4,
          StaticLeaf 5
        ]
      ]

staticTree StaticGame10 = StaticNode [
    StaticNode [
      StaticNode [
        StaticNode [
          StaticLeaf 5,
          StaticLeaf 6
        ],
        StaticNode [
          StaticLeaf 7,
          StaticLeaf 4,
          error "Inspected StaticNode to be pruned"
        ]
      ],
      StaticNode [
        StaticNode [
          StaticLeaf 3
        ]
      ]
    ],
    StaticNode [
      StaticNode [
        StaticNode [
          StaticLeaf 6
        ],
        StaticNode [
          StaticLeaf 6,
          error "Inspected StaticNode to be pruned"
        ]
      ],
      StaticNode [
        StaticNode [
          StaticLeaf 7
        ]
      ]
    ],
    StaticNode [
      StaticNode [
        StaticNode [
          StaticLeaf 5
        ]
      ],
      error "Inspected StaticNode to be pruned"
    ]
  ]

-- http://www.cs.uku.fi/~mnykanen/TEK/teklectures5.pdf
staticTree StaticGame3 = StaticNode [
    StaticNode [
      StaticLeaf 3,
      StaticLeaf 12,
      StaticLeaf 8
    ],
    StaticNode [
      StaticLeaf 2,
      StaticLeaf 5,
      StaticLeaf 3
    ],
    StaticNode [
      StaticLeaf 14,
      StaticLeaf 5,
      StaticLeaf 2
    ]
  ]

-- http://cs.ucla.edu/~rosen/161/notes/alphabeta.html
staticTree StaticGame4 = StaticNode [
    StaticNode [
      StaticNode [
        StaticNode [
          StaticLeaf 3,
          StaticLeaf 17
        ],
        StaticNode [
          StaticLeaf 2,
          error "Inspected StaticNode to be pruned"
        ]
      ],
      StaticNode [
        StaticNode [
          StaticLeaf 15
        ],
        error "Inspected StaticNode to be pruned"
      ]
    ],
    StaticNode [
      StaticNode [
        StaticNode [
          StaticLeaf 2,
          error "Inspected StaticNode to be pruned"
        ]
      ],
      StaticNode [
        StaticNode [
          StaticLeaf 3
        ]
      ]
    ]
  ]

--http://repository.cmu.edu/cgi/viewcontent.cgi?article=2700&context=compsci
staticTree StaticGame5 = StaticNode [
    StaticNode [
      StaticNode [
        StaticLeaf (-20),
        StaticLeaf 3
      ],
      StaticNode [
        StaticLeaf 2,
        StaticLeaf 5,
        error "Beta cutoff"
      ],
      StaticNode [
        StaticLeaf 35,
        error "Beta cutoff"
      ]
    ],
    StaticNode [
      StaticNode [
        StaticLeaf (-8),
        StaticLeaf 1,
        StaticLeaf (-1)
      ],
      error "alpha cutoff"
    ],
    StaticNode [
      StaticNode [
        StaticLeaf 7,
        StaticLeaf 13,
        StaticLeaf (-2)
      ],
      StaticNode [
        StaticLeaf 9,
        StaticLeaf 14,
        error "beta cutoff"
      ]
    ],
    StaticNode [
      StaticNode [
        StaticLeaf 3,
        StaticLeaf 2
      ]
    ]
  ]


--http://www.netlib.org/utk/lsi/pcwLSI/text/StaticNode351.html
staticTree StaticGame6 = StaticNode [
    StaticNode [
      StaticNode [
        StaticLeaf 4,
        StaticLeaf (-5)
      ],
      StaticNode [
        StaticLeaf 20,
        error "alpha-beta prune"
      ],
      StaticNode [
        StaticLeaf 13,
        error "alpha-beta prune"
      ]
    ],
    StaticNode [
      StaticNode [
        StaticLeaf (-1),
        StaticLeaf (-12)
      ],
      error "alpha-beta prune"
    ],
    StaticNode [
      StaticNode [
        StaticLeaf (-7),
        StaticLeaf (-8)
      ],
      error "alpha-beta prune"
    ],
    StaticNode [
      StaticNode [
        StaticLeaf (-11),
        StaticLeaf (-40),
        StaticLeaf (-19)
      ],
      error "alpha-beta prune"
    ]
  ]
--http://will.thimbleby.net/algorithms/doku.php?id=minimax_search_with_alpha-beta_pruning
staticTree StaticGame7 = StaticNode [
    StaticNode [
      StaticLeaf 15
    ],
    StaticNode [
      StaticNode [
        StaticLeaf 9
      ],
      error "alpha-beta prune"
    ],
    StaticNode [
      StaticNode [
        StaticNode [
          StaticLeaf 5
        ]
      ],
      error "alpha-beta prune"
    ]
  ]


--http://www.emunix.emich.edu/~evett/AI/AlphaBeta_movie/sld018.htm
staticTree StaticGame8 = StaticNode [
    StaticNode [
      StaticNode [
        StaticNode [
          StaticLeaf 5,
          StaticLeaf 11
        ],
        StaticNode [
          StaticLeaf 2,
          error "alpha-beta prune"
        ]
      ],
      StaticNode [
        StaticNode [
          StaticLeaf 13
        ],
        error "alpha-beta prune"
      ]
    ],
    StaticNode [
      StaticNode [
        StaticLeaf 3,
        StaticLeaf 7
      ],
      StaticNode [
        StaticLeaf 9,
        error "alpha-beta prune"
      ]
    ]
  ]


return []
main = $quickCheckAll
