-- module MainTestRun where
import Data.Map (Map)
import Kalaha
import System.Environment
import Control.Monad
import Text.Read
import Data.Map (Map)
import Kalaha
import Text.Read
import System.Random


data TreeAlg = Minimax | AlphaBeta  deriving (Show,Read)
type Alg = (TreeAlg, Int)

fromBoard = const id        
fromMove = const id
toBoard = const id

uniformly res gen = case length res of
    0 -> Nothing
    1 -> Just $ head res
    _ -> Just $ res !! i where (i,_) = randomR (0, length res - 1) gen

getNextMove :: (Ord m) => String -> Game s m -> s -> Player -> IO (Maybe m)
getNextMove conf g s p = case (readMaybe conf :: Maybe Alg) of
    Just (Minimax, d)    -> return $ fst $ minimax $ takeTree d $ tree g (p,fromBoard g s)
    Just (AlphaBeta, d)  -> return $ fst $ minimaxAlphaBeta (-1.0/0.0, 1.0/0.0) $ takeTree d $ tree g (p,fromBoard g s)
    
    _ -> do
        print $ "Could not parse " ++ conf
        return Nothing

playCaller :: String -> (String,String) -> Player -> IO ()
playCaller gConf cs p = 
    case (readMaybe gConf :: Maybe Kalaha)  of 
        Just g -> play (kalahaGame g) cs (p, startState $ kalahaGame g)
        Nothing -> putStrLn $ "could not parse game" ++ (gConf)

play :: (Show s,Show m,Ord m) => Game s m -> (String,String) -> (Player, s) -> IO ()
play g cs@(defenderConf, attackerConf) (p,s) =
    let conf = if p then attackerConf else defenderConf
    in getNextMove conf g (toBoard g s) p >>= (\m ->
        handleMove g cs p s (fmap (fromMove g) m) play
    )

handleMove g cs p s m rec = do
--    putStrLn $ GGame g s
    case m of
        Nothing -> putStrLn "No available moves!"
        Just m -> do
            putStrLn $ "moveImpl g " ++ show p ++ " " ++ show s ++ " " ++ show m ++ " == " ++ (show $ move g p s m)
            rec g cs (move g p s m)

main = do
        --let g = Kalaha 6 6
        --let s = startState g
        xs <- getArgs
        case xs of
            [gc, x]    -> playCaller gc (x,x) False
            (gc:x:y:_) -> playCaller gc (x,y) False
            _          -> putStrLn desc
    where
        desc = "Usage: runhaskell MainTestRun.hs alg1 [alg2]\nExample: runhaskell MainTestRun.hs \"Kalaha 6 6\" \"(AlphaBeta, 5)\" \"(Minimax, 2)\""