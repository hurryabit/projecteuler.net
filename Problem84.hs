{-# LANGUAGE TemplateHaskell #-}
module Problem84 where

import Control.Applicative
import Control.Monad.State hiding (get)
import Control.Monad.Trans
import Data.Array.IO
import Data.List
import Data.Ord
import Data.Record.Label
import System.Random
import System.Random.Shuffle

data Field = GO   | A1 | CC1 | A2  | T1  | R1 | B1  | CH1 | B2 | B3
           | JAIL | C1 | U1  | C2  | C3  | R2 | D1  | CC2 | D2 | D3
           | FP   | E1 | CH2 | E2  | E3  | R3 | F1  | F2  | U2 | F3
           | G2J  | G1 | G2  | CC3 | G3  | R4 | CH3 | H1  | T2 | H2
  deriving (Eq, Ord, Enum, Bounded, Ix, Show, Read)

type MoveInstruction = ([Field] -> [Field],String)

type Card = Maybe MoveInstruction

goto :: Field -> MoveInstruction
goto f = (goto' (== f),"Goto " ++ show f)

gotoNextR :: MoveInstruction
gotoNextR = (goto' (`elem` [R1,R2,R3,R4]),"Goto next R")

gotoNextU :: MoveInstruction
gotoNextU = (goto' (`elem` [U1,U2]),"Goto next U")

goBack3 :: MoveInstruction
goBack3 = (drop 37,"Go back 3")

goto' :: (Field -> Bool) -> ([Field] -> [Field])
goto' p = dropWhile (not . p)

fields :: [Field]
fields = [minBound..maxBound]

ccCards :: [Card]
ccCards = Just (goto GO):Just (goto JAIL):replicate 14 Nothing

chCards :: [Card]
chCards = map Just (map goto [GO,JAIL,C1,E3,H2,R1] ++
  [gotoNextR,gotoNextR,gotoNextU,goBack3]) ++ replicate 6 Nothing

isCcField :: Field -> Bool
isCcField = flip elem [CC1,CC2,CC3]

isChField :: Field -> Bool
isChField = flip elem [CH1,CH2,CH3]

data GameState = GameState
  { _gsFields  :: [Field]
  , _gsCcCards :: [Card]
  , _gsChCards :: [Card]
  , _gsDoubles :: Int
  , _gsStats   :: IOUArray Field Int
  }

$(mkLabels [''GameState])

trace :: String -> StateT GameState IO ()
trace = const $ return () -- liftIO . putStr

simulation :: IO ()
simulation = do
  gen1 <- newStdGen
  gen2 <- newStdGen
  stats <- newArray (minBound,maxBound) 0
  gs <- execStateT (replicateM_ 1000000 run) $ GameState
          { _gsFields   = cycle fields
          , _gsCcCards  = cycle $ shuffle' ccCards 16 gen1
          , _gsChCards  = cycle $ shuffle' chCards 16 gen2
          , _gsDoubles  = 0
          , _gsStats    = stats
          }
  getAssocs (get gsStats gs) >>= mapM_ print . take 10 . sortBy (flip (comparing snd))

run :: StateT GameState IO ()
run = do
  let dice :: IO Int
      dice = randomRIO (1,4)
  (d1,d2) <- liftIO $ liftM2 (,) dice dice
  trace $ "Rolled " ++ show (d1,d2)
  if d1 == d2 then modM gsDoubles (+1) else setM gsDoubles 0
  doubles <- getM gsDoubles
  if doubles == 3 then do
      trace " (3rd double)"
      modM gsFields (goto' (==JAIL))
    else
      modM gsFields (drop (d1+d2))
  field1 <- head <$> getM gsFields
  trace $ " landed on " ++ show field1
  let special
        | field1 == G2J     = do
            trace " (we go to JAIL)"
            modM gsFields (goto' (==JAIL))
        | isCcField field1  = handleCard gsCcCards
        | isChField field1  = handleCard gsChCards
        | otherwise         = return ()
  special
  field2 <- head <$> getM gsFields
  when (field2 == JAIL) $ setM gsDoubles 0
  stats <- getM gsStats
  liftIO $ readArray stats field2 >>= writeArray stats field2 . (+1)
  trace "\n"
  where
    handleCard stack = do
      card <- head <$> getM stack
      modM stack tail
      case card of
        Nothing -> trace " (unimportant card)"
        Just (t,l) -> do
          trace $ " (" ++ l ++ ")"
          modM gsFields t
          field <- head <$> getM gsFields
          trace $ " now we are on " ++ show field

main = simulation
