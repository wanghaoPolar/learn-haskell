{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

randomDieValues :: Int -> IO[DieValue]
randomDieValues n =
  replicateM n $ evalRandIO die

generatePairs :: [DieValue] -> [DieValue] -> [(Army, Army)]
generatePairs attackers defenders =
  zip (sortBy (flip compare) (map unDV attackers))
      (sortBy (flip compare) (map unDV defenders) ++ repeat 0)

battleResult :: [DieValue] -> [DieValue] -> (Army, Army)
battleResult attackers defenders =
  foldr (\(a, d) (as, ds) -> if a > d then (as, ds - 1) else (as - 1, ds))
        (attackNum, defenderNum)
        pairs
      where
        attackNum = length attackers
        defenderNum = length defenders
        pairs = generatePairs attackers defenders

getBattle :: Battlefield -> (Army, Army)
getBattle (Battlefield attackers defenders) =
  (attackNum, denfenNum)
  where
    attackNum = min 3 (attackers - 1)
    denfenNum = min 2 defenders

sampleBattlefiled :: Battlefield
sampleBattlefiled = Battlefield 6 6

battle :: Battlefield -> Rand StdGen Battlefield
battle battlefiled =
  let
    (a', d') = getBattle battlefiled
  in
    replicateM a' die >>= \ra ->
      replicateM d' die >>= \rb ->
      let
        (attacerkRemain, defenderRemain) = battleResult ra rb
        a'' = attackers battlefiled - a' + max 0 attacerkRemain
        d'' = defenders battlefiled - d' + max 0 defenderRemain
      in
        return (Battlefield a'' d'')

-- battle until there is no defenders or less than two attackers
invade :: Battlefield -> Rand StdGen Battlefield
invade (Battlefield attackers defenders) =
  let
    stop = defenders == 0 || attackers < 2
  in
    if stop
      -- 装到 monad 里面返回
      then return (Battlefield attackers defenders)
      -- m a -> (a -> m b) -> m b
      else battle (Battlefield attackers defenders) >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb battlefiled =
  replicateM 1000 (invade battlefiled) >>= \stimulation ->
    let success = length $
                    filter
                      (\(Battlefield _ defenders) -> defenders == 0)
                      stimulation
      in return (fromIntegral success / 1000)
