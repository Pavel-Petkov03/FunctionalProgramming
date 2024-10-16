-- {{{ Pragmas

-- NOTE: read up more here - https://wiki.haskell.org/Language_Pragmas

-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Graphics.Win32 (equalRgn)
-- disable some hints that spoil the easy tasks
{-# HLINT ignore "Use even" #-}

-- }}}


fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)



data RPS = Rock | Paper | Scissors
  deriving (Show)
beats :: RPS -> RPS -> Bool
beats = undefined


data Point = MyPoint Integer Integer
    deriving (Show)

defaultPoint :: Point
defaultPoint = MyPoint 0 0


next :: RPS -> RPS
next Rock = Paper
next Paper = Scissors
next Scissors = Rock

describeTuple :: (RPS, RPS) -> Bool
describeTuple pair = case pair of
    (Scissors, Scissors) -> True
    (Rock , Rock) -> True
    (Paper , Paper) -> True
    _ -> False



 




