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
-- disable some hints that spoil the easy tasks
{-# HLINT ignore "Use even" #-}

-- }}}


fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)
fwefewfew

-- >>> fact 5