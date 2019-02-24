-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.Basics.Set
-- Copyright : (c) Levent Erkok
-- License   : BSD3
-- Maintainer: erkokl@gmail.com
-- Stability : experimental
--
-- Test sets.
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

module TestSuite.Basics.Set (tests)  where

import Data.SBV hiding (complement)
import Data.SBV.Set

import Data.SBV.RegExp (RegExpMatchable, match, identifier)

import Data.SBV.Control

import Utils.SBVTestFramework hiding (complement)

data E = A | B | C
mkSymbolicEnumeration ''E

c :: (SymVal a, Ord a) => SSet a -> SSet a
c = complement

u, i :: (SymVal a, Ord a) => SSet a -> SSet a -> SSet a
u = union
i = intersection

isIdentifier :: RegExpMatchable a => a -> SBool
isIdentifier = (`match` identifier)

sIdentifier :: String -> Symbolic SChar
sIdentifier nm = do n <- sChar nm
                    constrain $ isIdentifier n
                    return n

-- Test suite
tests :: TestTree
tests = testGroup "Basics.Set" [
          goldenCapturedIO "set_uninterp1"  $ ta setE1
        , goldenCapturedIO "set_uninterp2"  $ tq setE2
        , goldenCapturedIO "set_union1"     $ tq setU1
        , goldenCapturedIO "set_intersect1" $ tq setI1
        ]
    where ta tc goldFile    = record goldFile =<< tc defaultSMTCfg{verbose=True, redirectVerbose=Just goldFile}
          tq tc goldFile    = record goldFile =<< runSMTWith defaultSMTCfg{verbose=True, redirectVerbose=Just goldFile} tc
          record goldFile r = appendFile goldFile ("\nFINAL:\n" ++ show r ++ "\nDONE!\n")

setE1 :: SMTConfig -> IO AllSatResult
setE1 cfg = allSatWith cfg $ \(_ :: SSet E) -> sTrue

setE2 :: Symbolic (RCSet E, RCSet E)
setE2 = do a :: SSet E <- sSet "a"
           b :: SSet E <- sSet "b"

           constrain $ distinct [a, b]

           query $ do ensureSat
                      (,) <$> getValue a <*> getValue b

setU1 :: Symbolic (Char, Char, RCSet Char, RCSet Char, RCSet Char, RCSet Char)
setU1 = do a <- sIdentifier "a"
           b <- sIdentifier "b"

           -- I'd love to have these distinct, but
           -- currently z3 is spitting out too complicated a
           -- model for us to parse back if I have this
           -- constraint. See: https://github.com/Z3Prover/z3/issues/2153
           -- If they ever fix it, add this constraint back.
           -- constrain $ distinct [a, b]

           let sa = singleton a
               sb = singleton b

               o1 =   sa `u`   sb
               o2 =   sa `u` c sb
               o3 = c sa `u`   sb
               o4 = c sa `u` c sb

           query $ do ensureSat
                      (,,,,,) <$> getValue a <*> getValue b <*> getValue o1 <*> getValue o2 <*> getValue o3 <*> getValue o4

setI1 :: Symbolic (Char, Char, RCSet Char, RCSet Char, RCSet Char, RCSet Char)
setI1 = do a <- sIdentifier "a"
           b <- sIdentifier "b"

           -- See relevant note in setU1 for the following constraint:
           -- constrain $ distinct [a, b]

           let sa = singleton a
               sb = singleton b

               o1 =   sa `i`   sb
               o2 =   sa `i` c sb
               o3 = c sa `i`   sb
               o4 = c sa `i` c sb

           query $ do ensureSat
                      (,,,,,) <$> getValue a <*> getValue b <*> getValue o1 <*> getValue o2 <*> getValue o3 <*> getValue o4
