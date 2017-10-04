-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Provers.VeriT
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- The connection to the veriT SMT solver
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Data.SBV.Provers.VeriT(veriT) where

import Data.SBV.Core.Data
import Data.SBV.SMT.SMT

-- | The description of the veriT SMT solver.
-- The default executable is @\"veriT\"@, which must be in your path. You can use the @SBV_VERIT@ environment variable to point to the executable on your system.
-- The default options are @\"--disable-banner\"@. You can use the @SBV_VERIT_OPTIONS@ environment variable to override the options.
veriT :: SMTSolver
veriT = SMTSolver {
           name         = VeriT
         , executable   = "veriT"
         , options      = modConfig ["--disable-banner"]
         , engine       = standardEngine "SBV_VERIT" "SBV_VERIT_OPTIONS"
         , capabilities = SolverCapabilities {
                                supportsQuantifiers        = True
                              , supportsUninterpretedSorts = True
                              , supportsUnboundedInts      = True
                              , supportsReals              = True
                              , supportsApproxReals        = True
                              , supportsIEEE754            = True
                              , supportsOptimization       = True
                              , supportsPseudoBooleans     = True
                              , supportsCustomQueries      = True
                              , supportsGlobalDecls        = True
                              }
         }

 where modConfig :: [String] -> SMTConfig -> [String]
       modConfig opts _cfg = opts
