-----------------------------------------------------------------------------
-- |
-- Module    : Data.SBV.Client
-- Copyright : (c) Levent Erkok
-- License   : BSD3
-- Maintainer: erkokl@gmail.com
-- Stability : experimental
--
-- Cross-cutting toplevel client functions
-----------------------------------------------------------------------------

{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Data.SBV.Client
  ( sbvCheckSolverInstallation
  , defaultSolverConfig
  , getAvailableSolvers
  , mkSymbolicEnumeration
  , mkSymbolicDatatype
  , mkUninterpretedSort
  ) where

import Control.Monad (filterM)
import Data.Generics

import qualified Control.Exception   as C

import qualified "template-haskell" Language.Haskell.TH as TH

import Data.SBV.Core.Data hiding (name)
import Data.SBV.Core.Model
import Data.SBV.Provers.Prover hiding (name, sName)

import Data.SBV.Tuple  as ST
import Data.SBV.Either as SE
import Data.List   (foldl', foldl1')

-- | Check whether the given solver is installed and is ready to go. This call does a
-- simple call to the solver to ensure all is well.
sbvCheckSolverInstallation :: SMTConfig -> IO Bool
sbvCheckSolverInstallation cfg = check `C.catch` (\(_ :: C.SomeException) -> return False)
  where check = do ThmResult r <- proveWith cfg $ \x -> sNot (sNot x) .== (x :: SBool)
                   case r of
                     Unsatisfiable{} -> return True
                     _               -> return False

-- | The default configs corresponding to supported SMT solvers
defaultSolverConfig :: Solver -> SMTConfig
defaultSolverConfig Z3        = z3
defaultSolverConfig Yices     = yices
defaultSolverConfig DReal     = dReal
defaultSolverConfig Boolector = boolector
defaultSolverConfig CVC4      = cvc4
defaultSolverConfig MathSAT   = mathSAT
defaultSolverConfig ABC       = abc

-- | Return the known available solver configs, installed on your machine.
getAvailableSolvers :: IO [SMTConfig]
getAvailableSolvers = filterM sbvCheckSolverInstallation (map defaultSolverConfig [minBound .. maxBound])

-- | Turn a name into a symbolic type. If first argument is true, we'll also derive Eq and Ord instances.
declareSymbolic :: Bool -> TH.Name -> TH.Q [TH.Dec]
declareSymbolic isEnum typeName = do
    let typeCon = TH.conT typeName

    cstrs <- if isEnum then ensureEnumeration typeName
                       else ensureEmptyData   typeName

    deriveEqOrds <- if isEnum
                       then [d| deriving instance Eq       $(typeCon)
                                deriving instance Ord      $(typeCon)
                            |]
                       else pure []

    derives <- [d| deriving instance Show     $(typeCon)
                   deriving instance Read     $(typeCon)
                   deriving instance Data     $(typeCon)
                   deriving instance SymVal   $(typeCon)
                   deriving instance HasKind  $(typeCon)
                   deriving instance SatModel $(typeCon)
               |]


    sType <- TH.conT ''SBV `TH.appT` typeCon

    let declConstructor c = [sig, def]
          where nm   = TH.mkName $ 's' : TH.nameBase c
                def  = TH.FunD nm [TH.Clause [] (TH.NormalB body) []]
                body = TH.AppE (TH.VarE 'literal) (TH.ConE c)
                sig  = TH.SigD nm sType

        cdecl = concatMap declConstructor cstrs

    let tdecl = TH.TySynD (TH.mkName ('S' : TH.nameBase typeName)) [] sType
    pure $ deriveEqOrds ++ derives ++ [tdecl] ++ cdecl

-- | Make an enumeration a symbolic type.
mkSymbolicEnumeration :: TH.Name -> TH.Q [TH.Dec]
mkSymbolicEnumeration = declareSymbolic True

-- | Make an uninterpred sort.
mkUninterpretedSort :: TH.Name -> TH.Q [TH.Dec]
mkUninterpretedSort = declareSymbolic False

-- | Make sure the given type is an enumeration
ensureEnumeration :: TH.Name -> TH.Q [TH.Name]
ensureEnumeration nm = do
        c <- TH.reify nm
        case c of
          TH.TyConI d -> case d of
                           TH.DataD _ _ _ _ cons _ -> case cons of
                                                        [] -> bad "The datatype given has no constructors."
                                                        xs -> concat <$> mapM check xs
                           _                       -> bad "The name given is not a datatype."

          _        -> bad "The name given is not a datatype."
 where n = TH.nameBase nm

       check (TH.NormalC c xs) = case xs of
                                   [] -> pure [c]
                                   _  -> bad $ "Constructor " ++ show c ++ " has arguments."

       check c                 = bad $ "Constructor " ++ show c ++ " is not an enumeration value."

       bad m = do TH.reportError $ unlines [ "Data.SBV.mkSymbolicEnumeration: Invalid argument " ++ show n
                                           , ""
                                           , "    Expected an enumeration. " ++ m
                                           , ""
                                           , "    To create an enumerated sort, use a simple Haskell enumerated type."
                                           ]
                  pure []

-- | Make sure the given type is an empty data
ensureEmptyData :: TH.Name -> TH.Q [TH.Name]
ensureEmptyData nm = do
        c <- TH.reify nm
        case c of
          TH.TyConI d -> case d of
                           TH.DataD _ _ _ _ cons _ -> case cons of
                                                        [] -> pure []
                                                        _  -> bad "The datatype given has constructors."
                           _                       -> bad "The name given is not a datatype."

          _        -> bad "The name given is not a datatype."
 where n = TH.nameBase nm
       bad m = do TH.reportError $ unlines [ "Data.SBV.mkUninterpretedSort: Invalid argument " ++ show n
                                           , ""
                                           , "    Expected an empty datatype. " ++ m
                                           , ""
                                           , "    To create an uninterpreted sort, use an empty datatype declaration."
                                           ]
                  pure []

mkSymbolicDatatype :: TH.Name -> TH.Q [TH.Dec]
mkSymbolicDatatype topType =
  do TH.TyConI (TH.DataD _ _ tvs _ clauses _) <- TH.reify topType
     let typeBaseName = TH.nameBase topType
     let sName = TH.mkName ('S':typeBaseName)
     let nName = TH.mkName ('N':typeBaseName)
     let ssName = TH.mkName ('S':'S':typeBaseName)
     nestedDecl <- mkNestedDec tvs nName clauses
     symDecl <- mkSymDec tvs sName nName
     subSymDecl <- mkSubSymDec tvs ssName clauses
     nestFunc <- mkNestFunc tvs typeBaseName topType nName clauses
     unNestFunc <- mkUnNestFunc tvs typeBaseName topType nName clauses
     symCaseFunc <- mkSymCase tvs typeBaseName sName ssName clauses
     symConsts <- mkSymConsts tvs sName clauses id
     return (nestedDecl:symDecl:subSymDecl:
             (nestFunc ++ unNestFunc ++ symCaseFunc ++ symConsts))

 where nestClauses :: [TH.Con] -> TH.TypeQ
       nestClauses [] = error "No constructors for type"
       nestClauses [TH.NormalC _ [(_, t)]] =
         return t
       nestClauses [TH.NormalC _ params] =
         foldl' (TH.appT) (TH.tupleT (length params)) [return t | (_, t) <- params]
       nestClauses list = TH.appT (TH.appT [t| Either |]
                                     (nestClauses fHalf))
                               (nestClauses sHalf)
         where ll = length list
               (fHalf, sHalf) = splitAt (ll - (length list `div` 2)) list

       mkNestedDec :: [TH.TyVarBndr] -> TH.Name -> [TH.Con] -> TH.Q TH.Dec
       mkNestedDec tvs nName clauses =
         TH.tySynD nName tvs $ nestClauses clauses

       getTVName :: TH.TyVarBndr -> TH.Name
       getTVName (TH.PlainTV name) = name
       getTVName (TH.KindedTV name _) = name

       addTVs :: [TH.TyVarBndr] -> TH.TypeQ -> TH.TypeQ
       addTVs tvs con = foldl' TH.appT con $ map (TH.varT . getTVName) tvs

       mkSymDec :: [TH.TyVarBndr] -> TH.Name -> TH.Name -> TH.Q TH.Dec
       mkSymDec tvs sName nName =
         TH.tySynD sName tvs [t| SBV $(foldl' TH.appT (TH.conT nName) $ map (TH.varT . getTVName) tvs) |]

       mkSubSymDec :: [TH.TyVarBndr] -> TH.Name -> [TH.Con] -> TH.Q TH.Dec
       mkSubSymDec tvs ssName clauses =
         TH.dataD (return [])
               ssName
               tvs Nothing [ TH.normalC (let typeBaseName = TH.nameBase name in
                                      TH.mkName ('S':'S':typeBaseName))
                                     [ TH.bangType (return pb)
                                                [t| SBV $(return param) |]
                                      | (pb, param) <- params ]
                            | TH.NormalC name params <- clauses ]
               []

       nestFunClauses :: [TH.Con] -> (TH.ExpQ -> TH.ExpQ) -> [TH.ClauseQ]
       nestFunClauses [] _ = error "No constructors for type"
       nestFunClauses [TH.NormalC name params] f =
         [do names <- mapM (\x -> TH.newName ('p':(show x))) [1..(length params)]
             TH.clause [ TH.conP name [ TH.varP n | n <- names ] ]
                    (TH.normalB (f (tupE' (map TH.varE names)))) []]
       nestFunClauses list f = (nestFunClauses fHalf (f . (\x -> [e| Left $(x) |]))) ++
                               (nestFunClauses sHalf (f . (\x -> [e| Right $(x) |])))
         where ll = length list
               (fHalf, sHalf) = splitAt (ll - (length list `div` 2)) list

       addSymValToTVs :: [TH.TyVarBndr] -> TH.TypeQ -> TH.TypeQ
       addSymValToTVs [] ty = ty
       addSymValToTVs tvs ty =
         do x <- [t| SymVal |]
            aty <- ty
            return $ TH.ForallT [] [TH.AppT x (TH.VarT $ getTVName tv) | tv <- tvs] aty

       mkNestFunc :: [TH.TyVarBndr] -> String -> TH.Name -> TH.Name -> [TH.Con] -> TH.Q [TH.Dec]
       mkNestFunc tvs typeBaseName typeName typeNName clauses =
         do let nestFunName = ("nest" ++ typeBaseName)
            let nfName = TH.mkName nestFunName
            signature <- TH.sigD nfName (addSymValToTVs tvs
                                       (TH.appT (TH.appT TH.arrowT (addTVs tvs $ TH.conT typeName))
                                                   (addTVs tvs $ TH.conT typeNName)))
            declaration <- TH.funD nfName $ nestFunClauses clauses id
            return [signature, declaration]

       unNestFunClauses :: [TH.Con] -> (TH.PatQ -> TH.PatQ) -> [TH.ClauseQ]
       unNestFunClauses [] _ = error "No constructors for type"
       unNestFunClauses [TH.NormalC name params] f =
         [do names <- mapM (\x -> TH.newName ('p':(show x))) [1..(length params)]
             TH.clause [f (tupP' (map TH.varP names))]
                    (TH.normalB (foldl' TH.appE (TH.conE name) [ TH.varE n | n <- names ])) []]
       unNestFunClauses list f = (unNestFunClauses fHalf (f . (\x -> [p| Left $(x) |]))) ++
                                 (unNestFunClauses sHalf (f . (\x -> [p| Right $(x) |])))
         where ll = length list
               (fHalf, sHalf) = splitAt (ll - (length list `div` 2)) list

       mkUnNestFunc :: [TH.TyVarBndr] -> String -> TH.Name -> TH.Name -> [TH.Con] -> TH.Q [TH.Dec]
       mkUnNestFunc tvs typeBaseName typeName typeNName clauses =
         do let unNestFunName = ("unNest" ++ typeBaseName)
            let unfName = TH.mkName unNestFunName
            signature <- TH.sigD unfName (TH.appT (TH.appT TH.arrowT (addTVs tvs $ TH.conT typeNName))
                                                  (addTVs tvs $ TH.conT typeName))
            declaration <- TH.funD unfName $ unNestFunClauses clauses id
            return [signature, declaration]

       mkSymCaseRec :: TH.Name -> [TH.Con] -> TH.ExpQ
       mkSymCaseRec _ [] = error "No constructors for type"
       mkSymCaseRec func [TH.NormalC name params] =
         do paramNames <- mapM (\x -> TH.newName ('p':(show x))) [1..(length params)]
            let ssName = TH.mkName ('S':'S':(TH.nameBase name))
            let wrapping = [e| (\ $(tupP' $ map TH.varP paramNames) ->
                               $(TH.varE func)
                               ($(foldl' (TH.appE) (TH.conE ssName) $ map (TH.varE) paramNames))) |]
            if length params > 1
            then [e| $(wrapping) . ST.untuple |]
            else if (length params > 0)
                 then wrapping
                 else [e| $(wrapping) . (const ()) |]
       mkSymCaseRec func list = [e| SE.either $(mkSymCaseRec func fHalf)
                                              $(mkSymCaseRec func sHalf) |]
         where ll = length list
               (fHalf, sHalf) = splitAt (ll - (length list `div` 2)) list

       mkSymCase :: [TH.TyVarBndr] -> String -> TH.Name -> TH.Name -> [TH.Con] -> TH.Q [TH.Dec]
       mkSymCase tvs typeBaseName sName ssName clauses =
         do let symCaseFunName = ("symCase" ++ typeBaseName)
            let scName = TH.mkName symCaseFunName
            typeVar <- TH.newName "a"
            func <- TH.newName "f"
            signature <- TH.sigD scName $ addSymValToTVs tvs $
                                       [t| SymVal $(TH.varT typeVar) =>
                                           ($(addTVs tvs $ TH.conT ssName)
                                            -> SBV $(TH.varT typeVar))
                                        -> $(addTVs tvs $ TH.conT sName)
                                        -> SBV $(TH.varT typeVar) |]
            declaration <- TH.funD scName $
                           [TH.clause [TH.varP func]
                                   (TH.normalB (mkSymCaseRec func clauses)) []]
            return [signature, declaration]

       mkSymConsts :: [TH.TyVarBndr] -> TH.Name -> [TH.Con] -> (TH.ExpQ -> TH.ExpQ) -> TH.Q [TH.Dec]
       mkSymConsts _ _ [] _ = error "No constructors for type"
       mkSymConsts tvs sName [TH.NormalC name params] f =
         do let nestFunName = ("s" ++ (TH.nameBase name))
            let nfName = TH.mkName nestFunName
            signature <- TH.sigD nfName $ addSymValToTVs tvs
                                       (foldl1' (\x y -> TH.appT (TH.appT TH.arrowT y) x)
                                         (addTVs tvs(TH.conT sName):
                                          (reverse [ [t| SBV $(return param) |]
                                                    | (_, param) <- params ])))
            declaration <- TH.funD nfName $
              [do names <- mapM (\x -> TH.newName ('p':(show x))) [1..(length params)]
                  TH.clause [ TH.varP n | n <- names ]
                         (TH.normalB (f (if length params > 1
                                      then [e| ST.tuple $(tupE' (map TH.varE names)) |]
                                      else if length params > 0
                                           then tupE' (map TH.varE names)
                                           else [e| literal () |]))) []]
            return [signature, declaration]
       mkSymConsts tvs sName list f =
         do rfHalf <- (mkSymConsts tvs sName fHalf (f . (\x -> [e| sLeft $(x) |])))
            rsHalf <- (mkSymConsts tvs sName sHalf (f . (\x -> [e| sRight $(x) |])))
            return (rfHalf ++ rsHalf)
         where ll = length list
               (fHalf, sHalf) = splitAt (ll - (length list `div` 2)) list

       -- | Backwards-compatible handling of unary tuple expressions.
       --
       -- From GHC-8.10.1, unary tuples do not map to the plain expression, but
       -- apply the 'Unit' type. To keep compatibility between several TH versions,
       -- we restore the old behaviour here.
       --
       -- See https://gitlab.haskell.org/ghc/ghc/-/issues/16881
       tupE' :: [TH.ExpQ] -> TH.ExpQ
       tupE' [e] = e
       tupE' es  = TH.tupE es

       -- | Backwards-compatible handling of unary tuple patterns.
       --
       -- From GHC-8.10.1, unary tuples do not map to the plain pattern, but
       -- apply the 'Unit' type. To keep compatibility between several TH versions,
       -- we restore the old behaviour here.
       --
       -- See https://gitlab.haskell.org/ghc/ghc/-/issues/16881
       tupP' :: [TH.PatQ] -> TH.PatQ
       tupP' [p] = p
       tupP' ps  = TH.tupP ps
