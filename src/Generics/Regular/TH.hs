{-# LANGUAGE TemplateHaskell, CPP #-}
{-# OPTIONS_GHC -w           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.TH
-- Copyright   :  (c) 2008--2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module contains Template Haskell code that can be used to
-- automatically generate the boilerplate code for the regular
-- library.
--
-----------------------------------------------------------------------------

-- Adapted from Generics.Multirec.TH
module Generics.Regular.TH
  ( deriveAll,
    deriveConstructors,
    deriveSelectors,
    deriveRegular,
    derivePF
  ) where

import Data.List (intercalate)
import Generics.Regular.Base
import Generics.Regular.Constructor
import Language.Haskell.TH hiding (Fixity())
import Language.Haskell.TH.Syntax (Lift(..))
import Control.Monad

-- | Given the type and the name (as string) for the pattern functor to derive,
-- generate the Constructor' instances, the Selector' instances and the
-- 'Regular' instance.

deriveAll :: Name -> String -> Q [Dec]
deriveAll n s =
  do a <- deriveConstructors n
     b <- deriveSelectors n
     c <- deriveRegular n s
     return (a ++ b ++ c)

-- | Given a datatype name, derive datatypes and 
-- instances of class 'Constructor'.

deriveConstructors :: Name -> Q [Dec]
deriveConstructors = constrInstance

-- | Given a datatype name, derive datatypes and 
-- instances of class 'Selector'.

deriveSelectors :: Name -> Q [Dec]
deriveSelectors = selectInstance

-- | Given the type and the name (as string) for the
-- pattern functor to derive, generate the 'Regular'
-- instance.

deriveRegular :: Name -> String -> Q [Dec]
deriveRegular n pfn =
  do
    pf  <- derivePF pfn n
    fam <- deriveInst n
    return $ pf ++ fam

-- | Derive only the 'PF' instance. Not needed if 'deriveRegular'
-- is used.

derivePF :: String -> Name -> Q [Dec]
derivePF pfn n =
  do
    i <- reify n
    fmap (:[]) $ tySynD (mkName pfn) (typeVariables i) (pfType n)

deriveInst :: Name -> Q [Dec]
deriveInst t =
  do
    i <- reify t
    let typ = foldl (\a -> AppT a . VarT . tyVarBndrToName) (ConT t) (typeVariables i)
    fcs <- mkFrom t 1 0 t
    tcs <- mkTo   t 1 0 t
    liftM (:[]) $
      instanceD (cxt []) (conT ''Regular `appT` return typ)
        [funD 'from fcs, funD 'to tcs]

constrInstance :: Name -> Q [Dec]
constrInstance n =
  do
    i <- reify n
    case i of
      TyConI (DataD    _ n _ cs _) -> mkInstance n cs
      TyConI (NewtypeD _ n _ c  _) -> mkInstance n [c]
      _ -> return []
   where
     mkInstance n cs = do
       ds <- mapM (mkConstrData n) cs
       is <- mapM (mkConstrInstance n) cs
       return $ ds ++ is

selectInstance :: Name -> Q [Dec]
selectInstance n =
  do
    i <- reify n
    case i of
      TyConI (DataD    _ n _ cs _) -> mkInstance n cs
      TyConI (NewtypeD _ n _ c  _) -> mkInstance n [c]
      _ -> return []
  where
    mkInstance n cs = do
      ds <- mapM (mkSelectData n) cs
      is <- mapM (mkSelectInstance n) cs
      return $ concat (ds ++ is)

#ifdef TH_TYVARBNDR
typeVariables :: Info -> [TyVarBndr]
#else
typeVariables :: Info -> [Name]
#endif
typeVariables (TyConI (DataD    _ _ tv _ _)) = tv
typeVariables (TyConI (NewtypeD _ _ tv _ _)) = tv
typeVariables _                           = []

#ifdef TH_TYVARBNDR
tyVarBndrToName :: TyVarBndr -> Name
tyVarBndrToName (PlainTV  name)   = name
tyVarBndrToName (KindedTV name _) = name
#else
tyVarBndrToName :: Name -> Name
tyVarBndrToName = id
#endif

stripRecordNames :: Con -> Con
stripRecordNames (RecC n f) =
  NormalC n (map (\(_, s, t) -> (s, t)) f)
stripRecordNames c = c

genName :: [Name] -> Name
genName = mkName . (++"_") . intercalate "_" . map nameBase

mkConstrData :: Name -> Con -> Q Dec
mkConstrData dt (NormalC n _) =
  dataD (cxt []) (genName [dt, n]) [] [] [] 
mkConstrData dt r@(RecC _ _) =
  mkConstrData dt (stripRecordNames r)
mkConstrData dt (InfixC t1 n t2) =
  mkConstrData dt (NormalC n [t1,t2])

mkSelectData :: Name -> Con -> Q [Dec]
mkSelectData dt r@(RecC n fs) = return (map one fs)
  where one (f, _, _) = DataD [] (genName [dt, n, f]) [] [] []
mkSelectData dt _ = return []

instance Lift Fixity where
  lift Prefix      = conE 'Prefix
  lift (Infix a n) = conE 'Infix `appE` [| a |] `appE` [| n |]

instance Lift Associativity where
  lift LeftAssociative  = conE 'LeftAssociative
  lift RightAssociative = conE 'RightAssociative
  lift NotAssociative   = conE 'NotAssociative

mkConstrInstance :: Name -> Con -> Q Dec
mkConstrInstance dt (NormalC n _) = mkConstrInstanceWith dt n []
mkConstrInstance dt (RecC    n _) = mkConstrInstanceWith dt n
      [ funD 'conIsRecord [clause [wildP] (normalB (conE 'True)) []]]
mkConstrInstance dt (InfixC t1 n t2) =
    do
      i <- reify n
      let fi = case i of
                 DataConI _ _ _ f -> convertFixity f
                 _ -> Prefix
      instanceD (cxt []) (appT (conT ''Constructor) (conT $ genName [dt, n]))
        [funD 'conName   [clause [wildP] (normalB (stringE (nameBase n))) []],
         funD 'conFixity [clause [wildP] (normalB [| fi |]) []]]
  where
    convertFixity (Fixity n d) = Infix (convertDirection d) n
    convertDirection InfixL = LeftAssociative
    convertDirection InfixR = RightAssociative
    convertDirection InfixN = NotAssociative

mkConstrInstanceWith :: Name -> Name -> [Q Dec] -> Q Dec
mkConstrInstanceWith dt n extra = 
    instanceD (cxt []) (appT (conT ''Constructor) (conT $ genName [dt, n]))
      (funD 'conName [clause [wildP] (normalB (stringE (nameBase n))) []] : extra)

mkSelectInstance :: Name -> Con -> Q [Dec]
mkSelectInstance dt r@(RecC n fs) = return (map one fs)
  where
    one (f, _, _) = 
      InstanceD ([]) (AppT (ConT ''Selector) (ConT $ genName [dt, n, f]))
        [FunD 'selName [Clause [WildP] (NormalB (LitE (StringL (nameBase f)))) []]]
mkSelectInstance _ _ = return []

pfType :: Name -> Q Type
pfType n =
    do
      -- runIO $ putStrLn $ "processing " ++ show n
      i <- reify n
      let b = case i of
                TyConI (DataD _ dt vs cs _) ->
                  foldr1 sum (map (pfCon (dt, map tyVarBndrToName vs)) cs)
                TyConI (NewtypeD _ dt vs c _) ->
                  pfCon (dt, map tyVarBndrToName vs) c
                TyConI (TySynD t _ _) ->
                  conT ''K `appT` conT t
                _ -> error "unknown construct" 
      --appT b (conT $ mkName (nameBase n))
      b
  where
    sum :: Q Type -> Q Type -> Q Type
    sum a b = conT ''(:+:) `appT` a `appT` b


pfCon :: (Name, [Name]) -> Con -> Q Type
pfCon (dt, vs) (NormalC n []) =
    appT (appT (conT ''C) (conT $ genName [dt, n])) (conT ''U)
pfCon (dt, vs) (NormalC n fs) =
    appT (appT (conT ''C) (conT $ genName [dt, n])) (foldr1 prod (map (pfField (dt, vs) . snd) fs))
  where
    prod :: Q Type -> Q Type -> Q Type
    prod a b = conT ''(:*:) `appT` a `appT` b
pfCon (dt, vs) r@(RecC n []) =
    appT (appT (conT ''C) (conT $ genName [dt, n])) (conT ''U)
pfCon (dt, vs) r@(RecC n fs) =
    appT (appT (conT ''C) (conT $ genName [dt, n])) (foldr1 prod (map (pfField' (dt, vs) n) fs))
  where
    prod :: Q Type -> Q Type -> Q Type
    prod a b = conT ''(:*:) `appT` a `appT` b

pfCon d (InfixC t1 n t2) =
    pfCon d (NormalC n [t1,t2])

dataDeclToType :: (Name, [Name]) -> Type
dataDeclToType (dt, vs) = foldl (\a b -> AppT a (VarT b)) (ConT dt) vs

pfField :: (Name, [Name]) -> Type -> Q Type
pfField d t | t == dataDeclToType d = conT ''I
pfField d t                         = conT ''K `appT` return t

pfField' :: (Name, [Name]) -> Name -> (Name, Strict, Type) -> Q Type
pfField' d ns (_, _, t) | t == dataDeclToType d = conT ''I
pfField' (dt, vs) ns (f, _, t)                  = conT ''S `appT` conT (genName [dt, ns, f]) `appT` (conT ''K `appT` return t)

mkFrom :: Name -> Int -> Int -> Name -> Q [Q Clause]
mkFrom ns m i n =
    do
      -- runIO $ putStrLn $ "processing " ++ show n
      let wrapE e = lrE m i e
      i <- reify n
      let b = case i of
                TyConI (DataD _ dt vs cs _) ->
                  zipWith (fromCon wrapE ns (dt, map tyVarBndrToName vs) (length cs)) [0..] cs
                TyConI (NewtypeD _ dt vs c _) ->
                  [fromCon wrapE ns (dt, map tyVarBndrToName vs) 1 0 c]
                TyConI (TySynD t _ _) ->
                  [clause [varP (field 0)] (normalB (wrapE $ conE 'K `appE` varE (field 0))) []]
                _ -> error "unknown construct"
      return b

mkTo :: Name -> Int -> Int -> Name -> Q [Q Clause]
mkTo ns m i n =
    do
      -- runIO $ putStrLn $ "processing " ++ show n
      let wrapP p = lrP m i p
      i <- reify n
      let b = case i of
                TyConI (DataD _ dt vs cs _) ->
                  zipWith (toCon wrapP ns (dt, map tyVarBndrToName vs) (length cs)) [0..] cs
                TyConI (NewtypeD _ dt vs c _) ->
                  [toCon wrapP ns (dt, map tyVarBndrToName vs) 1 0 c]
                TyConI (TySynD t _ _) ->
                  [clause [wrapP $ conP 'K [varP (field 0)]] (normalB $ varE (field 0)) []]
                _ -> error "unknown construct" 
      return b

fromCon :: (Q Exp -> Q Exp) -> Name -> (Name, [Name]) -> Int -> Int -> Con -> Q Clause
fromCon wrap ns (dt, vs) m i (NormalC cn []) =
    clause
      [conP cn []]
      (normalB $ wrap $ lrE m i $ conE 'C `appE` (conE 'U)) []
fromCon wrap ns (dt, vs) m i (NormalC cn fs) =
    -- runIO (putStrLn ("constructor " ++ show ix)) >>
    clause
      [conP cn (map (varP . field) [0..length fs - 1])]
      (normalB $ wrap $ lrE m i $ conE 'C `appE` foldr1 prod (zipWith (fromField (dt, vs)) [0..] (map snd fs))) []
  where
    prod x y = conE '(:*:) `appE` x `appE` y
fromCon wrap ns (dt, vs) m i r@(RecC cn []) =
    clause
      [conP cn []]
      (normalB $ wrap $ lrE m i $ conE 'C `appE` (conE 'U)) []
fromCon wrap ns (dt, vs) m i r@(RecC cn fs) =
    clause
      [conP cn (map (varP . field) [0..length fs - 1])]
      (normalB $ wrap $ lrE m i $ conE 'C `appE` foldr1 prod (zipWith (fromField' (dt, vs)) [0..] fs)) []
  where
    prod x y = conE '(:*:) `appE` x `appE` y
fromCon wrap ns (dt, vs) m i (InfixC t1 cn t2) =
  fromCon wrap ns (dt, vs) m i (NormalC cn [t1,t2])

fromField :: (Name, [Name]) -> Int -> Type -> Q Exp
fromField (dt, vs) nr t | t == dataDeclToType (dt, vs) = conE 'I `appE` varE (field nr)
fromField (dt, vs) nr t                                = conE 'K `appE` varE (field nr)

fromField' :: (Name, [Name]) -> Int -> (Name, Strict, Type) -> Q Exp
fromField' (dt, vs) nr (_, _, t) | t == dataDeclToType (dt, vs) = conE 'I `appE` varE (field nr)
fromField' (dt, vs) nr (_, _, t)                                = conE 'S `appE` (conE 'K `appE` varE (field nr))

toCon :: (Q Pat -> Q Pat) -> Name -> (Name, [Name]) -> Int -> Int -> Con -> Q Clause
toCon wrap ns (dt, vs) m i (NormalC cn []) =
    clause
      [wrap $ lrP m i $ conP 'C [conP 'U []]]
      (normalB $ conE cn) []
toCon wrap ns (dt, vs) m i (NormalC cn fs) =
    -- runIO (putStrLn ("constructor " ++ show ix)) >>
    clause
      [wrap $ lrP m i $ conP 'C [foldr1 prod (zipWith (toField (dt, vs)) [0..] (map snd fs))]]
      (normalB $ foldl appE (conE cn) (map (varE . field) [0..length fs - 1])) []
  where
    prod x y = conP '(:*:) [x,y]
toCon wrap ns (dt, vs) m i r@(RecC cn []) =
    clause
      [wrap $ lrP m i $ conP 'C [conP 'U []]]
      (normalB $ conE cn) []
toCon wrap ns (dt, vs) m i r@(RecC cn fs) =
    clause
      [wrap $ lrP m i $ conP 'C [foldr1 prod (zipWith (toField' (dt, vs)) [0..] fs)]]
      (normalB $ foldl appE (conE cn) (map (varE . field) [0..length fs - 1])) []
  where
    prod x y = conP '(:*:) [x,y]
toCon wrap ns (dt, vs) m i (InfixC t1 cn t2) =
  toCon wrap ns (dt, vs) m i (NormalC cn [t1,t2])

toField :: (Name, [Name]) -> Int -> Type -> Q Pat
toField (dt, vs) nr t | t == dataDeclToType (dt, vs) = conP 'I [varP (field nr)]
toField (dt, vs) nr t                                = conP 'K [varP (field nr)]

toField' :: (Name, [Name]) -> Int -> (Name, Strict, Type) -> Q Pat
toField' (dt, vs) nr (_, _, t) | t == dataDeclToType (dt, vs) = conP 'I [varP (field nr)]
toField' (dt, vs) nr (_, _, t)                                = conP 'S [conP 'K [varP (field nr)]]

field :: Int -> Name
field n = mkName $ "f" ++ show n

lrP :: Int -> Int -> (Q Pat -> Q Pat)
lrP 1 0 p = p
lrP m 0 p = conP 'L [p]
lrP m i p = conP 'R [lrP (m-1) (i-1) p]

lrE :: Int -> Int -> (Q Exp -> Q Exp)
lrE 1 0 e = e
lrE m 0 e = conE 'L `appE` e
lrE m i e = conE 'R `appE` lrE (m-1) (i-1) e

