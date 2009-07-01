{-# LANGUAGE TemplateHaskell #-}
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
  ( deriveConstructors,
    deriveRegular,
    derivePF
  ) where

import Generics.Regular.Base
import Generics.Regular.Constructor
import Language.Haskell.TH hiding (Fixity())
import Language.Haskell.TH.Syntax (Lift(..))
import Control.Monad

-- | Given a datatype name, derive datatypes and 
-- instances of class 'Constructor'.

deriveConstructors :: Name -> Q [Dec]
deriveConstructors = constrInstance

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
    fmap (:[]) $
    tySynD (mkName pfn) [] (pfType n)

deriveInst :: Name -> Q [Dec]
deriveInst t =
  do
    fcs <- mkFrom t 1 0 t
    tcs <- mkTo   t 1 0 t
    liftM (:[]) $
      instanceD (cxt []) (conT ''Regular `appT` conT t)
        [funD 'from fcs, funD 'to tcs]

constrInstance :: Name -> Q [Dec]
constrInstance n =
  do
    i <- reify n
    -- runIO (print i)
    let cs = case i of
               TyConI (DataD _ _ _ cs _) -> cs
               _ -> []
    ds <- mapM mkData cs
    is <- mapM mkInstance cs
    return $ ds ++ is

stripRecordNames :: Con -> Con
stripRecordNames (RecC n f) =
  NormalC n (map (\(_, s, t) -> (s, t)) f)
stripRecordNames c = c

mkData :: Con -> Q Dec
mkData (NormalC n _) =
  dataD (cxt []) (mkName (nameBase n)) [] [] [] 
mkData r@(RecC _ _) =
  mkData (stripRecordNames r)
mkData (InfixC t1 n t2) =
  mkData (NormalC n [t1,t2])

instance Lift Fixity where
  lift Prefix      = conE 'Prefix
  lift (Infix a n) = conE 'Infix `appE` [| a |] `appE` [| n |]

instance Lift Associativity where
  lift LeftAssociative  = conE 'LeftAssociative
  lift RightAssociative = conE 'RightAssociative
  lift NotAssociative   = conE 'NotAssociative

mkInstance :: Con -> Q Dec
mkInstance (NormalC n _) =
    instanceD (cxt []) (appT (conT ''Constructor) (conT $ mkName (nameBase n)))
      [funD 'conName [clause [wildP] (normalB (stringE (nameBase n))) []]]
mkInstance r@(RecC _ _) =
  mkInstance (stripRecordNames r)
mkInstance (InfixC t1 n t2) =
    do
      i <- reify n
      let fi = case i of
                 DataConI _ _ _ f -> convertFixity f
                 _ -> Prefix
      instanceD (cxt []) (appT (conT ''Constructor) (conT $ mkName (nameBase n)))
        [funD 'conName   [clause [wildP] (normalB (stringE ("(" ++ (nameBase n) ++ ")"))) []],
         funD 'conFixity [clause [wildP] (normalB [| fi |]) []]]
  where
    convertFixity (Fixity n d) = Infix (convertDirection d) n
    convertDirection InfixL = LeftAssociative
    convertDirection InfixR = RightAssociative
    convertDirection InfixN = NotAssociative

pfType :: Name -> Q Type
pfType n =
    do
      -- runIO $ putStrLn $ "processing " ++ show n
      i <- reify n
      let b = case i of
                TyConI (DataD _ _ _ cs _) ->
                  foldr1 sum (map (pfCon n) cs)
                TyConI (TySynD t _ _) ->
                  conT ''K `appT` conT t
                _ -> error "unknown construct" 
      --appT b (conT $ mkName (nameBase n))
      b
  where
    sum :: Q Type -> Q Type -> Q Type
    sum a b = conT ''(:+:) `appT` a `appT` b

pfCon :: Name -> Con -> Q Type
pfCon ns (NormalC n []) =
    appT (appT (conT ''C) (conT $ mkName (nameBase n))) (conT ''U)
pfCon ns (NormalC n fs) =
    appT (appT (conT ''C) (conT $ mkName (nameBase n))) (foldr1 prod (map (pfField ns . snd) fs))
  where
    prod :: Q Type -> Q Type -> Q Type
    prod a b = conT ''(:*:) `appT` a `appT` b
pfCon ns r@(RecC _ _) =
  pfCon ns (stripRecordNames r)
pfCon ns (InfixC t1 n t2) =
    pfCon ns (NormalC n [t1,t2])

pfField :: Name -> Type -> Q Type
pfField ns t@(ConT n) | n == ns = conT ''I
pfField ns t                    = conT ''K `appT` return t

mkFrom :: Name -> Int -> Int -> Name -> Q [Q Clause]
mkFrom ns m i n =
    do
      -- runIO $ putStrLn $ "processing " ++ show n
      let wrapE e = lrE m i e
      i <- reify n
      let dn = mkName (nameBase n)
      let b = case i of
                TyConI (DataD _ _ _ cs _) ->
                  zipWith (fromCon wrapE ns dn (length cs)) [0..] cs
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
      let dn = mkName (nameBase n)
      let b = case i of
                TyConI (DataD _ _ _ cs _) ->
                  zipWith (toCon wrapP ns dn (length cs)) [0..] cs
                TyConI (TySynD t _ _) ->
                  [clause [wrapP $ conP 'K [varP (field 0)]] (normalB $ varE (field 0)) []]
                _ -> error "unknown construct" 
      return b

fromCon :: (Q Exp -> Q Exp) -> Name -> Name -> Int -> Int -> Con -> Q Clause
fromCon wrap ns n m i (NormalC cn []) =
    clause
      [conP cn []]
      (normalB $ wrap $ lrE m i $ conE 'C `appE` (conE 'U)) []
fromCon wrap ns n m i (NormalC cn fs) =
    -- runIO (putStrLn ("constructor " ++ show ix)) >>
    clause
      [conP cn (map (varP . field) [0..length fs - 1])]
      (normalB $ wrap $ lrE m i $ conE 'C `appE` foldr1 prod (zipWith (fromField ns) [0..] (map snd fs))) []
  where
    prod x y = conE '(:*:) `appE` x `appE` y
fromCon wrap ns n m i r@(RecC _ _) =
  fromCon wrap ns n m i (stripRecordNames r)
fromCon wrap ns n m i (InfixC t1 cn t2) =
  fromCon wrap ns n m i (NormalC cn [t1,t2])

toCon :: (Q Pat -> Q Pat) -> Name -> Name -> Int -> Int -> Con -> Q Clause
toCon wrap ns n m i (NormalC cn []) =
    clause
      [wrap $ lrP m i $ conP 'C [conP 'U []]]
      (normalB $ conE cn) []
toCon wrap ns n m i (NormalC cn fs) =
    -- runIO (putStrLn ("constructor " ++ show ix)) >>
    clause
      [wrap $ lrP m i $ conP 'C [foldr1 prod (zipWith (toField ns) [0..] (map snd fs))]]
      (normalB $ foldl appE (conE cn) (map (varE . field) [0..length fs - 1])) []
  where
    prod x y = conP '(:*:) [x,y]
toCon wrap ns n m i r@(RecC _ _) =
  toCon wrap ns n m i (stripRecordNames r)
toCon wrap ns n m i (InfixC t1 cn t2) =
  toCon wrap ns n m i (NormalC cn [t1,t2])

fromField :: Name -> Int -> Type -> Q Exp
fromField ns nr t@(ConT n) | n == ns = conE 'I `appE` varE (field nr)
fromField ns nr t                    = conE 'K `appE` varE (field nr)

toField :: Name -> Int -> Type -> Q Pat
toField ns nr t@(ConT n) | n == ns = conP 'I [varP (field nr)]
toField ns nr t                    = conP 'K [varP (field nr)]

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

