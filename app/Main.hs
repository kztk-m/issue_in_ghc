{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Main where
import           Data.Kind (Type)

data Env (f :: k -> Type) (as :: [k]) where
  ENil  :: Env f '[]
  ECons :: f a -> Env f as -> Env f (a ': as)

data Sig2 k = [k] :~> k

data DimSimple (s :: Sig2 k) where
  DimSimple :: OfLength as -> DimSimple (as ':~> a)

data OfLength as where
  LZ :: OfLength '[]
  LS :: OfLength as -> OfLength (a ': as)

class LiftOfLength f as t | t -> as where
  liftOfLength :: OfLength as -> f t

instance t ~ (as ':~> a) => LiftOfLength DimSimple as t where
  liftOfLength = undefined

data EnvI (sem :: [k] -> k -> Type) (a :: k) -- = EnvI { runEnvI :: forall as. Env Proxy as -> sem as a }

type family Func sem as r where
  Func sem '[] r       = r
  Func sem (a ': as) r = sem a -> Func sem as r

type family FuncU (sem :: [k] -> k -> Type) (ss :: [Sig2 k])
                  (r :: k) = res | res -> sem r where
  FuncU sem '[] r = EnvI sem r
  FuncU sem ((as ':~> a) ': ss) r = Func (EnvI sem) as (EnvI sem a)
                                    -> FuncU sem ss r

liftSOn :: forall sem ss r. Env DimSimple ss -> FuncU sem ss r
liftSOn _ = undefined

-- The following code causes non termination of type checking in GHC 9.2, 9.8, and 9.10.
f :: (EnvI sem a -> EnvI sem b) -> EnvI sem (a -> b)
f = liftSOn (ECons (liftOfLength (LS LZ)) ENil)

-- Following versions have no issues in GHC 9.8
-- f = undefined $ liftSOn (ECons (liftOfLength (LS LZ)) ENil)
-- f = let h = liftSOn (ECons (liftOfLength (LS LZ)) ENil) in h
-- f = h where h = liftSOn (ECons (liftOfLength (LS LZ)) ENil)
-- f = liftSOn (ECons (DimSimple (LS LZ)) ENil)
-- f = liftSOn d where {d :: Env DimSimple '[ '[a] :~> b ]; d = (ECons (liftOfLength (LS LZ)) ENil) }


main :: IO ()
main = pure ()
