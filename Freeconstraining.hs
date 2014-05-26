{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Freeconstraining where

data ExpF e a where
  ValueE  :: a -> ExpF e a
  AddE    :: Num a => e a -> e a -> ExpF e a
  CondE   :: e Bool -> e a -> e a -> ExpF e a

deriving instance (Show a, Show (e a), Show (e Bool)) => Show (ExpF e a)

newtype Mu f a
  = Mu { unMu :: f (Mu f) a }

deriving instance Show (f (Mu f) a) => Show (Mu f a)

type Exp
  = Mu ExpF

valueE :: a -> Exp a
valueE
  = Mu . ValueE

addE :: Num a => Exp a -> Exp a -> Exp a
addE e1 e2
  = Mu (AddE e1 e2)

condE :: Exp Bool -> Exp a -> Exp a -> Exp a
condE p t f
  = Mu (CondE p t f)

class Elem a as where
  evidence :: Evidence a as

data Evidence a as where
  Head :: Evidence a (a ': as)
  Tail :: Elem a as => Evidence a (b ': as)

data D as f a where
  D :: Elem a as => f a -> D as f a

newtype O f g a
  = O { unO :: f (g a) }

newtype S as e a
  = S { unS :: D as (ExpF e) a }

type ExpS as
  = Mu (S as)

valueS :: Elem a as => a -> ExpS as a
valueS
  = Mu . S . D . ValueE

addS  :: (Elem a as, Num a)
      => ExpS as a
      -> ExpS as a
      -> ExpS as a

addS e1 e2
  = Mu (S (D (AddE e1 e2)))

condS p t f
  = Mu (S (D (CondE p t f)))
