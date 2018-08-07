{-# LANGUAGE FlexibleInstances #-}

data Quant a b
  = Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a

data K a b =
  K a

instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b))

data EvilGoateeConst a b =
  GoatyConst a

instance Functor (EvilGoateeConst a) where
  fmap _ (GoatyConst a) = GoatyConst a

data LiftItOut f a =
  LiftItOut (f a)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut innerFunctor) = LiftItOut $ fmap f innerFunctor

data Parappa f g a =
  DaWrappa (f a)
           (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa innerFunctor1 innerFunctor2) =
    DaWrappa (fmap f innerFunctor1) (fmap f innerFunctor2)

data IgnoreOne f g a b =
  IgnoringSomething (f a)
                    (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x innerFunctor) =
    IgnoringSomething x (fmap f innerFunctor)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious x y innerFunctor) = Notorious x y (fmap f innerFunctor)

data List a = Nil | Cons a (List a)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a rest) = Cons (f a) (fmap f rest)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print str a) = Print str (f a)
    fmap f (Read g) = Read (f . g)
