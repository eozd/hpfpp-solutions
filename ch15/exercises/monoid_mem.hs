newtype Mem s a = Mem
  { runMem :: s -> (a, s)
  }

instance Semigroup a => Semigroup (Mem s a) where
  (<>) (Mem f) (Mem g) =
    Mem
      (\s ->
         let (a1, s1) = g s
             (a2, s2) = f s1
          in (a1 <> a2, s2))

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))
  mappend = (<>)

f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0
