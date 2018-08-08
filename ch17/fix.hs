a = const <$> Just "Hello" <*> Just "World"
b = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]
