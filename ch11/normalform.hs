-- data FlowerType = Gardenia | Daisy | Rose | Lilac

type Gardener = String

-- data Garden = Garden Gardener FlowerType

data Garden = Gardenia Gardener | Daisy Gardener | Rose Gardener | Lilac Gardener
