module Monoidtest where
  -- folds and monoids
  -- The take-away message is that we can implement a fold for many (though not all) data types.
  -- The fold for T will take one (higher-order) argument for each of T’s constructors,
  -- encoding how to turn the values stored by that constructor into a value of the result type—
  -- assuming that any recursive occurrences of T have already been folded into a result.

newtype Product a =  Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)

-- instance Monoid Ordering where
--     mempty = EQ
--     LT `mappend` _ = LT
--     EQ `mappend` y = y
--     GT `mappend` _ = GT

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
    where vowels = length . filter (`elem` "aeiou")
-- The Ordering monoid is very cool because
-- it allows us to easily compare things by many different criteria and
-- put those criteria in an order themselves,
-- ranging from the most important to the least.

-- instance Monoid a => Monoid (Maybe a) where
--     mempty = Nothing
--     Nothing `mappend` m = m
--     m `mappend` Nothing = m
--     Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
