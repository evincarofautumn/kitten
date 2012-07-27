module Utils where

boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe True = Just
boolToMaybe False = const Nothing
