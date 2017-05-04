{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JsonValue where

import Core
import List

type Assoc = List (Chars, JsonValue)

data JsonValue =
     JsonString Chars
   | JsonRational  Bool !Rational
   | JsonObject Assoc
   | JsonArray  (List JsonValue)
   | JsonTrue
   | JsonFalse
   | JsonNull
  deriving (Show, Eq)
