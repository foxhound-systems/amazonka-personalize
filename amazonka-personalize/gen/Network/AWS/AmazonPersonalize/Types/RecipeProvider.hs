{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.RecipeProvider
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.RecipeProvider where

import Network.AWS.Prelude
  
data RecipeProvider = RecipeProviderService
                        deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                                  Typeable, Generic)

instance FromText RecipeProvider where
    parser = takeLowerText >>= \case
        "service" -> pure RecipeProviderService
        e -> fromTextError $ "Failure parsing RecipeProvider from value: '" <> e
           <> "'. Accepted values: service"

instance ToText RecipeProvider where
    toText = \case
        RecipeProviderService -> "SERVICE"

instance Hashable     RecipeProvider
instance NFData       RecipeProvider
instance ToByteString RecipeProvider
instance ToQuery      RecipeProvider
instance ToHeader     RecipeProvider

instance ToJSON RecipeProvider where
    toJSON = toJSONText
