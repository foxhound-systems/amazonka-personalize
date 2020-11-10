{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.TrainingMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.TrainingMode where

import Network.AWS.Prelude
  
data TrainingMode = Full
                  | Update
                      deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                                Typeable, Generic)

instance FromText TrainingMode where
    parser = takeLowerText >>= \case
        "full" -> pure Full
        "update" -> pure Update
        e -> fromTextError $ "Failure parsing TrainingMode from value: '" <> e
           <> "'. Accepted values: full, update"

instance ToText TrainingMode where
    toText = \case
        Full -> "FULL"
        Update -> "UPDATE"

instance Hashable     TrainingMode
instance NFData       TrainingMode
instance ToByteString TrainingMode
instance ToQuery      TrainingMode
instance ToHeader     TrainingMode

instance ToJSON TrainingMode where
    toJSON = toJSONText

instance FromJSON TrainingMode where
    parseJSON = parseJSONText "TrainingMode"
