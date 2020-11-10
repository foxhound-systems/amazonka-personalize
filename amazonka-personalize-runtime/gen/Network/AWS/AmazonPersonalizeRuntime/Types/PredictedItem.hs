{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalizeRuntime.Types.PredictedItem
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalizeRuntime.Types.PredictedItem where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that identifies an item.
--
--
-- The and APIs return a list of @PredictedItem@ s.
--
--
-- /See:/ 'predictedItem' smart constructor.
data PredictedItem = PredictedItem'{_piScore ::
                                    !(Maybe Double),
                                    _piItemId :: !(Maybe Text)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PredictedItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piScore' - A numeric representation of the model's certainty that the item will be the next user selection. For more information on scoring logic, see 'how-scores-work' .
--
-- * 'piItemId' - The recommended item ID.
predictedItem
    :: PredictedItem
predictedItem
  = PredictedItem'{_piScore = Nothing,
                   _piItemId = Nothing}

-- | A numeric representation of the model's certainty that the item will be the next user selection. For more information on scoring logic, see 'how-scores-work' .
piScore :: Lens' PredictedItem (Maybe Double)
piScore = lens _piScore (\ s a -> s{_piScore = a})

-- | The recommended item ID.
piItemId :: Lens' PredictedItem (Maybe Text)
piItemId = lens _piItemId (\ s a -> s{_piItemId = a})

instance FromJSON PredictedItem where
        parseJSON
          = withObject "PredictedItem"
              (\ x ->
                 PredictedItem' <$>
                   (x .:? "score") <*> (x .:? "itemId"))

instance Hashable PredictedItem where

instance NFData PredictedItem where
