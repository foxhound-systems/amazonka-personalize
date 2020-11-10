{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalizeEvents.Types.Item
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalizeEvents.Types.Item where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents item metadata added to an Items dataset using the @PutItems@ API.
--
--
--
-- /See:/ 'item' smart constructor.
data Item = Item'{_iProperties :: !(Maybe Text),
                  _iItemId :: !Text}
              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Item' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iProperties' - A string map of item-specific metadata. Each element in the map consists of a key-value pair. For example,  @{"numberOfRatings": "12"}@  The keys use camel case names that match the fields in the Items schema. In the above example, the @numberOfRatings@ would match the 'NUMBER_OF_RATINGS' field defined in the Items schema.
--
-- * 'iItemId' - The ID associated with the item.
item
    :: Text -- ^ 'iItemId'
    -> Item
item pItemId_
  = Item'{_iProperties = Nothing, _iItemId = pItemId_}

-- | A string map of item-specific metadata. Each element in the map consists of a key-value pair. For example,  @{"numberOfRatings": "12"}@  The keys use camel case names that match the fields in the Items schema. In the above example, the @numberOfRatings@ would match the 'NUMBER_OF_RATINGS' field defined in the Items schema.
iProperties :: Lens' Item (Maybe Text)
iProperties = lens _iProperties (\ s a -> s{_iProperties = a})

-- | The ID associated with the item.
iItemId :: Lens' Item Text
iItemId = lens _iItemId (\ s a -> s{_iItemId = a})

instance Hashable Item where

instance NFData Item where

instance ToJSON Item where
        toJSON Item'{..}
          = object
              (catMaybes
                 [("properties" .=) <$> _iProperties,
                  Just ("itemId" .= _iItemId)])
