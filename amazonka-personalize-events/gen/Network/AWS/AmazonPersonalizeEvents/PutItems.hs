{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalizeEvents.PutItems
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more items to an Items dataset. For more information see 'importing-items' .
--
--
module Network.AWS.AmazonPersonalizeEvents.PutItems
    (
    -- * Creating a Request
      putItems
    , PutItems
    -- * Request Lenses
    , piDatasetARN
    , piItems

    -- * Destructuring the Response
    , putItemsResponse
    , PutItemsResponse
    ) where

import Network.AWS.AmazonPersonalizeEvents.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putItems' smart constructor.
data PutItems = PutItems'{_piDatasetARN :: !Text,
                          _piItems :: !(List1 Item)}
                  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutItems' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piDatasetARN' - The Amazon Resource Number (ARN) of the Items dataset you are adding the item or items to.
--
-- * 'piItems' - A list of item data.
putItems
    :: Text -- ^ 'piDatasetARN'
    -> NonEmpty Item -- ^ 'piItems'
    -> PutItems
putItems pDatasetARN_ pItems_
  = PutItems'{_piDatasetARN = pDatasetARN_,
              _piItems = _List1 # pItems_}

-- | The Amazon Resource Number (ARN) of the Items dataset you are adding the item or items to.
piDatasetARN :: Lens' PutItems Text
piDatasetARN = lens _piDatasetARN (\ s a -> s{_piDatasetARN = a})

-- | A list of item data.
piItems :: Lens' PutItems (NonEmpty Item)
piItems = lens _piItems (\ s a -> s{_piItems = a}) . _List1

instance AWSRequest PutItems where
        type Rs PutItems = PutItemsResponse
        request = postJSON amazonPersonalizeEvents
        response = receiveNull PutItemsResponse'

instance Hashable PutItems where

instance NFData PutItems where

instance ToHeaders PutItems where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutItems where
        toJSON PutItems'{..}
          = object
              (catMaybes
                 [Just ("datasetArn" .= _piDatasetARN),
                  Just ("items" .= _piItems)])

instance ToPath PutItems where
        toPath = const "/items"

instance ToQuery PutItems where
        toQuery = const mempty

-- | /See:/ 'putItemsResponse' smart constructor.
data PutItemsResponse = PutItemsResponse'
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutItemsResponse' with the minimum fields required to make a request.
--
putItemsResponse
    :: PutItemsResponse
putItemsResponse = PutItemsResponse'

instance NFData PutItemsResponse where
