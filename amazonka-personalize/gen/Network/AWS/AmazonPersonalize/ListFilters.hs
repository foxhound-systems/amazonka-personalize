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
-- Module      : Network.AWS.AmazonPersonalize.ListFilters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all filters that belong to a given dataset group.
--
--
module Network.AWS.AmazonPersonalize.ListFilters
    (
    -- * Creating a Request
      listFilters
    , ListFilters
    -- * Request Lenses
    , lfNextToken
    , lfDatasetGroupARN
    , lfMaxResults

    -- * Destructuring the Response
    , listFiltersResponse
    , ListFiltersResponse
    -- * Response Lenses
    , lfrsFilters
    , lfrsNextToken
    , lfrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listFilters' smart constructor.
data ListFilters = ListFilters'{_lfNextToken ::
                                !(Maybe Text),
                                _lfDatasetGroupARN :: !(Maybe Text),
                                _lfMaxResults :: !(Maybe Nat)}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfNextToken' - A token returned from the previous call to @ListFilters@ for getting the next set of filters (if they exist).
--
-- * 'lfDatasetGroupARN' - The ARN of the dataset group that contains the filters.
--
-- * 'lfMaxResults' - The maximum number of filters to return.
listFilters
    :: ListFilters
listFilters
  = ListFilters'{_lfNextToken = Nothing,
                 _lfDatasetGroupARN = Nothing,
                 _lfMaxResults = Nothing}

-- | A token returned from the previous call to @ListFilters@ for getting the next set of filters (if they exist).
lfNextToken :: Lens' ListFilters (Maybe Text)
lfNextToken = lens _lfNextToken (\ s a -> s{_lfNextToken = a})

-- | The ARN of the dataset group that contains the filters.
lfDatasetGroupARN :: Lens' ListFilters (Maybe Text)
lfDatasetGroupARN = lens _lfDatasetGroupARN (\ s a -> s{_lfDatasetGroupARN = a})

-- | The maximum number of filters to return.
lfMaxResults :: Lens' ListFilters (Maybe Natural)
lfMaxResults = lens _lfMaxResults (\ s a -> s{_lfMaxResults = a}) . mapping _Nat

instance AWSRequest ListFilters where
        type Rs ListFilters = ListFiltersResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 ListFiltersResponse' <$>
                   (x .?> "Filters" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListFilters where

instance NFData ListFilters where

instance ToHeaders ListFilters where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.ListFilters" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListFilters where
        toJSON ListFilters'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lfNextToken,
                  ("datasetGroupArn" .=) <$> _lfDatasetGroupARN,
                  ("maxResults" .=) <$> _lfMaxResults])

instance ToPath ListFilters where
        toPath = const "/"

instance ToQuery ListFilters where
        toQuery = const mempty

-- | /See:/ 'listFiltersResponse' smart constructor.
data ListFiltersResponse = ListFiltersResponse'{_lfrsFilters
                                                :: !(Maybe [FilterSummary]),
                                                _lfrsNextToken :: !(Maybe Text),
                                                _lfrsResponseStatus :: !Int}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListFiltersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfrsFilters' - A list of returned filters.
--
-- * 'lfrsNextToken' - A token for getting the next set of filters (if they exist).
--
-- * 'lfrsResponseStatus' - -- | The response status code.
listFiltersResponse
    :: Int -- ^ 'lfrsResponseStatus'
    -> ListFiltersResponse
listFiltersResponse pResponseStatus_
  = ListFiltersResponse'{_lfrsFilters = Nothing,
                         _lfrsNextToken = Nothing,
                         _lfrsResponseStatus = pResponseStatus_}

-- | A list of returned filters.
lfrsFilters :: Lens' ListFiltersResponse [FilterSummary]
lfrsFilters = lens _lfrsFilters (\ s a -> s{_lfrsFilters = a}) . _Default . _Coerce

-- | A token for getting the next set of filters (if they exist).
lfrsNextToken :: Lens' ListFiltersResponse (Maybe Text)
lfrsNextToken = lens _lfrsNextToken (\ s a -> s{_lfrsNextToken = a})

-- | -- | The response status code.
lfrsResponseStatus :: Lens' ListFiltersResponse Int
lfrsResponseStatus = lens _lfrsResponseStatus (\ s a -> s{_lfrsResponseStatus = a})

instance NFData ListFiltersResponse where
