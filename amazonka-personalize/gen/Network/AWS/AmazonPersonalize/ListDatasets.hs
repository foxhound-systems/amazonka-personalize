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
-- Module      : Network.AWS.AmazonPersonalize.ListDatasets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of datasets contained in the given dataset group. The response provides the properties for each dataset, including the Amazon Resource Name (ARN). For more information on datasets, see 'CreateDataset' .
--
--
--
-- This operation returns paginated results.
module Network.AWS.AmazonPersonalize.ListDatasets
    (
    -- * Creating a Request
      listDatasets
    , ListDatasets
    -- * Request Lenses
    , ldNextToken
    , ldDatasetGroupARN
    , ldMaxResults

    -- * Destructuring the Response
    , listDatasetsResponse
    , ListDatasetsResponse
    -- * Response Lenses
    , ldrsNextToken
    , ldrsDatasets
    , ldrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDatasets' smart constructor.
data ListDatasets = ListDatasets'{_ldNextToken ::
                                  !(Maybe Text),
                                  _ldDatasetGroupARN :: !(Maybe Text),
                                  _ldMaxResults :: !(Maybe Nat)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDatasets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldNextToken' - A token returned from the previous call to @ListDatasetImportJobs@ for getting the next set of dataset import jobs (if they exist).
--
-- * 'ldDatasetGroupARN' - The Amazon Resource Name (ARN) of the dataset group that contains the datasets to list.
--
-- * 'ldMaxResults' - The maximum number of datasets to return.
listDatasets
    :: ListDatasets
listDatasets
  = ListDatasets'{_ldNextToken = Nothing,
                  _ldDatasetGroupARN = Nothing,
                  _ldMaxResults = Nothing}

-- | A token returned from the previous call to @ListDatasetImportJobs@ for getting the next set of dataset import jobs (if they exist).
ldNextToken :: Lens' ListDatasets (Maybe Text)
ldNextToken = lens _ldNextToken (\ s a -> s{_ldNextToken = a})

-- | The Amazon Resource Name (ARN) of the dataset group that contains the datasets to list.
ldDatasetGroupARN :: Lens' ListDatasets (Maybe Text)
ldDatasetGroupARN = lens _ldDatasetGroupARN (\ s a -> s{_ldDatasetGroupARN = a})

-- | The maximum number of datasets to return.
ldMaxResults :: Lens' ListDatasets (Maybe Natural)
ldMaxResults = lens _ldMaxResults (\ s a -> s{_ldMaxResults = a}) . mapping _Nat

instance AWSPager ListDatasets where
        page rq rs
          | stop (rs ^. ldrsNextToken) = Nothing
          | stop (rs ^. ldrsDatasets) = Nothing
          | otherwise =
            Just $ rq & ldNextToken .~ rs ^. ldrsNextToken

instance AWSRequest ListDatasets where
        type Rs ListDatasets = ListDatasetsResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 ListDatasetsResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "datasets" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListDatasets where

instance NFData ListDatasets where

instance ToHeaders ListDatasets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.ListDatasets" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDatasets where
        toJSON ListDatasets'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _ldNextToken,
                  ("datasetGroupArn" .=) <$> _ldDatasetGroupARN,
                  ("maxResults" .=) <$> _ldMaxResults])

instance ToPath ListDatasets where
        toPath = const "/"

instance ToQuery ListDatasets where
        toQuery = const mempty

-- | /See:/ 'listDatasetsResponse' smart constructor.
data ListDatasetsResponse = ListDatasetsResponse'{_ldrsNextToken
                                                  :: !(Maybe Text),
                                                  _ldrsDatasets ::
                                                  !(Maybe [DatasetSummary]),
                                                  _ldrsResponseStatus :: !Int}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDatasetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrsNextToken' - A token for getting the next set of datasets (if they exist).
--
-- * 'ldrsDatasets' - An array of @Dataset@ objects. Each object provides metadata information.
--
-- * 'ldrsResponseStatus' - -- | The response status code.
listDatasetsResponse
    :: Int -- ^ 'ldrsResponseStatus'
    -> ListDatasetsResponse
listDatasetsResponse pResponseStatus_
  = ListDatasetsResponse'{_ldrsNextToken = Nothing,
                          _ldrsDatasets = Nothing,
                          _ldrsResponseStatus = pResponseStatus_}

-- | A token for getting the next set of datasets (if they exist).
ldrsNextToken :: Lens' ListDatasetsResponse (Maybe Text)
ldrsNextToken = lens _ldrsNextToken (\ s a -> s{_ldrsNextToken = a})

-- | An array of @Dataset@ objects. Each object provides metadata information.
ldrsDatasets :: Lens' ListDatasetsResponse [DatasetSummary]
ldrsDatasets = lens _ldrsDatasets (\ s a -> s{_ldrsDatasets = a}) . _Default . _Coerce

-- | -- | The response status code.
ldrsResponseStatus :: Lens' ListDatasetsResponse Int
ldrsResponseStatus = lens _ldrsResponseStatus (\ s a -> s{_ldrsResponseStatus = a})

instance NFData ListDatasetsResponse where
