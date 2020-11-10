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
-- Module      : Network.AWS.AmazonPersonalize.ListDatasetImportJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of dataset import jobs that use the given dataset. When a dataset is not specified, all the dataset import jobs associated with the account are listed. The response provides the properties for each dataset import job, including the Amazon Resource Name (ARN). For more information on dataset import jobs, see 'CreateDatasetImportJob' . For more information on datasets, see 'CreateDataset' .
--
--
--
-- This operation returns paginated results.
module Network.AWS.AmazonPersonalize.ListDatasetImportJobs
    (
    -- * Creating a Request
      listDatasetImportJobs
    , ListDatasetImportJobs
    -- * Request Lenses
    , ldijDatasetARN
    , ldijNextToken
    , ldijMaxResults

    -- * Destructuring the Response
    , listDatasetImportJobsResponse
    , ListDatasetImportJobsResponse
    -- * Response Lenses
    , ldijrsDatasetImportJobs
    , ldijrsNextToken
    , ldijrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDatasetImportJobs' smart constructor.
data ListDatasetImportJobs = ListDatasetImportJobs'{_ldijDatasetARN
                                                    :: !(Maybe Text),
                                                    _ldijNextToken ::
                                                    !(Maybe Text),
                                                    _ldijMaxResults ::
                                                    !(Maybe Nat)}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'ListDatasetImportJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldijDatasetARN' - The Amazon Resource Name (ARN) of the dataset to list the dataset import jobs for.
--
-- * 'ldijNextToken' - A token returned from the previous call to @ListDatasetImportJobs@ for getting the next set of dataset import jobs (if they exist).
--
-- * 'ldijMaxResults' - The maximum number of dataset import jobs to return.
listDatasetImportJobs
    :: ListDatasetImportJobs
listDatasetImportJobs
  = ListDatasetImportJobs'{_ldijDatasetARN = Nothing,
                           _ldijNextToken = Nothing, _ldijMaxResults = Nothing}

-- | The Amazon Resource Name (ARN) of the dataset to list the dataset import jobs for.
ldijDatasetARN :: Lens' ListDatasetImportJobs (Maybe Text)
ldijDatasetARN = lens _ldijDatasetARN (\ s a -> s{_ldijDatasetARN = a})

-- | A token returned from the previous call to @ListDatasetImportJobs@ for getting the next set of dataset import jobs (if they exist).
ldijNextToken :: Lens' ListDatasetImportJobs (Maybe Text)
ldijNextToken = lens _ldijNextToken (\ s a -> s{_ldijNextToken = a})

-- | The maximum number of dataset import jobs to return.
ldijMaxResults :: Lens' ListDatasetImportJobs (Maybe Natural)
ldijMaxResults = lens _ldijMaxResults (\ s a -> s{_ldijMaxResults = a}) . mapping _Nat

instance AWSPager ListDatasetImportJobs where
        page rq rs
          | stop (rs ^. ldijrsNextToken) = Nothing
          | stop (rs ^. ldijrsDatasetImportJobs) = Nothing
          | otherwise =
            Just $ rq & ldijNextToken .~ rs ^. ldijrsNextToken

instance AWSRequest ListDatasetImportJobs where
        type Rs ListDatasetImportJobs =
             ListDatasetImportJobsResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 ListDatasetImportJobsResponse' <$>
                   (x .?> "datasetImportJobs" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListDatasetImportJobs where

instance NFData ListDatasetImportJobs where

instance ToHeaders ListDatasetImportJobs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.ListDatasetImportJobs" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDatasetImportJobs where
        toJSON ListDatasetImportJobs'{..}
          = object
              (catMaybes
                 [("datasetArn" .=) <$> _ldijDatasetARN,
                  ("nextToken" .=) <$> _ldijNextToken,
                  ("maxResults" .=) <$> _ldijMaxResults])

instance ToPath ListDatasetImportJobs where
        toPath = const "/"

instance ToQuery ListDatasetImportJobs where
        toQuery = const mempty

-- | /See:/ 'listDatasetImportJobsResponse' smart constructor.
data ListDatasetImportJobsResponse = ListDatasetImportJobsResponse'{_ldijrsDatasetImportJobs
                                                                    ::
                                                                    !(Maybe
                                                                        [DatasetImportJobSummary]),
                                                                    _ldijrsNextToken
                                                                    ::
                                                                    !(Maybe
                                                                        Text),
                                                                    _ldijrsResponseStatus
                                                                    :: !Int}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'ListDatasetImportJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldijrsDatasetImportJobs' - The list of dataset import jobs.
--
-- * 'ldijrsNextToken' - A token for getting the next set of dataset import jobs (if they exist).
--
-- * 'ldijrsResponseStatus' - -- | The response status code.
listDatasetImportJobsResponse
    :: Int -- ^ 'ldijrsResponseStatus'
    -> ListDatasetImportJobsResponse
listDatasetImportJobsResponse pResponseStatus_
  = ListDatasetImportJobsResponse'{_ldijrsDatasetImportJobs
                                     = Nothing,
                                   _ldijrsNextToken = Nothing,
                                   _ldijrsResponseStatus = pResponseStatus_}

-- | The list of dataset import jobs.
ldijrsDatasetImportJobs :: Lens' ListDatasetImportJobsResponse [DatasetImportJobSummary]
ldijrsDatasetImportJobs = lens _ldijrsDatasetImportJobs (\ s a -> s{_ldijrsDatasetImportJobs = a}) . _Default . _Coerce

-- | A token for getting the next set of dataset import jobs (if they exist).
ldijrsNextToken :: Lens' ListDatasetImportJobsResponse (Maybe Text)
ldijrsNextToken = lens _ldijrsNextToken (\ s a -> s{_ldijrsNextToken = a})

-- | -- | The response status code.
ldijrsResponseStatus :: Lens' ListDatasetImportJobsResponse Int
ldijrsResponseStatus = lens _ldijrsResponseStatus (\ s a -> s{_ldijrsResponseStatus = a})

instance NFData ListDatasetImportJobsResponse where
