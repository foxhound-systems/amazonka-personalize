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
-- Module      : Network.AWS.AmazonPersonalize.ListBatchInferenceJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the batch inference jobs that have been performed off of a solution version.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AmazonPersonalize.ListBatchInferenceJobs
    (
    -- * Creating a Request
      listBatchInferenceJobs
    , ListBatchInferenceJobs
    -- * Request Lenses
    , lbijNextToken
    , lbijMaxResults
    , lbijSolutionVersionARN

    -- * Destructuring the Response
    , listBatchInferenceJobsResponse
    , ListBatchInferenceJobsResponse
    -- * Response Lenses
    , lbijrsBatchInferenceJobs
    , lbijrsNextToken
    , lbijrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listBatchInferenceJobs' smart constructor.
data ListBatchInferenceJobs = ListBatchInferenceJobs'{_lbijNextToken
                                                      :: !(Maybe Text),
                                                      _lbijMaxResults ::
                                                      !(Maybe Nat),
                                                      _lbijSolutionVersionARN ::
                                                      !(Maybe Text)}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'ListBatchInferenceJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbijNextToken' - The token to request the next page of results.
--
-- * 'lbijMaxResults' - The maximum number of batch inference job results to return in each page. The default value is 100.
--
-- * 'lbijSolutionVersionARN' - The Amazon Resource Name (ARN) of the solution version from which the batch inference jobs were created.
listBatchInferenceJobs
    :: ListBatchInferenceJobs
listBatchInferenceJobs
  = ListBatchInferenceJobs'{_lbijNextToken = Nothing,
                            _lbijMaxResults = Nothing,
                            _lbijSolutionVersionARN = Nothing}

-- | The token to request the next page of results.
lbijNextToken :: Lens' ListBatchInferenceJobs (Maybe Text)
lbijNextToken = lens _lbijNextToken (\ s a -> s{_lbijNextToken = a})

-- | The maximum number of batch inference job results to return in each page. The default value is 100.
lbijMaxResults :: Lens' ListBatchInferenceJobs (Maybe Natural)
lbijMaxResults = lens _lbijMaxResults (\ s a -> s{_lbijMaxResults = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the solution version from which the batch inference jobs were created.
lbijSolutionVersionARN :: Lens' ListBatchInferenceJobs (Maybe Text)
lbijSolutionVersionARN = lens _lbijSolutionVersionARN (\ s a -> s{_lbijSolutionVersionARN = a})

instance AWSPager ListBatchInferenceJobs where
        page rq rs
          | stop (rs ^. lbijrsNextToken) = Nothing
          | stop (rs ^. lbijrsBatchInferenceJobs) = Nothing
          | otherwise =
            Just $ rq & lbijNextToken .~ rs ^. lbijrsNextToken

instance AWSRequest ListBatchInferenceJobs where
        type Rs ListBatchInferenceJobs =
             ListBatchInferenceJobsResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 ListBatchInferenceJobsResponse' <$>
                   (x .?> "batchInferenceJobs" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListBatchInferenceJobs where

instance NFData ListBatchInferenceJobs where

instance ToHeaders ListBatchInferenceJobs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.ListBatchInferenceJobs" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListBatchInferenceJobs where
        toJSON ListBatchInferenceJobs'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lbijNextToken,
                  ("maxResults" .=) <$> _lbijMaxResults,
                  ("solutionVersionArn" .=) <$>
                    _lbijSolutionVersionARN])

instance ToPath ListBatchInferenceJobs where
        toPath = const "/"

instance ToQuery ListBatchInferenceJobs where
        toQuery = const mempty

-- | /See:/ 'listBatchInferenceJobsResponse' smart constructor.
data ListBatchInferenceJobsResponse = ListBatchInferenceJobsResponse'{_lbijrsBatchInferenceJobs
                                                                      ::
                                                                      !(Maybe
                                                                          [BatchInferenceJobSummary]),
                                                                      _lbijrsNextToken
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _lbijrsResponseStatus
                                                                      :: !Int}
                                        deriving (Eq, Read, Show, Data,
                                                  Typeable, Generic)

-- | Creates a value of 'ListBatchInferenceJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbijrsBatchInferenceJobs' - A list containing information on each job that is returned.
--
-- * 'lbijrsNextToken' - The token to use to retreive the next page of results. The value is @null@ when there are no more results to return.
--
-- * 'lbijrsResponseStatus' - -- | The response status code.
listBatchInferenceJobsResponse
    :: Int -- ^ 'lbijrsResponseStatus'
    -> ListBatchInferenceJobsResponse
listBatchInferenceJobsResponse pResponseStatus_
  = ListBatchInferenceJobsResponse'{_lbijrsBatchInferenceJobs
                                      = Nothing,
                                    _lbijrsNextToken = Nothing,
                                    _lbijrsResponseStatus = pResponseStatus_}

-- | A list containing information on each job that is returned.
lbijrsBatchInferenceJobs :: Lens' ListBatchInferenceJobsResponse [BatchInferenceJobSummary]
lbijrsBatchInferenceJobs = lens _lbijrsBatchInferenceJobs (\ s a -> s{_lbijrsBatchInferenceJobs = a}) . _Default . _Coerce

-- | The token to use to retreive the next page of results. The value is @null@ when there are no more results to return.
lbijrsNextToken :: Lens' ListBatchInferenceJobsResponse (Maybe Text)
lbijrsNextToken = lens _lbijrsNextToken (\ s a -> s{_lbijrsNextToken = a})

-- | -- | The response status code.
lbijrsResponseStatus :: Lens' ListBatchInferenceJobsResponse Int
lbijrsResponseStatus = lens _lbijrsResponseStatus (\ s a -> s{_lbijrsResponseStatus = a})

instance NFData ListBatchInferenceJobsResponse where
