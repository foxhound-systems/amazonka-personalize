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
-- Module      : Network.AWS.AmazonPersonalize.DescribeBatchInferenceJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties of a batch inference job including name, Amazon Resource Name (ARN), status, input and output configurations, and the ARN of the solution version used to generate the recommendations.
--
--
module Network.AWS.AmazonPersonalize.DescribeBatchInferenceJob
    (
    -- * Creating a Request
      describeBatchInferenceJob
    , DescribeBatchInferenceJob
    -- * Request Lenses
    , dbijBatchInferenceJobARN

    -- * Destructuring the Response
    , describeBatchInferenceJobResponse
    , DescribeBatchInferenceJobResponse
    -- * Response Lenses
    , dbijrsBatchInferenceJob
    , dbijrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeBatchInferenceJob' smart constructor.
newtype DescribeBatchInferenceJob = DescribeBatchInferenceJob'{_dbijBatchInferenceJobARN
                                                               :: Text}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'DescribeBatchInferenceJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbijBatchInferenceJobARN' - The ARN of the batch inference job to describe.
describeBatchInferenceJob
    :: Text -- ^ 'dbijBatchInferenceJobARN'
    -> DescribeBatchInferenceJob
describeBatchInferenceJob pBatchInferenceJobARN_
  = DescribeBatchInferenceJob'{_dbijBatchInferenceJobARN
                                 = pBatchInferenceJobARN_}

-- | The ARN of the batch inference job to describe.
dbijBatchInferenceJobARN :: Lens' DescribeBatchInferenceJob Text
dbijBatchInferenceJobARN = lens _dbijBatchInferenceJobARN (\ s a -> s{_dbijBatchInferenceJobARN = a})

instance AWSRequest DescribeBatchInferenceJob where
        type Rs DescribeBatchInferenceJob =
             DescribeBatchInferenceJobResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 DescribeBatchInferenceJobResponse' <$>
                   (x .?> "batchInferenceJob") <*> (pure (fromEnum s)))

instance Hashable DescribeBatchInferenceJob where

instance NFData DescribeBatchInferenceJob where

instance ToHeaders DescribeBatchInferenceJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DescribeBatchInferenceJob" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeBatchInferenceJob where
        toJSON DescribeBatchInferenceJob'{..}
          = object
              (catMaybes
                 [Just
                    ("batchInferenceJobArn" .=
                       _dbijBatchInferenceJobARN)])

instance ToPath DescribeBatchInferenceJob where
        toPath = const "/"

instance ToQuery DescribeBatchInferenceJob where
        toQuery = const mempty

-- | /See:/ 'describeBatchInferenceJobResponse' smart constructor.
data DescribeBatchInferenceJobResponse = DescribeBatchInferenceJobResponse'{_dbijrsBatchInferenceJob
                                                                            ::
                                                                            !(Maybe
                                                                                BatchInferenceJob),
                                                                            _dbijrsResponseStatus
                                                                            ::
                                                                            !Int}
                                           deriving (Eq, Read, Show, Data,
                                                     Typeable, Generic)

-- | Creates a value of 'DescribeBatchInferenceJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbijrsBatchInferenceJob' - Information on the specified batch inference job.
--
-- * 'dbijrsResponseStatus' - -- | The response status code.
describeBatchInferenceJobResponse
    :: Int -- ^ 'dbijrsResponseStatus'
    -> DescribeBatchInferenceJobResponse
describeBatchInferenceJobResponse pResponseStatus_
  = DescribeBatchInferenceJobResponse'{_dbijrsBatchInferenceJob
                                         = Nothing,
                                       _dbijrsResponseStatus = pResponseStatus_}

-- | Information on the specified batch inference job.
dbijrsBatchInferenceJob :: Lens' DescribeBatchInferenceJobResponse (Maybe BatchInferenceJob)
dbijrsBatchInferenceJob = lens _dbijrsBatchInferenceJob (\ s a -> s{_dbijrsBatchInferenceJob = a})

-- | -- | The response status code.
dbijrsResponseStatus :: Lens' DescribeBatchInferenceJobResponse Int
dbijrsResponseStatus = lens _dbijrsResponseStatus (\ s a -> s{_dbijrsResponseStatus = a})

instance NFData DescribeBatchInferenceJobResponse
         where
