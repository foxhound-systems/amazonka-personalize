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
-- Module      : Network.AWS.AmazonPersonalize.DescribeDatasetImportJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the dataset import job created by 'CreateDatasetImportJob' , including the import job status.
--
--
module Network.AWS.AmazonPersonalize.DescribeDatasetImportJob
    (
    -- * Creating a Request
      describeDatasetImportJob
    , DescribeDatasetImportJob
    -- * Request Lenses
    , ddijDatasetImportJobARN

    -- * Destructuring the Response
    , describeDatasetImportJobResponse
    , DescribeDatasetImportJobResponse
    -- * Response Lenses
    , ddijrsDatasetImportJob
    , ddijrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDatasetImportJob' smart constructor.
newtype DescribeDatasetImportJob = DescribeDatasetImportJob'{_ddijDatasetImportJobARN
                                                             :: Text}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'DescribeDatasetImportJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddijDatasetImportJobARN' - The Amazon Resource Name (ARN) of the dataset import job to describe.
describeDatasetImportJob
    :: Text -- ^ 'ddijDatasetImportJobARN'
    -> DescribeDatasetImportJob
describeDatasetImportJob pDatasetImportJobARN_
  = DescribeDatasetImportJob'{_ddijDatasetImportJobARN
                                = pDatasetImportJobARN_}

-- | The Amazon Resource Name (ARN) of the dataset import job to describe.
ddijDatasetImportJobARN :: Lens' DescribeDatasetImportJob Text
ddijDatasetImportJobARN = lens _ddijDatasetImportJobARN (\ s a -> s{_ddijDatasetImportJobARN = a})

instance AWSRequest DescribeDatasetImportJob where
        type Rs DescribeDatasetImportJob =
             DescribeDatasetImportJobResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDatasetImportJobResponse' <$>
                   (x .?> "datasetImportJob") <*> (pure (fromEnum s)))

instance Hashable DescribeDatasetImportJob where

instance NFData DescribeDatasetImportJob where

instance ToHeaders DescribeDatasetImportJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DescribeDatasetImportJob" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeDatasetImportJob where
        toJSON DescribeDatasetImportJob'{..}
          = object
              (catMaybes
                 [Just
                    ("datasetImportJobArn" .= _ddijDatasetImportJobARN)])

instance ToPath DescribeDatasetImportJob where
        toPath = const "/"

instance ToQuery DescribeDatasetImportJob where
        toQuery = const mempty

-- | /See:/ 'describeDatasetImportJobResponse' smart constructor.
data DescribeDatasetImportJobResponse = DescribeDatasetImportJobResponse'{_ddijrsDatasetImportJob
                                                                          ::
                                                                          !(Maybe
                                                                              DatasetImportJob),
                                                                          _ddijrsResponseStatus
                                                                          ::
                                                                          !Int}
                                          deriving (Eq, Read, Show, Data,
                                                    Typeable, Generic)

-- | Creates a value of 'DescribeDatasetImportJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddijrsDatasetImportJob' - Information about the dataset import job, including the status. The status is one of the following values:     * CREATE PENDING     * CREATE IN_PROGRESS     * ACTIVE     * CREATE FAILED
--
-- * 'ddijrsResponseStatus' - -- | The response status code.
describeDatasetImportJobResponse
    :: Int -- ^ 'ddijrsResponseStatus'
    -> DescribeDatasetImportJobResponse
describeDatasetImportJobResponse pResponseStatus_
  = DescribeDatasetImportJobResponse'{_ddijrsDatasetImportJob
                                        = Nothing,
                                      _ddijrsResponseStatus = pResponseStatus_}

-- | Information about the dataset import job, including the status. The status is one of the following values:     * CREATE PENDING     * CREATE IN_PROGRESS     * ACTIVE     * CREATE FAILED
ddijrsDatasetImportJob :: Lens' DescribeDatasetImportJobResponse (Maybe DatasetImportJob)
ddijrsDatasetImportJob = lens _ddijrsDatasetImportJob (\ s a -> s{_ddijrsDatasetImportJob = a})

-- | -- | The response status code.
ddijrsResponseStatus :: Lens' DescribeDatasetImportJobResponse Int
ddijrsResponseStatus = lens _ddijrsResponseStatus (\ s a -> s{_ddijrsResponseStatus = a})

instance NFData DescribeDatasetImportJobResponse
         where
