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
-- Module      : Network.AWS.AmazonPersonalize.CreateDatasetImportJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job that imports training data from your data source (an Amazon S3 bucket) to an Amazon Personalize dataset. To allow Amazon Personalize to import the training data, you must specify an AWS Identity and Access Management (IAM) role that has permission to read from the data source, as Amazon Personalize makes a copy of your data and processes it in an internal AWS system.
--
--
-- /Important:/ The dataset import job replaces any previous data in the dataset.
--
-- __Status__ 
--
-- A dataset import job can be in one of the following states:
--
--     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
--
--
-- To get the status of the import job, call 'DescribeDatasetImportJob' , providing the Amazon Resource Name (ARN) of the dataset import job. The dataset import is complete when the status shows as ACTIVE. If the status shows as CREATE FAILED, the response includes a @failureReason@ key, which describes why the job failed.
--
-- __Related APIs__ 
--
--     * 'ListDatasetImportJobs' 
--
--     * 'DescribeDatasetImportJob' 
--
--
--
module Network.AWS.AmazonPersonalize.CreateDatasetImportJob
    (
    -- * Creating a Request
      createDatasetImportJob
    , CreateDatasetImportJob
    -- * Request Lenses
    , cdijJobName
    , cdijDatasetARN
    , cdijDataSource
    , cdijRoleARN

    -- * Destructuring the Response
    , createDatasetImportJobResponse
    , CreateDatasetImportJobResponse
    -- * Response Lenses
    , cdijrsDatasetImportJobARN
    , cdijrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDatasetImportJob' smart constructor.
data CreateDatasetImportJob = CreateDatasetImportJob'{_cdijJobName
                                                      :: !Text,
                                                      _cdijDatasetARN :: !Text,
                                                      _cdijDataSource ::
                                                      !DataSource,
                                                      _cdijRoleARN :: !Text}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'CreateDatasetImportJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdijJobName' - The name for the dataset import job.
--
-- * 'cdijDatasetARN' - The ARN of the dataset that receives the imported data.
--
-- * 'cdijDataSource' - The Amazon S3 bucket that contains the training data to import.
--
-- * 'cdijRoleARN' - The ARN of the IAM role that has permissions to read from the Amazon S3 data source.
createDatasetImportJob
    :: Text -- ^ 'cdijJobName'
    -> Text -- ^ 'cdijDatasetARN'
    -> DataSource -- ^ 'cdijDataSource'
    -> Text -- ^ 'cdijRoleARN'
    -> CreateDatasetImportJob
createDatasetImportJob pJobName_ pDatasetARN_
  pDataSource_ pRoleARN_
  = CreateDatasetImportJob'{_cdijJobName = pJobName_,
                            _cdijDatasetARN = pDatasetARN_,
                            _cdijDataSource = pDataSource_,
                            _cdijRoleARN = pRoleARN_}

-- | The name for the dataset import job.
cdijJobName :: Lens' CreateDatasetImportJob Text
cdijJobName = lens _cdijJobName (\ s a -> s{_cdijJobName = a})

-- | The ARN of the dataset that receives the imported data.
cdijDatasetARN :: Lens' CreateDatasetImportJob Text
cdijDatasetARN = lens _cdijDatasetARN (\ s a -> s{_cdijDatasetARN = a})

-- | The Amazon S3 bucket that contains the training data to import.
cdijDataSource :: Lens' CreateDatasetImportJob DataSource
cdijDataSource = lens _cdijDataSource (\ s a -> s{_cdijDataSource = a})

-- | The ARN of the IAM role that has permissions to read from the Amazon S3 data source.
cdijRoleARN :: Lens' CreateDatasetImportJob Text
cdijRoleARN = lens _cdijRoleARN (\ s a -> s{_cdijRoleARN = a})

instance AWSRequest CreateDatasetImportJob where
        type Rs CreateDatasetImportJob =
             CreateDatasetImportJobResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 CreateDatasetImportJobResponse' <$>
                   (x .?> "datasetImportJobArn") <*>
                     (pure (fromEnum s)))

instance Hashable CreateDatasetImportJob where

instance NFData CreateDatasetImportJob where

instance ToHeaders CreateDatasetImportJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.CreateDatasetImportJob" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDatasetImportJob where
        toJSON CreateDatasetImportJob'{..}
          = object
              (catMaybes
                 [Just ("jobName" .= _cdijJobName),
                  Just ("datasetArn" .= _cdijDatasetARN),
                  Just ("dataSource" .= _cdijDataSource),
                  Just ("roleArn" .= _cdijRoleARN)])

instance ToPath CreateDatasetImportJob where
        toPath = const "/"

instance ToQuery CreateDatasetImportJob where
        toQuery = const mempty

-- | /See:/ 'createDatasetImportJobResponse' smart constructor.
data CreateDatasetImportJobResponse = CreateDatasetImportJobResponse'{_cdijrsDatasetImportJobARN
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _cdijrsResponseStatus
                                                                      :: !Int}
                                        deriving (Eq, Read, Show, Data,
                                                  Typeable, Generic)

-- | Creates a value of 'CreateDatasetImportJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdijrsDatasetImportJobARN' - The ARN of the dataset import job.
--
-- * 'cdijrsResponseStatus' - -- | The response status code.
createDatasetImportJobResponse
    :: Int -- ^ 'cdijrsResponseStatus'
    -> CreateDatasetImportJobResponse
createDatasetImportJobResponse pResponseStatus_
  = CreateDatasetImportJobResponse'{_cdijrsDatasetImportJobARN
                                      = Nothing,
                                    _cdijrsResponseStatus = pResponseStatus_}

-- | The ARN of the dataset import job.
cdijrsDatasetImportJobARN :: Lens' CreateDatasetImportJobResponse (Maybe Text)
cdijrsDatasetImportJobARN = lens _cdijrsDatasetImportJobARN (\ s a -> s{_cdijrsDatasetImportJobARN = a})

-- | -- | The response status code.
cdijrsResponseStatus :: Lens' CreateDatasetImportJobResponse Int
cdijrsResponseStatus = lens _cdijrsResponseStatus (\ s a -> s{_cdijrsResponseStatus = a})

instance NFData CreateDatasetImportJobResponse where
