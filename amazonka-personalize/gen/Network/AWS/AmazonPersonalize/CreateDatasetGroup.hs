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
-- Module      : Network.AWS.AmazonPersonalize.CreateDatasetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty dataset group. A dataset group contains related datasets that supply data for training a model. A dataset group can contain at most three datasets, one for each type of dataset:
--
--
--     * Interactions
--
--     * Items
--
--     * Users
--
--
--
-- To train a model (create a solution), a dataset group that contains an @Interactions@ dataset is required. Call 'CreateDataset' to add a dataset to the group.
--
-- A dataset group can be in one of the following states:
--
--     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
--     * DELETE PENDING
--
--
--
-- To get the status of the dataset group, call 'DescribeDatasetGroup' . If the status shows as CREATE FAILED, the response includes a @failureReason@ key, which describes why the creation failed.
--
-- You can specify an AWS Key Management Service (KMS) key to encrypt the datasets in the group. If you specify a KMS key, you must also include an AWS Identity and Access Management (IAM) role that has permission to access the key.
--
-- __APIs that require a dataset group ARN in the request__ 
--
--     * 'CreateDataset' 
--
--     * 'CreateEventTracker' 
--
--     * 'CreateSolution' 
--
--
--
-- __Related APIs__ 
--
--     * 'ListDatasetGroups' 
--
--     * 'DescribeDatasetGroup' 
--
--     * 'DeleteDatasetGroup' 
--
--
--
module Network.AWS.AmazonPersonalize.CreateDatasetGroup
    (
    -- * Creating a Request
      createDatasetGroup
    , CreateDatasetGroup
    -- * Request Lenses
    , cdgKmsKeyARN
    , cdgRoleARN
    , cdgName

    -- * Destructuring the Response
    , createDatasetGroupResponse
    , CreateDatasetGroupResponse
    -- * Response Lenses
    , cdgrsDatasetGroupARN
    , cdgrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDatasetGroup' smart constructor.
data CreateDatasetGroup = CreateDatasetGroup'{_cdgKmsKeyARN
                                              :: !(Maybe Text),
                                              _cdgRoleARN :: !(Maybe Text),
                                              _cdgName :: !Text}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDatasetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdgKmsKeyARN' - The Amazon Resource Name (ARN) of a KMS key used to encrypt the datasets.
--
-- * 'cdgRoleARN' - The ARN of the IAM role that has permissions to access the KMS key. Supplying an IAM role is only valid when also specifying a KMS key.
--
-- * 'cdgName' - The name for the new dataset group.
createDatasetGroup
    :: Text -- ^ 'cdgName'
    -> CreateDatasetGroup
createDatasetGroup pName_
  = CreateDatasetGroup'{_cdgKmsKeyARN = Nothing,
                        _cdgRoleARN = Nothing, _cdgName = pName_}

-- | The Amazon Resource Name (ARN) of a KMS key used to encrypt the datasets.
cdgKmsKeyARN :: Lens' CreateDatasetGroup (Maybe Text)
cdgKmsKeyARN = lens _cdgKmsKeyARN (\ s a -> s{_cdgKmsKeyARN = a})

-- | The ARN of the IAM role that has permissions to access the KMS key. Supplying an IAM role is only valid when also specifying a KMS key.
cdgRoleARN :: Lens' CreateDatasetGroup (Maybe Text)
cdgRoleARN = lens _cdgRoleARN (\ s a -> s{_cdgRoleARN = a})

-- | The name for the new dataset group.
cdgName :: Lens' CreateDatasetGroup Text
cdgName = lens _cdgName (\ s a -> s{_cdgName = a})

instance AWSRequest CreateDatasetGroup where
        type Rs CreateDatasetGroup =
             CreateDatasetGroupResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 CreateDatasetGroupResponse' <$>
                   (x .?> "datasetGroupArn") <*> (pure (fromEnum s)))

instance Hashable CreateDatasetGroup where

instance NFData CreateDatasetGroup where

instance ToHeaders CreateDatasetGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.CreateDatasetGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDatasetGroup where
        toJSON CreateDatasetGroup'{..}
          = object
              (catMaybes
                 [("kmsKeyArn" .=) <$> _cdgKmsKeyARN,
                  ("roleArn" .=) <$> _cdgRoleARN,
                  Just ("name" .= _cdgName)])

instance ToPath CreateDatasetGroup where
        toPath = const "/"

instance ToQuery CreateDatasetGroup where
        toQuery = const mempty

-- | /See:/ 'createDatasetGroupResponse' smart constructor.
data CreateDatasetGroupResponse = CreateDatasetGroupResponse'{_cdgrsDatasetGroupARN
                                                              :: !(Maybe Text),
                                                              _cdgrsResponseStatus
                                                              :: !Int}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'CreateDatasetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdgrsDatasetGroupARN' - The Amazon Resource Name (ARN) of the new dataset group.
--
-- * 'cdgrsResponseStatus' - -- | The response status code.
createDatasetGroupResponse
    :: Int -- ^ 'cdgrsResponseStatus'
    -> CreateDatasetGroupResponse
createDatasetGroupResponse pResponseStatus_
  = CreateDatasetGroupResponse'{_cdgrsDatasetGroupARN =
                                  Nothing,
                                _cdgrsResponseStatus = pResponseStatus_}

-- | The Amazon Resource Name (ARN) of the new dataset group.
cdgrsDatasetGroupARN :: Lens' CreateDatasetGroupResponse (Maybe Text)
cdgrsDatasetGroupARN = lens _cdgrsDatasetGroupARN (\ s a -> s{_cdgrsDatasetGroupARN = a})

-- | -- | The response status code.
cdgrsResponseStatus :: Lens' CreateDatasetGroupResponse Int
cdgrsResponseStatus = lens _cdgrsResponseStatus (\ s a -> s{_cdgrsResponseStatus = a})

instance NFData CreateDatasetGroupResponse where
