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
-- Module      : Network.AWS.AmazonPersonalize.CreateDataset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty dataset and adds it to the specified dataset group. Use 'CreateDatasetImportJob' to import your training data to a dataset.
--
--
-- There are three types of datasets:
--
--     * Interactions
--
--     * Items
--
--     * Users
--
--
--
-- Each dataset type has an associated schema with required field types. Only the @Interactions@ dataset is required in order to train a model (also referred to as creating a solution).
--
-- A dataset can be in one of the following states:
--
--     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
--     * DELETE PENDING > DELETE IN_PROGRESS
--
--
--
-- To get the status of the dataset, call 'DescribeDataset' .
--
-- __Related APIs__ 
--
--     * 'CreateDatasetGroup' 
--
--     * 'ListDatasets' 
--
--     * 'DescribeDataset' 
--
--     * 'DeleteDataset' 
--
--
--
module Network.AWS.AmazonPersonalize.CreateDataset
    (
    -- * Creating a Request
      createDataset
    , CreateDataset
    -- * Request Lenses
    , cdName
    , cdSchemaARN
    , cdDatasetGroupARN
    , cdDatasetType

    -- * Destructuring the Response
    , createDatasetResponse
    , CreateDatasetResponse
    -- * Response Lenses
    , cdrsDatasetARN
    , cdrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDataset' smart constructor.
data CreateDataset = CreateDataset'{_cdName :: !Text,
                                    _cdSchemaARN :: !Text,
                                    _cdDatasetGroupARN :: !Text,
                                    _cdDatasetType :: !Text}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdName' - The name for the dataset.
--
-- * 'cdSchemaARN' - The ARN of the schema to associate with the dataset. The schema defines the dataset fields.
--
-- * 'cdDatasetGroupARN' - The Amazon Resource Name (ARN) of the dataset group to add the dataset to.
--
-- * 'cdDatasetType' - The type of dataset. One of the following (case insensitive) values:     * Interactions     * Items     * Users
createDataset
    :: Text -- ^ 'cdName'
    -> Text -- ^ 'cdSchemaARN'
    -> Text -- ^ 'cdDatasetGroupARN'
    -> Text -- ^ 'cdDatasetType'
    -> CreateDataset
createDataset pName_ pSchemaARN_ pDatasetGroupARN_
  pDatasetType_
  = CreateDataset'{_cdName = pName_,
                   _cdSchemaARN = pSchemaARN_,
                   _cdDatasetGroupARN = pDatasetGroupARN_,
                   _cdDatasetType = pDatasetType_}

-- | The name for the dataset.
cdName :: Lens' CreateDataset Text
cdName = lens _cdName (\ s a -> s{_cdName = a})

-- | The ARN of the schema to associate with the dataset. The schema defines the dataset fields.
cdSchemaARN :: Lens' CreateDataset Text
cdSchemaARN = lens _cdSchemaARN (\ s a -> s{_cdSchemaARN = a})

-- | The Amazon Resource Name (ARN) of the dataset group to add the dataset to.
cdDatasetGroupARN :: Lens' CreateDataset Text
cdDatasetGroupARN = lens _cdDatasetGroupARN (\ s a -> s{_cdDatasetGroupARN = a})

-- | The type of dataset. One of the following (case insensitive) values:     * Interactions     * Items     * Users
cdDatasetType :: Lens' CreateDataset Text
cdDatasetType = lens _cdDatasetType (\ s a -> s{_cdDatasetType = a})

instance AWSRequest CreateDataset where
        type Rs CreateDataset = CreateDatasetResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 CreateDatasetResponse' <$>
                   (x .?> "datasetArn") <*> (pure (fromEnum s)))

instance Hashable CreateDataset where

instance NFData CreateDataset where

instance ToHeaders CreateDataset where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.CreateDataset" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDataset where
        toJSON CreateDataset'{..}
          = object
              (catMaybes
                 [Just ("name" .= _cdName),
                  Just ("schemaArn" .= _cdSchemaARN),
                  Just ("datasetGroupArn" .= _cdDatasetGroupARN),
                  Just ("datasetType" .= _cdDatasetType)])

instance ToPath CreateDataset where
        toPath = const "/"

instance ToQuery CreateDataset where
        toQuery = const mempty

-- | /See:/ 'createDatasetResponse' smart constructor.
data CreateDatasetResponse = CreateDatasetResponse'{_cdrsDatasetARN
                                                    :: !(Maybe Text),
                                                    _cdrsResponseStatus :: !Int}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'CreateDatasetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrsDatasetARN' - The ARN of the dataset.
--
-- * 'cdrsResponseStatus' - -- | The response status code.
createDatasetResponse
    :: Int -- ^ 'cdrsResponseStatus'
    -> CreateDatasetResponse
createDatasetResponse pResponseStatus_
  = CreateDatasetResponse'{_cdrsDatasetARN = Nothing,
                           _cdrsResponseStatus = pResponseStatus_}

-- | The ARN of the dataset.
cdrsDatasetARN :: Lens' CreateDatasetResponse (Maybe Text)
cdrsDatasetARN = lens _cdrsDatasetARN (\ s a -> s{_cdrsDatasetARN = a})

-- | -- | The response status code.
cdrsResponseStatus :: Lens' CreateDatasetResponse Int
cdrsResponseStatus = lens _cdrsResponseStatus (\ s a -> s{_cdrsResponseStatus = a})

instance NFData CreateDatasetResponse where
