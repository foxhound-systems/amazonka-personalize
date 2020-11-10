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
-- Module      : Network.AWS.AmazonPersonalize.DeleteDataset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a dataset. You can't delete a dataset if an associated @DatasetImportJob@ or @SolutionVersion@ is in the CREATE PENDING or IN PROGRESS state. For more information on datasets, see 'CreateDataset' .
--
--
module Network.AWS.AmazonPersonalize.DeleteDataset
    (
    -- * Creating a Request
      deleteDataset
    , DeleteDataset
    -- * Request Lenses
    , delDatasetARN

    -- * Destructuring the Response
    , deleteDatasetResponse
    , DeleteDatasetResponse
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDataset' smart constructor.
newtype DeleteDataset = DeleteDataset'{_delDatasetARN
                                       :: Text}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delDatasetARN' - The Amazon Resource Name (ARN) of the dataset to delete.
deleteDataset
    :: Text -- ^ 'delDatasetARN'
    -> DeleteDataset
deleteDataset pDatasetARN_
  = DeleteDataset'{_delDatasetARN = pDatasetARN_}

-- | The Amazon Resource Name (ARN) of the dataset to delete.
delDatasetARN :: Lens' DeleteDataset Text
delDatasetARN = lens _delDatasetARN (\ s a -> s{_delDatasetARN = a})

instance AWSRequest DeleteDataset where
        type Rs DeleteDataset = DeleteDatasetResponse
        request = postJSON amazonPersonalize
        response = receiveNull DeleteDatasetResponse'

instance Hashable DeleteDataset where

instance NFData DeleteDataset where

instance ToHeaders DeleteDataset where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DeleteDataset" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDataset where
        toJSON DeleteDataset'{..}
          = object
              (catMaybes [Just ("datasetArn" .= _delDatasetARN)])

instance ToPath DeleteDataset where
        toPath = const "/"

instance ToQuery DeleteDataset where
        toQuery = const mempty

-- | /See:/ 'deleteDatasetResponse' smart constructor.
data DeleteDatasetResponse = DeleteDatasetResponse'
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'DeleteDatasetResponse' with the minimum fields required to make a request.
--
deleteDatasetResponse
    :: DeleteDatasetResponse
deleteDatasetResponse = DeleteDatasetResponse'

instance NFData DeleteDatasetResponse where
