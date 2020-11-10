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
-- Module      : Network.AWS.AmazonPersonalize.DeleteDatasetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a dataset group. Before you delete a dataset group, you must delete the following:
--
--
--     * All associated event trackers.
--
--     * All associated solutions.
--
--     * All datasets in the dataset group.
--
--
--
module Network.AWS.AmazonPersonalize.DeleteDatasetGroup
    (
    -- * Creating a Request
      deleteDatasetGroup
    , DeleteDatasetGroup
    -- * Request Lenses
    , ddgDatasetGroupARN

    -- * Destructuring the Response
    , deleteDatasetGroupResponse
    , DeleteDatasetGroupResponse
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDatasetGroup' smart constructor.
newtype DeleteDatasetGroup = DeleteDatasetGroup'{_ddgDatasetGroupARN
                                                 :: Text}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'DeleteDatasetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddgDatasetGroupARN' - The ARN of the dataset group to delete.
deleteDatasetGroup
    :: Text -- ^ 'ddgDatasetGroupARN'
    -> DeleteDatasetGroup
deleteDatasetGroup pDatasetGroupARN_
  = DeleteDatasetGroup'{_ddgDatasetGroupARN =
                          pDatasetGroupARN_}

-- | The ARN of the dataset group to delete.
ddgDatasetGroupARN :: Lens' DeleteDatasetGroup Text
ddgDatasetGroupARN = lens _ddgDatasetGroupARN (\ s a -> s{_ddgDatasetGroupARN = a})

instance AWSRequest DeleteDatasetGroup where
        type Rs DeleteDatasetGroup =
             DeleteDatasetGroupResponse
        request = postJSON amazonPersonalize
        response = receiveNull DeleteDatasetGroupResponse'

instance Hashable DeleteDatasetGroup where

instance NFData DeleteDatasetGroup where

instance ToHeaders DeleteDatasetGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DeleteDatasetGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDatasetGroup where
        toJSON DeleteDatasetGroup'{..}
          = object
              (catMaybes
                 [Just ("datasetGroupArn" .= _ddgDatasetGroupARN)])

instance ToPath DeleteDatasetGroup where
        toPath = const "/"

instance ToQuery DeleteDatasetGroup where
        toQuery = const mempty

-- | /See:/ 'deleteDatasetGroupResponse' smart constructor.
data DeleteDatasetGroupResponse = DeleteDatasetGroupResponse'
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'DeleteDatasetGroupResponse' with the minimum fields required to make a request.
--
deleteDatasetGroupResponse
    :: DeleteDatasetGroupResponse
deleteDatasetGroupResponse
  = DeleteDatasetGroupResponse'

instance NFData DeleteDatasetGroupResponse where
