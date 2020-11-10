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
-- Module      : Network.AWS.AmazonPersonalize.DescribeDatasetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the given dataset group. For more information on dataset groups, see 'CreateDatasetGroup' .
--
--
module Network.AWS.AmazonPersonalize.DescribeDatasetGroup
    (
    -- * Creating a Request
      describeDatasetGroup
    , DescribeDatasetGroup
    -- * Request Lenses
    , desDatasetGroupARN

    -- * Destructuring the Response
    , describeDatasetGroupResponse
    , DescribeDatasetGroupResponse
    -- * Response Lenses
    , ddgrsDatasetGroup
    , ddgrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDatasetGroup' smart constructor.
newtype DescribeDatasetGroup = DescribeDatasetGroup'{_desDatasetGroupARN
                                                     :: Text}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'DescribeDatasetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desDatasetGroupARN' - The Amazon Resource Name (ARN) of the dataset group to describe.
describeDatasetGroup
    :: Text -- ^ 'desDatasetGroupARN'
    -> DescribeDatasetGroup
describeDatasetGroup pDatasetGroupARN_
  = DescribeDatasetGroup'{_desDatasetGroupARN =
                            pDatasetGroupARN_}

-- | The Amazon Resource Name (ARN) of the dataset group to describe.
desDatasetGroupARN :: Lens' DescribeDatasetGroup Text
desDatasetGroupARN = lens _desDatasetGroupARN (\ s a -> s{_desDatasetGroupARN = a})

instance AWSRequest DescribeDatasetGroup where
        type Rs DescribeDatasetGroup =
             DescribeDatasetGroupResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDatasetGroupResponse' <$>
                   (x .?> "datasetGroup") <*> (pure (fromEnum s)))

instance Hashable DescribeDatasetGroup where

instance NFData DescribeDatasetGroup where

instance ToHeaders DescribeDatasetGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DescribeDatasetGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeDatasetGroup where
        toJSON DescribeDatasetGroup'{..}
          = object
              (catMaybes
                 [Just ("datasetGroupArn" .= _desDatasetGroupARN)])

instance ToPath DescribeDatasetGroup where
        toPath = const "/"

instance ToQuery DescribeDatasetGroup where
        toQuery = const mempty

-- | /See:/ 'describeDatasetGroupResponse' smart constructor.
data DescribeDatasetGroupResponse = DescribeDatasetGroupResponse'{_ddgrsDatasetGroup
                                                                  ::
                                                                  !(Maybe
                                                                      DatasetGroup),
                                                                  _ddgrsResponseStatus
                                                                  :: !Int}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'DescribeDatasetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddgrsDatasetGroup' - A listing of the dataset group's properties.
--
-- * 'ddgrsResponseStatus' - -- | The response status code.
describeDatasetGroupResponse
    :: Int -- ^ 'ddgrsResponseStatus'
    -> DescribeDatasetGroupResponse
describeDatasetGroupResponse pResponseStatus_
  = DescribeDatasetGroupResponse'{_ddgrsDatasetGroup =
                                    Nothing,
                                  _ddgrsResponseStatus = pResponseStatus_}

-- | A listing of the dataset group's properties.
ddgrsDatasetGroup :: Lens' DescribeDatasetGroupResponse (Maybe DatasetGroup)
ddgrsDatasetGroup = lens _ddgrsDatasetGroup (\ s a -> s{_ddgrsDatasetGroup = a})

-- | -- | The response status code.
ddgrsResponseStatus :: Lens' DescribeDatasetGroupResponse Int
ddgrsResponseStatus = lens _ddgrsResponseStatus (\ s a -> s{_ddgrsResponseStatus = a})

instance NFData DescribeDatasetGroupResponse where
