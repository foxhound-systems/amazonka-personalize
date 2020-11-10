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
-- Module      : Network.AWS.AmazonPersonalize.DescribeDataset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the given dataset. For more information on datasets, see 'CreateDataset' .
--
--
module Network.AWS.AmazonPersonalize.DescribeDataset
    (
    -- * Creating a Request
      describeDataset
    , DescribeDataset
    -- * Request Lenses
    , ddDatasetARN

    -- * Destructuring the Response
    , describeDatasetResponse
    , DescribeDatasetResponse
    -- * Response Lenses
    , ddrsDataset
    , ddrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDataset' smart constructor.
newtype DescribeDataset = DescribeDataset'{_ddDatasetARN
                                           :: Text}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddDatasetARN' - The Amazon Resource Name (ARN) of the dataset to describe.
describeDataset
    :: Text -- ^ 'ddDatasetARN'
    -> DescribeDataset
describeDataset pDatasetARN_
  = DescribeDataset'{_ddDatasetARN = pDatasetARN_}

-- | The Amazon Resource Name (ARN) of the dataset to describe.
ddDatasetARN :: Lens' DescribeDataset Text
ddDatasetARN = lens _ddDatasetARN (\ s a -> s{_ddDatasetARN = a})

instance AWSRequest DescribeDataset where
        type Rs DescribeDataset = DescribeDatasetResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDatasetResponse' <$>
                   (x .?> "dataset") <*> (pure (fromEnum s)))

instance Hashable DescribeDataset where

instance NFData DescribeDataset where

instance ToHeaders DescribeDataset where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DescribeDataset" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeDataset where
        toJSON DescribeDataset'{..}
          = object
              (catMaybes [Just ("datasetArn" .= _ddDatasetARN)])

instance ToPath DescribeDataset where
        toPath = const "/"

instance ToQuery DescribeDataset where
        toQuery = const mempty

-- | /See:/ 'describeDatasetResponse' smart constructor.
data DescribeDatasetResponse = DescribeDatasetResponse'{_ddrsDataset
                                                        :: !(Maybe Dataset),
                                                        _ddrsResponseStatus ::
                                                        !Int}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'DescribeDatasetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsDataset' - A listing of the dataset's properties.
--
-- * 'ddrsResponseStatus' - -- | The response status code.
describeDatasetResponse
    :: Int -- ^ 'ddrsResponseStatus'
    -> DescribeDatasetResponse
describeDatasetResponse pResponseStatus_
  = DescribeDatasetResponse'{_ddrsDataset = Nothing,
                             _ddrsResponseStatus = pResponseStatus_}

-- | A listing of the dataset's properties.
ddrsDataset :: Lens' DescribeDatasetResponse (Maybe Dataset)
ddrsDataset = lens _ddrsDataset (\ s a -> s{_ddrsDataset = a})

-- | -- | The response status code.
ddrsResponseStatus :: Lens' DescribeDatasetResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\ s a -> s{_ddrsResponseStatus = a})

instance NFData DescribeDatasetResponse where
