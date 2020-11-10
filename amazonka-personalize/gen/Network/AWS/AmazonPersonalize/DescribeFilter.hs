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
-- Module      : Network.AWS.AmazonPersonalize.DescribeFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a filter's properties.
--
--
module Network.AWS.AmazonPersonalize.DescribeFilter
    (
    -- * Creating a Request
      describeFilter
    , DescribeFilter
    -- * Request Lenses
    , dfFilterARN

    -- * Destructuring the Response
    , describeFilterResponse
    , DescribeFilterResponse
    -- * Response Lenses
    , dfrsFilter
    , dfrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeFilter' smart constructor.
newtype DescribeFilter = DescribeFilter'{_dfFilterARN
                                         :: Text}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfFilterARN' - The ARN of the filter to describe.
describeFilter
    :: Text -- ^ 'dfFilterARN'
    -> DescribeFilter
describeFilter pFilterARN_
  = DescribeFilter'{_dfFilterARN = pFilterARN_}

-- | The ARN of the filter to describe.
dfFilterARN :: Lens' DescribeFilter Text
dfFilterARN = lens _dfFilterARN (\ s a -> s{_dfFilterARN = a})

instance AWSRequest DescribeFilter where
        type Rs DescribeFilter = DescribeFilterResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 DescribeFilterResponse' <$>
                   (x .?> "filter") <*> (pure (fromEnum s)))

instance Hashable DescribeFilter where

instance NFData DescribeFilter where

instance ToHeaders DescribeFilter where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DescribeFilter" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeFilter where
        toJSON DescribeFilter'{..}
          = object
              (catMaybes [Just ("filterArn" .= _dfFilterARN)])

instance ToPath DescribeFilter where
        toPath = const "/"

instance ToQuery DescribeFilter where
        toQuery = const mempty

-- | /See:/ 'describeFilterResponse' smart constructor.
data DescribeFilterResponse = DescribeFilterResponse'{_dfrsFilter
                                                      :: !(Maybe Filter),
                                                      _dfrsResponseStatus ::
                                                      !Int}
                                deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFilterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfrsFilter' - The filter's details.
--
-- * 'dfrsResponseStatus' - -- | The response status code.
describeFilterResponse
    :: Int -- ^ 'dfrsResponseStatus'
    -> DescribeFilterResponse
describeFilterResponse pResponseStatus_
  = DescribeFilterResponse'{_dfrsFilter = Nothing,
                            _dfrsResponseStatus = pResponseStatus_}

-- | The filter's details.
dfrsFilter :: Lens' DescribeFilterResponse (Maybe Filter)
dfrsFilter = lens _dfrsFilter (\ s a -> s{_dfrsFilter = a})

-- | -- | The response status code.
dfrsResponseStatus :: Lens' DescribeFilterResponse Int
dfrsResponseStatus = lens _dfrsResponseStatus (\ s a -> s{_dfrsResponseStatus = a})

instance NFData DescribeFilterResponse where
