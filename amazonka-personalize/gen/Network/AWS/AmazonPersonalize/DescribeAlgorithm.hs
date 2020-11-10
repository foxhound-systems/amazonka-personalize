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
-- Module      : Network.AWS.AmazonPersonalize.DescribeAlgorithm
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the given algorithm.
--
--
module Network.AWS.AmazonPersonalize.DescribeAlgorithm
    (
    -- * Creating a Request
      describeAlgorithm
    , DescribeAlgorithm
    -- * Request Lenses
    , daAlgorithmARN

    -- * Destructuring the Response
    , describeAlgorithmResponse
    , DescribeAlgorithmResponse
    -- * Response Lenses
    , darsAlgorithm
    , darsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAlgorithm' smart constructor.
newtype DescribeAlgorithm = DescribeAlgorithm'{_daAlgorithmARN
                                               :: Text}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAlgorithm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAlgorithmARN' - The Amazon Resource Name (ARN) of the algorithm to describe.
describeAlgorithm
    :: Text -- ^ 'daAlgorithmARN'
    -> DescribeAlgorithm
describeAlgorithm pAlgorithmARN_
  = DescribeAlgorithm'{_daAlgorithmARN =
                         pAlgorithmARN_}

-- | The Amazon Resource Name (ARN) of the algorithm to describe.
daAlgorithmARN :: Lens' DescribeAlgorithm Text
daAlgorithmARN = lens _daAlgorithmARN (\ s a -> s{_daAlgorithmARN = a})

instance AWSRequest DescribeAlgorithm where
        type Rs DescribeAlgorithm = DescribeAlgorithmResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAlgorithmResponse' <$>
                   (x .?> "algorithm") <*> (pure (fromEnum s)))

instance Hashable DescribeAlgorithm where

instance NFData DescribeAlgorithm where

instance ToHeaders DescribeAlgorithm where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DescribeAlgorithm" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAlgorithm where
        toJSON DescribeAlgorithm'{..}
          = object
              (catMaybes
                 [Just ("algorithmArn" .= _daAlgorithmARN)])

instance ToPath DescribeAlgorithm where
        toPath = const "/"

instance ToQuery DescribeAlgorithm where
        toQuery = const mempty

-- | /See:/ 'describeAlgorithmResponse' smart constructor.
data DescribeAlgorithmResponse = DescribeAlgorithmResponse'{_darsAlgorithm
                                                            ::
                                                            !(Maybe Algorithm),
                                                            _darsResponseStatus
                                                            :: !Int}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'DescribeAlgorithmResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsAlgorithm' - A listing of the properties of the algorithm.
--
-- * 'darsResponseStatus' - -- | The response status code.
describeAlgorithmResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DescribeAlgorithmResponse
describeAlgorithmResponse pResponseStatus_
  = DescribeAlgorithmResponse'{_darsAlgorithm =
                                 Nothing,
                               _darsResponseStatus = pResponseStatus_}

-- | A listing of the properties of the algorithm.
darsAlgorithm :: Lens' DescribeAlgorithmResponse (Maybe Algorithm)
darsAlgorithm = lens _darsAlgorithm (\ s a -> s{_darsAlgorithm = a})

-- | -- | The response status code.
darsResponseStatus :: Lens' DescribeAlgorithmResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

instance NFData DescribeAlgorithmResponse where
