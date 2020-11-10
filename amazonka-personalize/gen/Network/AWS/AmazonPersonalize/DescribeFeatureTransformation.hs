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
-- Module      : Network.AWS.AmazonPersonalize.DescribeFeatureTransformation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the given feature transformation.
--
--
module Network.AWS.AmazonPersonalize.DescribeFeatureTransformation
    (
    -- * Creating a Request
      describeFeatureTransformation
    , DescribeFeatureTransformation
    -- * Request Lenses
    , dftFeatureTransformationARN

    -- * Destructuring the Response
    , describeFeatureTransformationResponse
    , DescribeFeatureTransformationResponse
    -- * Response Lenses
    , dftrsFeatureTransformation
    , dftrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeFeatureTransformation' smart constructor.
newtype DescribeFeatureTransformation = DescribeFeatureTransformation'{_dftFeatureTransformationARN
                                                                       :: Text}
                                          deriving (Eq, Read, Show, Data,
                                                    Typeable, Generic)

-- | Creates a value of 'DescribeFeatureTransformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dftFeatureTransformationARN' - The Amazon Resource Name (ARN) of the feature transformation to describe.
describeFeatureTransformation
    :: Text -- ^ 'dftFeatureTransformationARN'
    -> DescribeFeatureTransformation
describeFeatureTransformation
  pFeatureTransformationARN_
  = DescribeFeatureTransformation'{_dftFeatureTransformationARN
                                     = pFeatureTransformationARN_}

-- | The Amazon Resource Name (ARN) of the feature transformation to describe.
dftFeatureTransformationARN :: Lens' DescribeFeatureTransformation Text
dftFeatureTransformationARN = lens _dftFeatureTransformationARN (\ s a -> s{_dftFeatureTransformationARN = a})

instance AWSRequest DescribeFeatureTransformation
         where
        type Rs DescribeFeatureTransformation =
             DescribeFeatureTransformationResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 DescribeFeatureTransformationResponse' <$>
                   (x .?> "featureTransformation") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeFeatureTransformation where

instance NFData DescribeFeatureTransformation where

instance ToHeaders DescribeFeatureTransformation
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DescribeFeatureTransformation" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeFeatureTransformation where
        toJSON DescribeFeatureTransformation'{..}
          = object
              (catMaybes
                 [Just
                    ("featureTransformationArn" .=
                       _dftFeatureTransformationARN)])

instance ToPath DescribeFeatureTransformation where
        toPath = const "/"

instance ToQuery DescribeFeatureTransformation where
        toQuery = const mempty

-- | /See:/ 'describeFeatureTransformationResponse' smart constructor.
data DescribeFeatureTransformationResponse = DescribeFeatureTransformationResponse'{_dftrsFeatureTransformation
                                                                                    ::
                                                                                    !(Maybe
                                                                                        FeatureTransformation),
                                                                                    _dftrsResponseStatus
                                                                                    ::
                                                                                    !Int}
                                               deriving (Eq, Read, Show, Data,
                                                         Typeable, Generic)

-- | Creates a value of 'DescribeFeatureTransformationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dftrsFeatureTransformation' - A listing of the FeatureTransformation properties.
--
-- * 'dftrsResponseStatus' - -- | The response status code.
describeFeatureTransformationResponse
    :: Int -- ^ 'dftrsResponseStatus'
    -> DescribeFeatureTransformationResponse
describeFeatureTransformationResponse
  pResponseStatus_
  = DescribeFeatureTransformationResponse'{_dftrsFeatureTransformation
                                             = Nothing,
                                           _dftrsResponseStatus =
                                             pResponseStatus_}

-- | A listing of the FeatureTransformation properties.
dftrsFeatureTransformation :: Lens' DescribeFeatureTransformationResponse (Maybe FeatureTransformation)
dftrsFeatureTransformation = lens _dftrsFeatureTransformation (\ s a -> s{_dftrsFeatureTransformation = a})

-- | -- | The response status code.
dftrsResponseStatus :: Lens' DescribeFeatureTransformationResponse Int
dftrsResponseStatus = lens _dftrsResponseStatus (\ s a -> s{_dftrsResponseStatus = a})

instance NFData DescribeFeatureTransformationResponse
         where
