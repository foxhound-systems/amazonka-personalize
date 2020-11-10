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
-- Module      : Network.AWS.AmazonPersonalize.GetSolutionMetrics
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the metrics for the specified solution version.
--
--
module Network.AWS.AmazonPersonalize.GetSolutionMetrics
    (
    -- * Creating a Request
      getSolutionMetrics
    , GetSolutionMetrics
    -- * Request Lenses
    , gsmSolutionVersionARN

    -- * Destructuring the Response
    , getSolutionMetricsResponse
    , GetSolutionMetricsResponse
    -- * Response Lenses
    , gsmrsMetrics
    , gsmrsSolutionVersionARN
    , gsmrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSolutionMetrics' smart constructor.
newtype GetSolutionMetrics = GetSolutionMetrics'{_gsmSolutionVersionARN
                                                 :: Text}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'GetSolutionMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsmSolutionVersionARN' - The Amazon Resource Name (ARN) of the solution version for which to get metrics.
getSolutionMetrics
    :: Text -- ^ 'gsmSolutionVersionARN'
    -> GetSolutionMetrics
getSolutionMetrics pSolutionVersionARN_
  = GetSolutionMetrics'{_gsmSolutionVersionARN =
                          pSolutionVersionARN_}

-- | The Amazon Resource Name (ARN) of the solution version for which to get metrics.
gsmSolutionVersionARN :: Lens' GetSolutionMetrics Text
gsmSolutionVersionARN = lens _gsmSolutionVersionARN (\ s a -> s{_gsmSolutionVersionARN = a})

instance AWSRequest GetSolutionMetrics where
        type Rs GetSolutionMetrics =
             GetSolutionMetricsResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 GetSolutionMetricsResponse' <$>
                   (x .?> "metrics" .!@ mempty) <*>
                     (x .?> "solutionVersionArn")
                     <*> (pure (fromEnum s)))

instance Hashable GetSolutionMetrics where

instance NFData GetSolutionMetrics where

instance ToHeaders GetSolutionMetrics where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.GetSolutionMetrics" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetSolutionMetrics where
        toJSON GetSolutionMetrics'{..}
          = object
              (catMaybes
                 [Just
                    ("solutionVersionArn" .= _gsmSolutionVersionARN)])

instance ToPath GetSolutionMetrics where
        toPath = const "/"

instance ToQuery GetSolutionMetrics where
        toQuery = const mempty

-- | /See:/ 'getSolutionMetricsResponse' smart constructor.
data GetSolutionMetricsResponse = GetSolutionMetricsResponse'{_gsmrsMetrics
                                                              ::
                                                              !(Maybe
                                                                  (Map Text
                                                                     Double)),
                                                              _gsmrsSolutionVersionARN
                                                              :: !(Maybe Text),
                                                              _gsmrsResponseStatus
                                                              :: !Int}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'GetSolutionMetricsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsmrsMetrics' - The metrics for the solution version.
--
-- * 'gsmrsSolutionVersionARN' - The same solution version ARN as specified in the request.
--
-- * 'gsmrsResponseStatus' - -- | The response status code.
getSolutionMetricsResponse
    :: Int -- ^ 'gsmrsResponseStatus'
    -> GetSolutionMetricsResponse
getSolutionMetricsResponse pResponseStatus_
  = GetSolutionMetricsResponse'{_gsmrsMetrics =
                                  Nothing,
                                _gsmrsSolutionVersionARN = Nothing,
                                _gsmrsResponseStatus = pResponseStatus_}

-- | The metrics for the solution version.
gsmrsMetrics :: Lens' GetSolutionMetricsResponse (HashMap Text Double)
gsmrsMetrics = lens _gsmrsMetrics (\ s a -> s{_gsmrsMetrics = a}) . _Default . _Map

-- | The same solution version ARN as specified in the request.
gsmrsSolutionVersionARN :: Lens' GetSolutionMetricsResponse (Maybe Text)
gsmrsSolutionVersionARN = lens _gsmrsSolutionVersionARN (\ s a -> s{_gsmrsSolutionVersionARN = a})

-- | -- | The response status code.
gsmrsResponseStatus :: Lens' GetSolutionMetricsResponse Int
gsmrsResponseStatus = lens _gsmrsResponseStatus (\ s a -> s{_gsmrsResponseStatus = a})

instance NFData GetSolutionMetricsResponse where
