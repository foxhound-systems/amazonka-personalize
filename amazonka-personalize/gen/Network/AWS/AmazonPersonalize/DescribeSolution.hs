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
-- Module      : Network.AWS.AmazonPersonalize.DescribeSolution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a solution. For more information on solutions, see 'CreateSolution' .
--
--
module Network.AWS.AmazonPersonalize.DescribeSolution
    (
    -- * Creating a Request
      describeSolution
    , DescribeSolution
    -- * Request Lenses
    , dsSolutionARN

    -- * Destructuring the Response
    , describeSolutionResponse
    , DescribeSolutionResponse
    -- * Response Lenses
    , dsrsSolution
    , dsrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeSolution' smart constructor.
newtype DescribeSolution = DescribeSolution'{_dsSolutionARN
                                             :: Text}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeSolution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsSolutionARN' - The Amazon Resource Name (ARN) of the solution to describe.
describeSolution
    :: Text -- ^ 'dsSolutionARN'
    -> DescribeSolution
describeSolution pSolutionARN_
  = DescribeSolution'{_dsSolutionARN = pSolutionARN_}

-- | The Amazon Resource Name (ARN) of the solution to describe.
dsSolutionARN :: Lens' DescribeSolution Text
dsSolutionARN = lens _dsSolutionARN (\ s a -> s{_dsSolutionARN = a})

instance AWSRequest DescribeSolution where
        type Rs DescribeSolution = DescribeSolutionResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSolutionResponse' <$>
                   (x .?> "solution") <*> (pure (fromEnum s)))

instance Hashable DescribeSolution where

instance NFData DescribeSolution where

instance ToHeaders DescribeSolution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DescribeSolution" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeSolution where
        toJSON DescribeSolution'{..}
          = object
              (catMaybes [Just ("solutionArn" .= _dsSolutionARN)])

instance ToPath DescribeSolution where
        toPath = const "/"

instance ToQuery DescribeSolution where
        toQuery = const mempty

-- | /See:/ 'describeSolutionResponse' smart constructor.
data DescribeSolutionResponse = DescribeSolutionResponse'{_dsrsSolution
                                                          :: !(Maybe Solution),
                                                          _dsrsResponseStatus ::
                                                          !Int}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'DescribeSolutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsSolution' - An object that describes the solution.
--
-- * 'dsrsResponseStatus' - -- | The response status code.
describeSolutionResponse
    :: Int -- ^ 'dsrsResponseStatus'
    -> DescribeSolutionResponse
describeSolutionResponse pResponseStatus_
  = DescribeSolutionResponse'{_dsrsSolution = Nothing,
                              _dsrsResponseStatus = pResponseStatus_}

-- | An object that describes the solution.
dsrsSolution :: Lens' DescribeSolutionResponse (Maybe Solution)
dsrsSolution = lens _dsrsSolution (\ s a -> s{_dsrsSolution = a})

-- | -- | The response status code.
dsrsResponseStatus :: Lens' DescribeSolutionResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\ s a -> s{_dsrsResponseStatus = a})

instance NFData DescribeSolutionResponse where
