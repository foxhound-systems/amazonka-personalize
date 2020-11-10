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
-- Module      : Network.AWS.AmazonPersonalize.DescribeSolutionVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a specific version of a solution. For more information on solutions, see 'CreateSolution' .
--
--
module Network.AWS.AmazonPersonalize.DescribeSolutionVersion
    (
    -- * Creating a Request
      describeSolutionVersion
    , DescribeSolutionVersion
    -- * Request Lenses
    , dsvSolutionVersionARN

    -- * Destructuring the Response
    , describeSolutionVersionResponse
    , DescribeSolutionVersionResponse
    -- * Response Lenses
    , dsvrsSolutionVersion
    , dsvrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeSolutionVersion' smart constructor.
newtype DescribeSolutionVersion = DescribeSolutionVersion'{_dsvSolutionVersionARN
                                                           :: Text}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'DescribeSolutionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsvSolutionVersionARN' - The Amazon Resource Name (ARN) of the solution version.
describeSolutionVersion
    :: Text -- ^ 'dsvSolutionVersionARN'
    -> DescribeSolutionVersion
describeSolutionVersion pSolutionVersionARN_
  = DescribeSolutionVersion'{_dsvSolutionVersionARN =
                               pSolutionVersionARN_}

-- | The Amazon Resource Name (ARN) of the solution version.
dsvSolutionVersionARN :: Lens' DescribeSolutionVersion Text
dsvSolutionVersionARN = lens _dsvSolutionVersionARN (\ s a -> s{_dsvSolutionVersionARN = a})

instance AWSRequest DescribeSolutionVersion where
        type Rs DescribeSolutionVersion =
             DescribeSolutionVersionResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSolutionVersionResponse' <$>
                   (x .?> "solutionVersion") <*> (pure (fromEnum s)))

instance Hashable DescribeSolutionVersion where

instance NFData DescribeSolutionVersion where

instance ToHeaders DescribeSolutionVersion where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DescribeSolutionVersion" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeSolutionVersion where
        toJSON DescribeSolutionVersion'{..}
          = object
              (catMaybes
                 [Just
                    ("solutionVersionArn" .= _dsvSolutionVersionARN)])

instance ToPath DescribeSolutionVersion where
        toPath = const "/"

instance ToQuery DescribeSolutionVersion where
        toQuery = const mempty

-- | /See:/ 'describeSolutionVersionResponse' smart constructor.
data DescribeSolutionVersionResponse = DescribeSolutionVersionResponse'{_dsvrsSolutionVersion
                                                                        ::
                                                                        !(Maybe
                                                                            SolutionVersion),
                                                                        _dsvrsResponseStatus
                                                                        :: !Int}
                                         deriving (Eq, Read, Show, Data,
                                                   Typeable, Generic)

-- | Creates a value of 'DescribeSolutionVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsvrsSolutionVersion' - The solution version.
--
-- * 'dsvrsResponseStatus' - -- | The response status code.
describeSolutionVersionResponse
    :: Int -- ^ 'dsvrsResponseStatus'
    -> DescribeSolutionVersionResponse
describeSolutionVersionResponse pResponseStatus_
  = DescribeSolutionVersionResponse'{_dsvrsSolutionVersion
                                       = Nothing,
                                     _dsvrsResponseStatus = pResponseStatus_}

-- | The solution version.
dsvrsSolutionVersion :: Lens' DescribeSolutionVersionResponse (Maybe SolutionVersion)
dsvrsSolutionVersion = lens _dsvrsSolutionVersion (\ s a -> s{_dsvrsSolutionVersion = a})

-- | -- | The response status code.
dsvrsResponseStatus :: Lens' DescribeSolutionVersionResponse Int
dsvrsResponseStatus = lens _dsvrsResponseStatus (\ s a -> s{_dsvrsResponseStatus = a})

instance NFData DescribeSolutionVersionResponse where
