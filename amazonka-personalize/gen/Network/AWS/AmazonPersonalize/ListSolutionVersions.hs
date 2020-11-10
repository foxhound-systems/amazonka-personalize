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
-- Module      : Network.AWS.AmazonPersonalize.ListSolutionVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of solution versions for the given solution. When a solution is not specified, all the solution versions associated with the account are listed. The response provides the properties for each solution version, including the Amazon Resource Name (ARN). For more information on solutions, see 'CreateSolution' .
--
--
--
-- This operation returns paginated results.
module Network.AWS.AmazonPersonalize.ListSolutionVersions
    (
    -- * Creating a Request
      listSolutionVersions
    , ListSolutionVersions
    -- * Request Lenses
    , lsvSolutionARN
    , lsvNextToken
    , lsvMaxResults

    -- * Destructuring the Response
    , listSolutionVersionsResponse
    , ListSolutionVersionsResponse
    -- * Response Lenses
    , lsvrsNextToken
    , lsvrsSolutionVersions
    , lsvrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSolutionVersions' smart constructor.
data ListSolutionVersions = ListSolutionVersions'{_lsvSolutionARN
                                                  :: !(Maybe Text),
                                                  _lsvNextToken ::
                                                  !(Maybe Text),
                                                  _lsvMaxResults ::
                                                  !(Maybe Nat)}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSolutionVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsvSolutionARN' - The Amazon Resource Name (ARN) of the solution.
--
-- * 'lsvNextToken' - A token returned from the previous call to @ListSolutionVersions@ for getting the next set of solution versions (if they exist).
--
-- * 'lsvMaxResults' - The maximum number of solution versions to return.
listSolutionVersions
    :: ListSolutionVersions
listSolutionVersions
  = ListSolutionVersions'{_lsvSolutionARN = Nothing,
                          _lsvNextToken = Nothing, _lsvMaxResults = Nothing}

-- | The Amazon Resource Name (ARN) of the solution.
lsvSolutionARN :: Lens' ListSolutionVersions (Maybe Text)
lsvSolutionARN = lens _lsvSolutionARN (\ s a -> s{_lsvSolutionARN = a})

-- | A token returned from the previous call to @ListSolutionVersions@ for getting the next set of solution versions (if they exist).
lsvNextToken :: Lens' ListSolutionVersions (Maybe Text)
lsvNextToken = lens _lsvNextToken (\ s a -> s{_lsvNextToken = a})

-- | The maximum number of solution versions to return.
lsvMaxResults :: Lens' ListSolutionVersions (Maybe Natural)
lsvMaxResults = lens _lsvMaxResults (\ s a -> s{_lsvMaxResults = a}) . mapping _Nat

instance AWSPager ListSolutionVersions where
        page rq rs
          | stop (rs ^. lsvrsNextToken) = Nothing
          | stop (rs ^. lsvrsSolutionVersions) = Nothing
          | otherwise =
            Just $ rq & lsvNextToken .~ rs ^. lsvrsNextToken

instance AWSRequest ListSolutionVersions where
        type Rs ListSolutionVersions =
             ListSolutionVersionsResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 ListSolutionVersionsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "solutionVersions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListSolutionVersions where

instance NFData ListSolutionVersions where

instance ToHeaders ListSolutionVersions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.ListSolutionVersions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListSolutionVersions where
        toJSON ListSolutionVersions'{..}
          = object
              (catMaybes
                 [("solutionArn" .=) <$> _lsvSolutionARN,
                  ("nextToken" .=) <$> _lsvNextToken,
                  ("maxResults" .=) <$> _lsvMaxResults])

instance ToPath ListSolutionVersions where
        toPath = const "/"

instance ToQuery ListSolutionVersions where
        toQuery = const mempty

-- | /See:/ 'listSolutionVersionsResponse' smart constructor.
data ListSolutionVersionsResponse = ListSolutionVersionsResponse'{_lsvrsNextToken
                                                                  ::
                                                                  !(Maybe Text),
                                                                  _lsvrsSolutionVersions
                                                                  ::
                                                                  !(Maybe
                                                                      [SolutionVersionSummary]),
                                                                  _lsvrsResponseStatus
                                                                  :: !Int}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'ListSolutionVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsvrsNextToken' - A token for getting the next set of solution versions (if they exist).
--
-- * 'lsvrsSolutionVersions' - A list of solution versions describing the version properties.
--
-- * 'lsvrsResponseStatus' - -- | The response status code.
listSolutionVersionsResponse
    :: Int -- ^ 'lsvrsResponseStatus'
    -> ListSolutionVersionsResponse
listSolutionVersionsResponse pResponseStatus_
  = ListSolutionVersionsResponse'{_lsvrsNextToken =
                                    Nothing,
                                  _lsvrsSolutionVersions = Nothing,
                                  _lsvrsResponseStatus = pResponseStatus_}

-- | A token for getting the next set of solution versions (if they exist).
lsvrsNextToken :: Lens' ListSolutionVersionsResponse (Maybe Text)
lsvrsNextToken = lens _lsvrsNextToken (\ s a -> s{_lsvrsNextToken = a})

-- | A list of solution versions describing the version properties.
lsvrsSolutionVersions :: Lens' ListSolutionVersionsResponse [SolutionVersionSummary]
lsvrsSolutionVersions = lens _lsvrsSolutionVersions (\ s a -> s{_lsvrsSolutionVersions = a}) . _Default . _Coerce

-- | -- | The response status code.
lsvrsResponseStatus :: Lens' ListSolutionVersionsResponse Int
lsvrsResponseStatus = lens _lsvrsResponseStatus (\ s a -> s{_lsvrsResponseStatus = a})

instance NFData ListSolutionVersionsResponse where
