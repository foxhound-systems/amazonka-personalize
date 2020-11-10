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
-- Module      : Network.AWS.AmazonPersonalize.ListSolutions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of solutions that use the given dataset group. When a dataset group is not specified, all the solutions associated with the account are listed. The response provides the properties for each solution, including the Amazon Resource Name (ARN). For more information on solutions, see 'CreateSolution' .
--
--
--
-- This operation returns paginated results.
module Network.AWS.AmazonPersonalize.ListSolutions
    (
    -- * Creating a Request
      listSolutions
    , ListSolutions
    -- * Request Lenses
    , lNextToken
    , lDatasetGroupARN
    , lMaxResults

    -- * Destructuring the Response
    , listSolutionsResponse
    , ListSolutionsResponse
    -- * Response Lenses
    , lrsNextToken
    , lrsSolutions
    , lrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSolutions' smart constructor.
data ListSolutions = ListSolutions'{_lNextToken ::
                                    !(Maybe Text),
                                    _lDatasetGroupARN :: !(Maybe Text),
                                    _lMaxResults :: !(Maybe Nat)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSolutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lNextToken' - A token returned from the previous call to @ListSolutions@ for getting the next set of solutions (if they exist).
--
-- * 'lDatasetGroupARN' - The Amazon Resource Name (ARN) of the dataset group.
--
-- * 'lMaxResults' - The maximum number of solutions to return.
listSolutions
    :: ListSolutions
listSolutions
  = ListSolutions'{_lNextToken = Nothing,
                   _lDatasetGroupARN = Nothing, _lMaxResults = Nothing}

-- | A token returned from the previous call to @ListSolutions@ for getting the next set of solutions (if they exist).
lNextToken :: Lens' ListSolutions (Maybe Text)
lNextToken = lens _lNextToken (\ s a -> s{_lNextToken = a})

-- | The Amazon Resource Name (ARN) of the dataset group.
lDatasetGroupARN :: Lens' ListSolutions (Maybe Text)
lDatasetGroupARN = lens _lDatasetGroupARN (\ s a -> s{_lDatasetGroupARN = a})

-- | The maximum number of solutions to return.
lMaxResults :: Lens' ListSolutions (Maybe Natural)
lMaxResults = lens _lMaxResults (\ s a -> s{_lMaxResults = a}) . mapping _Nat

instance AWSPager ListSolutions where
        page rq rs
          | stop (rs ^. lrsNextToken) = Nothing
          | stop (rs ^. lrsSolutions) = Nothing
          | otherwise =
            Just $ rq & lNextToken .~ rs ^. lrsNextToken

instance AWSRequest ListSolutions where
        type Rs ListSolutions = ListSolutionsResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 ListSolutionsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "solutions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListSolutions where

instance NFData ListSolutions where

instance ToHeaders ListSolutions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.ListSolutions" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListSolutions where
        toJSON ListSolutions'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lNextToken,
                  ("datasetGroupArn" .=) <$> _lDatasetGroupARN,
                  ("maxResults" .=) <$> _lMaxResults])

instance ToPath ListSolutions where
        toPath = const "/"

instance ToQuery ListSolutions where
        toQuery = const mempty

-- | /See:/ 'listSolutionsResponse' smart constructor.
data ListSolutionsResponse = ListSolutionsResponse'{_lrsNextToken
                                                    :: !(Maybe Text),
                                                    _lrsSolutions ::
                                                    !(Maybe [SolutionSummary]),
                                                    _lrsResponseStatus :: !Int}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'ListSolutionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsNextToken' - A token for getting the next set of solutions (if they exist).
--
-- * 'lrsSolutions' - A list of the current solutions.
--
-- * 'lrsResponseStatus' - -- | The response status code.
listSolutionsResponse
    :: Int -- ^ 'lrsResponseStatus'
    -> ListSolutionsResponse
listSolutionsResponse pResponseStatus_
  = ListSolutionsResponse'{_lrsNextToken = Nothing,
                           _lrsSolutions = Nothing,
                           _lrsResponseStatus = pResponseStatus_}

-- | A token for getting the next set of solutions (if they exist).
lrsNextToken :: Lens' ListSolutionsResponse (Maybe Text)
lrsNextToken = lens _lrsNextToken (\ s a -> s{_lrsNextToken = a})

-- | A list of the current solutions.
lrsSolutions :: Lens' ListSolutionsResponse [SolutionSummary]
lrsSolutions = lens _lrsSolutions (\ s a -> s{_lrsSolutions = a}) . _Default . _Coerce

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListSolutionsResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\ s a -> s{_lrsResponseStatus = a})

instance NFData ListSolutionsResponse where
