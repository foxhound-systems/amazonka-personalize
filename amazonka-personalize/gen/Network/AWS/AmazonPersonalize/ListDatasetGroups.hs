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
-- Module      : Network.AWS.AmazonPersonalize.ListDatasetGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of dataset groups. The response provides the properties for each dataset group, including the Amazon Resource Name (ARN). For more information on dataset groups, see 'CreateDatasetGroup' .
--
--
--
-- This operation returns paginated results.
module Network.AWS.AmazonPersonalize.ListDatasetGroups
    (
    -- * Creating a Request
      listDatasetGroups
    , ListDatasetGroups
    -- * Request Lenses
    , ldgNextToken
    , ldgMaxResults

    -- * Destructuring the Response
    , listDatasetGroupsResponse
    , ListDatasetGroupsResponse
    -- * Response Lenses
    , ldgrsNextToken
    , ldgrsDatasetGroups
    , ldgrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDatasetGroups' smart constructor.
data ListDatasetGroups = ListDatasetGroups'{_ldgNextToken
                                            :: !(Maybe Text),
                                            _ldgMaxResults :: !(Maybe Nat)}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDatasetGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldgNextToken' - A token returned from the previous call to @ListDatasetGroups@ for getting the next set of dataset groups (if they exist).
--
-- * 'ldgMaxResults' - The maximum number of dataset groups to return.
listDatasetGroups
    :: ListDatasetGroups
listDatasetGroups
  = ListDatasetGroups'{_ldgNextToken = Nothing,
                       _ldgMaxResults = Nothing}

-- | A token returned from the previous call to @ListDatasetGroups@ for getting the next set of dataset groups (if they exist).
ldgNextToken :: Lens' ListDatasetGroups (Maybe Text)
ldgNextToken = lens _ldgNextToken (\ s a -> s{_ldgNextToken = a})

-- | The maximum number of dataset groups to return.
ldgMaxResults :: Lens' ListDatasetGroups (Maybe Natural)
ldgMaxResults = lens _ldgMaxResults (\ s a -> s{_ldgMaxResults = a}) . mapping _Nat

instance AWSPager ListDatasetGroups where
        page rq rs
          | stop (rs ^. ldgrsNextToken) = Nothing
          | stop (rs ^. ldgrsDatasetGroups) = Nothing
          | otherwise =
            Just $ rq & ldgNextToken .~ rs ^. ldgrsNextToken

instance AWSRequest ListDatasetGroups where
        type Rs ListDatasetGroups = ListDatasetGroupsResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 ListDatasetGroupsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "datasetGroups" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListDatasetGroups where

instance NFData ListDatasetGroups where

instance ToHeaders ListDatasetGroups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.ListDatasetGroups" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDatasetGroups where
        toJSON ListDatasetGroups'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _ldgNextToken,
                  ("maxResults" .=) <$> _ldgMaxResults])

instance ToPath ListDatasetGroups where
        toPath = const "/"

instance ToQuery ListDatasetGroups where
        toQuery = const mempty

-- | /See:/ 'listDatasetGroupsResponse' smart constructor.
data ListDatasetGroupsResponse = ListDatasetGroupsResponse'{_ldgrsNextToken
                                                            :: !(Maybe Text),
                                                            _ldgrsDatasetGroups
                                                            ::
                                                            !(Maybe
                                                                [DatasetGroupSummary]),
                                                            _ldgrsResponseStatus
                                                            :: !Int}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'ListDatasetGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldgrsNextToken' - A token for getting the next set of dataset groups (if they exist).
--
-- * 'ldgrsDatasetGroups' - The list of your dataset groups.
--
-- * 'ldgrsResponseStatus' - -- | The response status code.
listDatasetGroupsResponse
    :: Int -- ^ 'ldgrsResponseStatus'
    -> ListDatasetGroupsResponse
listDatasetGroupsResponse pResponseStatus_
  = ListDatasetGroupsResponse'{_ldgrsNextToken =
                                 Nothing,
                               _ldgrsDatasetGroups = Nothing,
                               _ldgrsResponseStatus = pResponseStatus_}

-- | A token for getting the next set of dataset groups (if they exist).
ldgrsNextToken :: Lens' ListDatasetGroupsResponse (Maybe Text)
ldgrsNextToken = lens _ldgrsNextToken (\ s a -> s{_ldgrsNextToken = a})

-- | The list of your dataset groups.
ldgrsDatasetGroups :: Lens' ListDatasetGroupsResponse [DatasetGroupSummary]
ldgrsDatasetGroups = lens _ldgrsDatasetGroups (\ s a -> s{_ldgrsDatasetGroups = a}) . _Default . _Coerce

-- | -- | The response status code.
ldgrsResponseStatus :: Lens' ListDatasetGroupsResponse Int
ldgrsResponseStatus = lens _ldgrsResponseStatus (\ s a -> s{_ldgrsResponseStatus = a})

instance NFData ListDatasetGroupsResponse where
