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
-- Module      : Network.AWS.AmazonPersonalize.ListEventTrackers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of event trackers associated with the account. The response provides the properties for each event tracker, including the Amazon Resource Name (ARN) and tracking ID. For more information on event trackers, see 'CreateEventTracker' .
--
--
--
-- This operation returns paginated results.
module Network.AWS.AmazonPersonalize.ListEventTrackers
    (
    -- * Creating a Request
      listEventTrackers
    , ListEventTrackers
    -- * Request Lenses
    , letNextToken
    , letDatasetGroupARN
    , letMaxResults

    -- * Destructuring the Response
    , listEventTrackersResponse
    , ListEventTrackersResponse
    -- * Response Lenses
    , letrsEventTrackers
    , letrsNextToken
    , letrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listEventTrackers' smart constructor.
data ListEventTrackers = ListEventTrackers'{_letNextToken
                                            :: !(Maybe Text),
                                            _letDatasetGroupARN ::
                                            !(Maybe Text),
                                            _letMaxResults :: !(Maybe Nat)}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListEventTrackers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'letNextToken' - A token returned from the previous call to @ListEventTrackers@ for getting the next set of event trackers (if they exist).
--
-- * 'letDatasetGroupARN' - The ARN of a dataset group used to filter the response.
--
-- * 'letMaxResults' - The maximum number of event trackers to return.
listEventTrackers
    :: ListEventTrackers
listEventTrackers
  = ListEventTrackers'{_letNextToken = Nothing,
                       _letDatasetGroupARN = Nothing,
                       _letMaxResults = Nothing}

-- | A token returned from the previous call to @ListEventTrackers@ for getting the next set of event trackers (if they exist).
letNextToken :: Lens' ListEventTrackers (Maybe Text)
letNextToken = lens _letNextToken (\ s a -> s{_letNextToken = a})

-- | The ARN of a dataset group used to filter the response.
letDatasetGroupARN :: Lens' ListEventTrackers (Maybe Text)
letDatasetGroupARN = lens _letDatasetGroupARN (\ s a -> s{_letDatasetGroupARN = a})

-- | The maximum number of event trackers to return.
letMaxResults :: Lens' ListEventTrackers (Maybe Natural)
letMaxResults = lens _letMaxResults (\ s a -> s{_letMaxResults = a}) . mapping _Nat

instance AWSPager ListEventTrackers where
        page rq rs
          | stop (rs ^. letrsNextToken) = Nothing
          | stop (rs ^. letrsEventTrackers) = Nothing
          | otherwise =
            Just $ rq & letNextToken .~ rs ^. letrsNextToken

instance AWSRequest ListEventTrackers where
        type Rs ListEventTrackers = ListEventTrackersResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 ListEventTrackersResponse' <$>
                   (x .?> "eventTrackers" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListEventTrackers where

instance NFData ListEventTrackers where

instance ToHeaders ListEventTrackers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.ListEventTrackers" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListEventTrackers where
        toJSON ListEventTrackers'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _letNextToken,
                  ("datasetGroupArn" .=) <$> _letDatasetGroupARN,
                  ("maxResults" .=) <$> _letMaxResults])

instance ToPath ListEventTrackers where
        toPath = const "/"

instance ToQuery ListEventTrackers where
        toQuery = const mempty

-- | /See:/ 'listEventTrackersResponse' smart constructor.
data ListEventTrackersResponse = ListEventTrackersResponse'{_letrsEventTrackers
                                                            ::
                                                            !(Maybe
                                                                [EventTrackerSummary]),
                                                            _letrsNextToken ::
                                                            !(Maybe Text),
                                                            _letrsResponseStatus
                                                            :: !Int}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'ListEventTrackersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'letrsEventTrackers' - A list of event trackers.
--
-- * 'letrsNextToken' - A token for getting the next set of event trackers (if they exist).
--
-- * 'letrsResponseStatus' - -- | The response status code.
listEventTrackersResponse
    :: Int -- ^ 'letrsResponseStatus'
    -> ListEventTrackersResponse
listEventTrackersResponse pResponseStatus_
  = ListEventTrackersResponse'{_letrsEventTrackers =
                                 Nothing,
                               _letrsNextToken = Nothing,
                               _letrsResponseStatus = pResponseStatus_}

-- | A list of event trackers.
letrsEventTrackers :: Lens' ListEventTrackersResponse [EventTrackerSummary]
letrsEventTrackers = lens _letrsEventTrackers (\ s a -> s{_letrsEventTrackers = a}) . _Default . _Coerce

-- | A token for getting the next set of event trackers (if they exist).
letrsNextToken :: Lens' ListEventTrackersResponse (Maybe Text)
letrsNextToken = lens _letrsNextToken (\ s a -> s{_letrsNextToken = a})

-- | -- | The response status code.
letrsResponseStatus :: Lens' ListEventTrackersResponse Int
letrsResponseStatus = lens _letrsResponseStatus (\ s a -> s{_letrsResponseStatus = a})

instance NFData ListEventTrackersResponse where
