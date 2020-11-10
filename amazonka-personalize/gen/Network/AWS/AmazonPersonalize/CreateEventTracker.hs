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
-- Module      : Network.AWS.AmazonPersonalize.CreateEventTracker
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an event tracker that you use when sending event data to the specified dataset group using the <https://docs.aws.amazon.com/personalize/latest/dg/API_UBS_PutEvents.html PutEvents> API.
--
--
-- When Amazon Personalize creates an event tracker, it also creates an /event-interactions/ dataset in the dataset group associated with the event tracker. The event-interactions dataset stores the event data from the @PutEvents@ call. The contents of this dataset are not available to the user.
--
-- When you send event data you include your tracking ID. The tracking ID identifies the customer and authorizes the customer to send the data.
--
-- The event tracker can be in one of the following states:
--
--     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
--     * DELETE PENDING > DELETE IN_PROGRESS
--
--
--
-- To get the status of the event tracker, call 'DescribeEventTracker' .
--
-- __Related APIs__ 
--
--     * 'ListEventTrackers' 
--
--     * 'DescribeEventTracker' 
--
--     * 'DeleteEventTracker' 
--
--
--
module Network.AWS.AmazonPersonalize.CreateEventTracker
    (
    -- * Creating a Request
      createEventTracker
    , CreateEventTracker
    -- * Request Lenses
    , cetName
    , cetDatasetGroupARN

    -- * Destructuring the Response
    , createEventTrackerResponse
    , CreateEventTrackerResponse
    -- * Response Lenses
    , cetrsTrackingId
    , cetrsEventTrackerARN
    , cetrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createEventTracker' smart constructor.
data CreateEventTracker = CreateEventTracker'{_cetName
                                              :: !Text,
                                              _cetDatasetGroupARN :: !Text}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateEventTracker' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cetName' - The name for the event tracker.
--
-- * 'cetDatasetGroupARN' - The Amazon Resource Name (ARN) of the dataset group that receives the event data.
createEventTracker
    :: Text -- ^ 'cetName'
    -> Text -- ^ 'cetDatasetGroupARN'
    -> CreateEventTracker
createEventTracker pName_ pDatasetGroupARN_
  = CreateEventTracker'{_cetName = pName_,
                        _cetDatasetGroupARN = pDatasetGroupARN_}

-- | The name for the event tracker.
cetName :: Lens' CreateEventTracker Text
cetName = lens _cetName (\ s a -> s{_cetName = a})

-- | The Amazon Resource Name (ARN) of the dataset group that receives the event data.
cetDatasetGroupARN :: Lens' CreateEventTracker Text
cetDatasetGroupARN = lens _cetDatasetGroupARN (\ s a -> s{_cetDatasetGroupARN = a})

instance AWSRequest CreateEventTracker where
        type Rs CreateEventTracker =
             CreateEventTrackerResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 CreateEventTrackerResponse' <$>
                   (x .?> "trackingId") <*> (x .?> "eventTrackerArn")
                     <*> (pure (fromEnum s)))

instance Hashable CreateEventTracker where

instance NFData CreateEventTracker where

instance ToHeaders CreateEventTracker where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.CreateEventTracker" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateEventTracker where
        toJSON CreateEventTracker'{..}
          = object
              (catMaybes
                 [Just ("name" .= _cetName),
                  Just ("datasetGroupArn" .= _cetDatasetGroupARN)])

instance ToPath CreateEventTracker where
        toPath = const "/"

instance ToQuery CreateEventTracker where
        toQuery = const mempty

-- | /See:/ 'createEventTrackerResponse' smart constructor.
data CreateEventTrackerResponse = CreateEventTrackerResponse'{_cetrsTrackingId
                                                              :: !(Maybe Text),
                                                              _cetrsEventTrackerARN
                                                              :: !(Maybe Text),
                                                              _cetrsResponseStatus
                                                              :: !Int}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'CreateEventTrackerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cetrsTrackingId' - The ID of the event tracker. Include this ID in requests to the <https://docs.aws.amazon.com/personalize/latest/dg/API_UBS_PutEvents.html PutEvents> API.
--
-- * 'cetrsEventTrackerARN' - The ARN of the event tracker.
--
-- * 'cetrsResponseStatus' - -- | The response status code.
createEventTrackerResponse
    :: Int -- ^ 'cetrsResponseStatus'
    -> CreateEventTrackerResponse
createEventTrackerResponse pResponseStatus_
  = CreateEventTrackerResponse'{_cetrsTrackingId =
                                  Nothing,
                                _cetrsEventTrackerARN = Nothing,
                                _cetrsResponseStatus = pResponseStatus_}

-- | The ID of the event tracker. Include this ID in requests to the <https://docs.aws.amazon.com/personalize/latest/dg/API_UBS_PutEvents.html PutEvents> API.
cetrsTrackingId :: Lens' CreateEventTrackerResponse (Maybe Text)
cetrsTrackingId = lens _cetrsTrackingId (\ s a -> s{_cetrsTrackingId = a})

-- | The ARN of the event tracker.
cetrsEventTrackerARN :: Lens' CreateEventTrackerResponse (Maybe Text)
cetrsEventTrackerARN = lens _cetrsEventTrackerARN (\ s a -> s{_cetrsEventTrackerARN = a})

-- | -- | The response status code.
cetrsResponseStatus :: Lens' CreateEventTrackerResponse Int
cetrsResponseStatus = lens _cetrsResponseStatus (\ s a -> s{_cetrsResponseStatus = a})

instance NFData CreateEventTrackerResponse where
