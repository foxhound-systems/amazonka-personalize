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
-- Module      : Network.AWS.AmazonPersonalize.DescribeEventTracker
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an event tracker. The response includes the @trackingId@ and @status@ of the event tracker. For more information on event trackers, see 'CreateEventTracker' .
--
--
module Network.AWS.AmazonPersonalize.DescribeEventTracker
    (
    -- * Creating a Request
      describeEventTracker
    , DescribeEventTracker
    -- * Request Lenses
    , dEventTrackerARN

    -- * Destructuring the Response
    , describeEventTrackerResponse
    , DescribeEventTrackerResponse
    -- * Response Lenses
    , detrsEventTracker
    , detrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEventTracker' smart constructor.
newtype DescribeEventTracker = DescribeEventTracker'{_dEventTrackerARN
                                                     :: Text}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'DescribeEventTracker' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dEventTrackerARN' - The Amazon Resource Name (ARN) of the event tracker to describe.
describeEventTracker
    :: Text -- ^ 'dEventTrackerARN'
    -> DescribeEventTracker
describeEventTracker pEventTrackerARN_
  = DescribeEventTracker'{_dEventTrackerARN =
                            pEventTrackerARN_}

-- | The Amazon Resource Name (ARN) of the event tracker to describe.
dEventTrackerARN :: Lens' DescribeEventTracker Text
dEventTrackerARN = lens _dEventTrackerARN (\ s a -> s{_dEventTrackerARN = a})

instance AWSRequest DescribeEventTracker where
        type Rs DescribeEventTracker =
             DescribeEventTrackerResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEventTrackerResponse' <$>
                   (x .?> "eventTracker") <*> (pure (fromEnum s)))

instance Hashable DescribeEventTracker where

instance NFData DescribeEventTracker where

instance ToHeaders DescribeEventTracker where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DescribeEventTracker" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEventTracker where
        toJSON DescribeEventTracker'{..}
          = object
              (catMaybes
                 [Just ("eventTrackerArn" .= _dEventTrackerARN)])

instance ToPath DescribeEventTracker where
        toPath = const "/"

instance ToQuery DescribeEventTracker where
        toQuery = const mempty

-- | /See:/ 'describeEventTrackerResponse' smart constructor.
data DescribeEventTrackerResponse = DescribeEventTrackerResponse'{_detrsEventTracker
                                                                  ::
                                                                  !(Maybe
                                                                      EventTracker),
                                                                  _detrsResponseStatus
                                                                  :: !Int}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'DescribeEventTrackerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detrsEventTracker' - An object that describes the event tracker.
--
-- * 'detrsResponseStatus' - -- | The response status code.
describeEventTrackerResponse
    :: Int -- ^ 'detrsResponseStatus'
    -> DescribeEventTrackerResponse
describeEventTrackerResponse pResponseStatus_
  = DescribeEventTrackerResponse'{_detrsEventTracker =
                                    Nothing,
                                  _detrsResponseStatus = pResponseStatus_}

-- | An object that describes the event tracker.
detrsEventTracker :: Lens' DescribeEventTrackerResponse (Maybe EventTracker)
detrsEventTracker = lens _detrsEventTracker (\ s a -> s{_detrsEventTracker = a})

-- | -- | The response status code.
detrsResponseStatus :: Lens' DescribeEventTrackerResponse Int
detrsResponseStatus = lens _detrsResponseStatus (\ s a -> s{_detrsResponseStatus = a})

instance NFData DescribeEventTrackerResponse where
