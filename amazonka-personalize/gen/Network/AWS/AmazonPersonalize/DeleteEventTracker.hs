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
-- Module      : Network.AWS.AmazonPersonalize.DeleteEventTracker
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the event tracker. Does not delete the event-interactions dataset from the associated dataset group. For more information on event trackers, see 'CreateEventTracker' .
--
--
module Network.AWS.AmazonPersonalize.DeleteEventTracker
    (
    -- * Creating a Request
      deleteEventTracker
    , DeleteEventTracker
    -- * Request Lenses
    , detEventTrackerARN

    -- * Destructuring the Response
    , deleteEventTrackerResponse
    , DeleteEventTrackerResponse
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteEventTracker' smart constructor.
newtype DeleteEventTracker = DeleteEventTracker'{_detEventTrackerARN
                                                 :: Text}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'DeleteEventTracker' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detEventTrackerARN' - The Amazon Resource Name (ARN) of the event tracker to delete.
deleteEventTracker
    :: Text -- ^ 'detEventTrackerARN'
    -> DeleteEventTracker
deleteEventTracker pEventTrackerARN_
  = DeleteEventTracker'{_detEventTrackerARN =
                          pEventTrackerARN_}

-- | The Amazon Resource Name (ARN) of the event tracker to delete.
detEventTrackerARN :: Lens' DeleteEventTracker Text
detEventTrackerARN = lens _detEventTrackerARN (\ s a -> s{_detEventTrackerARN = a})

instance AWSRequest DeleteEventTracker where
        type Rs DeleteEventTracker =
             DeleteEventTrackerResponse
        request = postJSON amazonPersonalize
        response = receiveNull DeleteEventTrackerResponse'

instance Hashable DeleteEventTracker where

instance NFData DeleteEventTracker where

instance ToHeaders DeleteEventTracker where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DeleteEventTracker" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteEventTracker where
        toJSON DeleteEventTracker'{..}
          = object
              (catMaybes
                 [Just ("eventTrackerArn" .= _detEventTrackerARN)])

instance ToPath DeleteEventTracker where
        toPath = const "/"

instance ToQuery DeleteEventTracker where
        toQuery = const mempty

-- | /See:/ 'deleteEventTrackerResponse' smart constructor.
data DeleteEventTrackerResponse = DeleteEventTrackerResponse'
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'DeleteEventTrackerResponse' with the minimum fields required to make a request.
--
deleteEventTrackerResponse
    :: DeleteEventTrackerResponse
deleteEventTrackerResponse
  = DeleteEventTrackerResponse'

instance NFData DeleteEventTrackerResponse where
