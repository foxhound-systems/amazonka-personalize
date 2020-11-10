{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.EventTracker
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.EventTracker where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about an event tracker.
--
--
--
-- /See:/ 'eventTracker' smart constructor.
data EventTracker = EventTracker'{_etStatus ::
                                  !(Maybe Text),
                                  _etTrackingId :: !(Maybe Text),
                                  _etLastUpdatedDateTime :: !(Maybe POSIX),
                                  _etAccountId :: !(Maybe Text),
                                  _etName :: !(Maybe Text),
                                  _etCreationDateTime :: !(Maybe POSIX),
                                  _etDatasetGroupARN :: !(Maybe Text),
                                  _etEventTrackerARN :: !(Maybe Text)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventTracker' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etStatus' - The status of the event tracker. An event tracker can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING > DELETE IN_PROGRESS
--
-- * 'etTrackingId' - The ID of the event tracker. Include this ID in requests to the <https://docs.aws.amazon.com/personalize/latest/dg/API_UBS_PutEvents.html PutEvents> API.
--
-- * 'etLastUpdatedDateTime' - The date and time (in Unix time) that the event tracker was last updated.
--
-- * 'etAccountId' - The Amazon AWS account that owns the event tracker.
--
-- * 'etName' - The name of the event tracker.
--
-- * 'etCreationDateTime' - The date and time (in Unix format) that the event tracker was created.
--
-- * 'etDatasetGroupARN' - The Amazon Resource Name (ARN) of the dataset group that receives the event data.
--
-- * 'etEventTrackerARN' - The ARN of the event tracker.
eventTracker
    :: EventTracker
eventTracker
  = EventTracker'{_etStatus = Nothing,
                  _etTrackingId = Nothing,
                  _etLastUpdatedDateTime = Nothing,
                  _etAccountId = Nothing, _etName = Nothing,
                  _etCreationDateTime = Nothing,
                  _etDatasetGroupARN = Nothing,
                  _etEventTrackerARN = Nothing}

-- | The status of the event tracker. An event tracker can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING > DELETE IN_PROGRESS
etStatus :: Lens' EventTracker (Maybe Text)
etStatus = lens _etStatus (\ s a -> s{_etStatus = a})

-- | The ID of the event tracker. Include this ID in requests to the <https://docs.aws.amazon.com/personalize/latest/dg/API_UBS_PutEvents.html PutEvents> API.
etTrackingId :: Lens' EventTracker (Maybe Text)
etTrackingId = lens _etTrackingId (\ s a -> s{_etTrackingId = a})

-- | The date and time (in Unix time) that the event tracker was last updated.
etLastUpdatedDateTime :: Lens' EventTracker (Maybe UTCTime)
etLastUpdatedDateTime = lens _etLastUpdatedDateTime (\ s a -> s{_etLastUpdatedDateTime = a}) . mapping _Time

-- | The Amazon AWS account that owns the event tracker.
etAccountId :: Lens' EventTracker (Maybe Text)
etAccountId = lens _etAccountId (\ s a -> s{_etAccountId = a})

-- | The name of the event tracker.
etName :: Lens' EventTracker (Maybe Text)
etName = lens _etName (\ s a -> s{_etName = a})

-- | The date and time (in Unix format) that the event tracker was created.
etCreationDateTime :: Lens' EventTracker (Maybe UTCTime)
etCreationDateTime = lens _etCreationDateTime (\ s a -> s{_etCreationDateTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the dataset group that receives the event data.
etDatasetGroupARN :: Lens' EventTracker (Maybe Text)
etDatasetGroupARN = lens _etDatasetGroupARN (\ s a -> s{_etDatasetGroupARN = a})

-- | The ARN of the event tracker.
etEventTrackerARN :: Lens' EventTracker (Maybe Text)
etEventTrackerARN = lens _etEventTrackerARN (\ s a -> s{_etEventTrackerARN = a})

instance FromJSON EventTracker where
        parseJSON
          = withObject "EventTracker"
              (\ x ->
                 EventTracker' <$>
                   (x .:? "status") <*> (x .:? "trackingId") <*>
                     (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "accountId")
                     <*> (x .:? "name")
                     <*> (x .:? "creationDateTime")
                     <*> (x .:? "datasetGroupArn")
                     <*> (x .:? "eventTrackerArn"))

instance Hashable EventTracker where

instance NFData EventTracker where
