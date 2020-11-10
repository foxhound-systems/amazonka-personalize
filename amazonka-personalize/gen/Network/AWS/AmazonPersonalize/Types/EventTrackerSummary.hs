{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.EventTrackerSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.EventTrackerSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a summary of the properties of an event tracker. For a complete listing, call the 'DescribeEventTracker' API.
--
--
--
-- /See:/ 'eventTrackerSummary' smart constructor.
data EventTrackerSummary = EventTrackerSummary'{_etsStatus
                                                :: !(Maybe Text),
                                                _etsLastUpdatedDateTime ::
                                                !(Maybe POSIX),
                                                _etsName :: !(Maybe Text),
                                                _etsCreationDateTime ::
                                                !(Maybe POSIX),
                                                _etsEventTrackerARN ::
                                                !(Maybe Text)}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventTrackerSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etsStatus' - The status of the event tracker. An event tracker can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING > DELETE IN_PROGRESS
--
-- * 'etsLastUpdatedDateTime' - The date and time (in Unix time) that the event tracker was last updated.
--
-- * 'etsName' - The name of the event tracker.
--
-- * 'etsCreationDateTime' - The date and time (in Unix time) that the event tracker was created.
--
-- * 'etsEventTrackerARN' - The Amazon Resource Name (ARN) of the event tracker.
eventTrackerSummary
    :: EventTrackerSummary
eventTrackerSummary
  = EventTrackerSummary'{_etsStatus = Nothing,
                         _etsLastUpdatedDateTime = Nothing,
                         _etsName = Nothing, _etsCreationDateTime = Nothing,
                         _etsEventTrackerARN = Nothing}

-- | The status of the event tracker. An event tracker can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING > DELETE IN_PROGRESS
etsStatus :: Lens' EventTrackerSummary (Maybe Text)
etsStatus = lens _etsStatus (\ s a -> s{_etsStatus = a})

-- | The date and time (in Unix time) that the event tracker was last updated.
etsLastUpdatedDateTime :: Lens' EventTrackerSummary (Maybe UTCTime)
etsLastUpdatedDateTime = lens _etsLastUpdatedDateTime (\ s a -> s{_etsLastUpdatedDateTime = a}) . mapping _Time

-- | The name of the event tracker.
etsName :: Lens' EventTrackerSummary (Maybe Text)
etsName = lens _etsName (\ s a -> s{_etsName = a})

-- | The date and time (in Unix time) that the event tracker was created.
etsCreationDateTime :: Lens' EventTrackerSummary (Maybe UTCTime)
etsCreationDateTime = lens _etsCreationDateTime (\ s a -> s{_etsCreationDateTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the event tracker.
etsEventTrackerARN :: Lens' EventTrackerSummary (Maybe Text)
etsEventTrackerARN = lens _etsEventTrackerARN (\ s a -> s{_etsEventTrackerARN = a})

instance FromJSON EventTrackerSummary where
        parseJSON
          = withObject "EventTrackerSummary"
              (\ x ->
                 EventTrackerSummary' <$>
                   (x .:? "status") <*> (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "name")
                     <*> (x .:? "creationDateTime")
                     <*> (x .:? "eventTrackerArn"))

instance Hashable EventTrackerSummary where

instance NFData EventTrackerSummary where
