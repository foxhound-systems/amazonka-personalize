{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalizeEvents.Types.Event
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalizeEvents.Types.Event where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents user interaction event information sent using the @PutEvents@ API.
--
--
--
-- /See:/ 'event' smart constructor.
data Event = Event'{_eRecommendationId ::
                    !(Maybe Text),
                    _eEventValue :: !(Maybe Double),
                    _eItemId :: !(Maybe Text),
                    _eImpression :: !(Maybe (List1 Text)),
                    _eEventId :: !(Maybe Text),
                    _eProperties :: !(Maybe Text), _eEventType :: !Text,
                    _eSentAt :: !POSIX}
               deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eRecommendationId' - The ID of the recommendation.
--
-- * 'eEventValue' - The event value that corresponds to the @EVENT_VALUE@ field of the Interactions schema.
--
-- * 'eItemId' - The item ID key that corresponds to the @ITEM_ID@ field of the Interactions schema.
--
-- * 'eImpression' - A list of item IDs that represents the sequence of items you have shown the user. For example, @["itemId1", "itemId2", "itemId3"]@ .
--
-- * 'eEventId' - An ID associated with the event. If an event ID is not provided, Amazon Personalize generates a unique ID for the event. An event ID is not used as an input to the model. Amazon Personalize uses the event ID to distinquish unique events. Any subsequent events after the first with the same event ID are not used in model training.
--
-- * 'eProperties' - A string map of event-specific data that you might choose to record. For example, if a user rates a movie on your site, other than movie ID (@itemId@ ) and rating (@eventValue@ ) , you might also send the number of movie ratings made by the user. Each item in the map consists of a key-value pair. For example, @{"numberOfRatings": "12"}@  The keys use camel case names that match the fields in the Interactions schema. In the above example, the @numberOfRatings@ would match the 'NUMBER_OF_RATINGS' field defined in the Interactions schema.
--
-- * 'eEventType' - The type of event, such as click or download. This property corresponds to the @EVENT_TYPE@ field of your Interactions schema and depends on the types of events you are tracking.
--
-- * 'eSentAt' - The timestamp (in Unix time) on the client side when the event occurred.
event
    :: Text -- ^ 'eEventType'
    -> UTCTime -- ^ 'eSentAt'
    -> Event
event pEventType_ pSentAt_
  = Event'{_eRecommendationId = Nothing,
           _eEventValue = Nothing, _eItemId = Nothing,
           _eImpression = Nothing, _eEventId = Nothing,
           _eProperties = Nothing, _eEventType = pEventType_,
           _eSentAt = _Time # pSentAt_}

-- | The ID of the recommendation.
eRecommendationId :: Lens' Event (Maybe Text)
eRecommendationId = lens _eRecommendationId (\ s a -> s{_eRecommendationId = a})

-- | The event value that corresponds to the @EVENT_VALUE@ field of the Interactions schema.
eEventValue :: Lens' Event (Maybe Double)
eEventValue = lens _eEventValue (\ s a -> s{_eEventValue = a})

-- | The item ID key that corresponds to the @ITEM_ID@ field of the Interactions schema.
eItemId :: Lens' Event (Maybe Text)
eItemId = lens _eItemId (\ s a -> s{_eItemId = a})

-- | A list of item IDs that represents the sequence of items you have shown the user. For example, @["itemId1", "itemId2", "itemId3"]@ .
eImpression :: Lens' Event (Maybe (NonEmpty Text))
eImpression = lens _eImpression (\ s a -> s{_eImpression = a}) . mapping _List1

-- | An ID associated with the event. If an event ID is not provided, Amazon Personalize generates a unique ID for the event. An event ID is not used as an input to the model. Amazon Personalize uses the event ID to distinquish unique events. Any subsequent events after the first with the same event ID are not used in model training.
eEventId :: Lens' Event (Maybe Text)
eEventId = lens _eEventId (\ s a -> s{_eEventId = a})

-- | A string map of event-specific data that you might choose to record. For example, if a user rates a movie on your site, other than movie ID (@itemId@ ) and rating (@eventValue@ ) , you might also send the number of movie ratings made by the user. Each item in the map consists of a key-value pair. For example, @{"numberOfRatings": "12"}@  The keys use camel case names that match the fields in the Interactions schema. In the above example, the @numberOfRatings@ would match the 'NUMBER_OF_RATINGS' field defined in the Interactions schema.
eProperties :: Lens' Event (Maybe Text)
eProperties = lens _eProperties (\ s a -> s{_eProperties = a})

-- | The type of event, such as click or download. This property corresponds to the @EVENT_TYPE@ field of your Interactions schema and depends on the types of events you are tracking.
eEventType :: Lens' Event Text
eEventType = lens _eEventType (\ s a -> s{_eEventType = a})

-- | The timestamp (in Unix time) on the client side when the event occurred.
eSentAt :: Lens' Event UTCTime
eSentAt = lens _eSentAt (\ s a -> s{_eSentAt = a}) . _Time

instance Hashable Event where

instance NFData Event where

instance ToJSON Event where
        toJSON Event'{..}
          = object
              (catMaybes
                 [("recommendationId" .=) <$> _eRecommendationId,
                  ("eventValue" .=) <$> _eEventValue,
                  ("itemId" .=) <$> _eItemId,
                  ("impression" .=) <$> _eImpression,
                  ("eventId" .=) <$> _eEventId,
                  ("properties" .=) <$> _eProperties,
                  Just ("eventType" .= _eEventType),
                  Just ("sentAt" .= _eSentAt)])
