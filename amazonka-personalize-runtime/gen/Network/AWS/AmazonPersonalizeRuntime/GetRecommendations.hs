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
-- Module      : Network.AWS.AmazonPersonalizeRuntime.GetRecommendations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of recommended items. The required input depends on the recipe type used to create the solution backing the campaign, as follows:
--
--
--     * RELATED_ITEMS - @itemId@ required, @userId@ not used
--
--     * USER_PERSONALIZATION - @itemId@ optional, @userId@ required
--
--
--
module Network.AWS.AmazonPersonalizeRuntime.GetRecommendations
    (
    -- * Creating a Request
      getRecommendations
    , GetRecommendations
    -- * Request Lenses
    , grContext
    , grItemId
    , grUserId
    , grNumResults
    , grFilterARN
    , grCampaignARN

    -- * Destructuring the Response
    , getRecommendationsResponse
    , GetRecommendationsResponse
    -- * Response Lenses
    , grrsRecommendationId
    , grrsItemList
    , grrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalizeRuntime.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRecommendations' smart constructor.
data GetRecommendations = GetRecommendations'{_grContext
                                              ::
                                              !(Maybe
                                                  (Map Text (Sensitive Text))),
                                              _grItemId :: !(Maybe Text),
                                              _grUserId :: !(Maybe Text),
                                              _grNumResults :: !(Maybe Nat),
                                              _grFilterARN :: !(Maybe Text),
                                              _grCampaignARN :: !Text}
                            deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetRecommendations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grContext' - The contextual metadata to use when getting recommendations. Contextual metadata includes any interaction information that might be relevant when getting a user's recommendations, such as the user's current location or device type.
--
-- * 'grItemId' - The item ID to provide recommendations for. Required for @RELATED_ITEMS@ recipe type.
--
-- * 'grUserId' - The user ID to provide recommendations for. Required for @USER_PERSONALIZATION@ recipe type.
--
-- * 'grNumResults' - The number of results to return. The default is 25. The maximum is 500.
--
-- * 'grFilterARN' - The ARN of the filter to apply to the returned recommendations. For more information, see <https://docs.aws.amazon.com/personalize/latest/dg/filters.html Using Filters with Amazon Personalize> . When using this parameter, be sure the filter resource is @ACTIVE@ .
--
-- * 'grCampaignARN' - The Amazon Resource Name (ARN) of the campaign to use for getting recommendations.
getRecommendations
    :: Text -- ^ 'grCampaignARN'
    -> GetRecommendations
getRecommendations pCampaignARN_
  = GetRecommendations'{_grContext = Nothing,
                        _grItemId = Nothing, _grUserId = Nothing,
                        _grNumResults = Nothing, _grFilterARN = Nothing,
                        _grCampaignARN = pCampaignARN_}

-- | The contextual metadata to use when getting recommendations. Contextual metadata includes any interaction information that might be relevant when getting a user's recommendations, such as the user's current location or device type.
grContext :: Lens' GetRecommendations (HashMap Text Text)
grContext = lens _grContext (\ s a -> s{_grContext = a}) . _Default . _Map

-- | The item ID to provide recommendations for. Required for @RELATED_ITEMS@ recipe type.
grItemId :: Lens' GetRecommendations (Maybe Text)
grItemId = lens _grItemId (\ s a -> s{_grItemId = a})

-- | The user ID to provide recommendations for. Required for @USER_PERSONALIZATION@ recipe type.
grUserId :: Lens' GetRecommendations (Maybe Text)
grUserId = lens _grUserId (\ s a -> s{_grUserId = a})

-- | The number of results to return. The default is 25. The maximum is 500.
grNumResults :: Lens' GetRecommendations (Maybe Natural)
grNumResults = lens _grNumResults (\ s a -> s{_grNumResults = a}) . mapping _Nat

-- | The ARN of the filter to apply to the returned recommendations. For more information, see <https://docs.aws.amazon.com/personalize/latest/dg/filters.html Using Filters with Amazon Personalize> . When using this parameter, be sure the filter resource is @ACTIVE@ .
grFilterARN :: Lens' GetRecommendations (Maybe Text)
grFilterARN = lens _grFilterARN (\ s a -> s{_grFilterARN = a})

-- | The Amazon Resource Name (ARN) of the campaign to use for getting recommendations.
grCampaignARN :: Lens' GetRecommendations Text
grCampaignARN = lens _grCampaignARN (\ s a -> s{_grCampaignARN = a})

instance AWSRequest GetRecommendations where
        type Rs GetRecommendations =
             GetRecommendationsResponse
        request = postJSON amazonPersonalizeRuntime
        response
          = receiveJSON
              (\ s h x ->
                 GetRecommendationsResponse' <$>
                   (x .?> "recommendationId") <*>
                     (x .?> "itemList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetRecommendations where

instance NFData GetRecommendations where

instance ToHeaders GetRecommendations where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRecommendations where
        toJSON GetRecommendations'{..}
          = object
              (catMaybes
                 [("context" .=) <$> _grContext,
                  ("itemId" .=) <$> _grItemId,
                  ("userId" .=) <$> _grUserId,
                  ("numResults" .=) <$> _grNumResults,
                  ("filterArn" .=) <$> _grFilterARN,
                  Just ("campaignArn" .= _grCampaignARN)])

instance ToPath GetRecommendations where
        toPath = const "/recommendations"

instance ToQuery GetRecommendations where
        toQuery = const mempty

-- | /See:/ 'getRecommendationsResponse' smart constructor.
data GetRecommendationsResponse = GetRecommendationsResponse'{_grrsRecommendationId
                                                              :: !(Maybe Text),
                                                              _grrsItemList ::
                                                              !(Maybe
                                                                  [PredictedItem]),
                                                              _grrsResponseStatus
                                                              :: !Int}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'GetRecommendationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrsRecommendationId' - The ID of the recommendation.
--
-- * 'grrsItemList' - A list of recommendations sorted in ascending order by prediction score. There can be a maximum of 500 items in the list.
--
-- * 'grrsResponseStatus' - -- | The response status code.
getRecommendationsResponse
    :: Int -- ^ 'grrsResponseStatus'
    -> GetRecommendationsResponse
getRecommendationsResponse pResponseStatus_
  = GetRecommendationsResponse'{_grrsRecommendationId =
                                  Nothing,
                                _grrsItemList = Nothing,
                                _grrsResponseStatus = pResponseStatus_}

-- | The ID of the recommendation.
grrsRecommendationId :: Lens' GetRecommendationsResponse (Maybe Text)
grrsRecommendationId = lens _grrsRecommendationId (\ s a -> s{_grrsRecommendationId = a})

-- | A list of recommendations sorted in ascending order by prediction score. There can be a maximum of 500 items in the list.
grrsItemList :: Lens' GetRecommendationsResponse [PredictedItem]
grrsItemList = lens _grrsItemList (\ s a -> s{_grrsItemList = a}) . _Default . _Coerce

-- | -- | The response status code.
grrsResponseStatus :: Lens' GetRecommendationsResponse Int
grrsResponseStatus = lens _grrsResponseStatus (\ s a -> s{_grrsResponseStatus = a})

instance NFData GetRecommendationsResponse where
