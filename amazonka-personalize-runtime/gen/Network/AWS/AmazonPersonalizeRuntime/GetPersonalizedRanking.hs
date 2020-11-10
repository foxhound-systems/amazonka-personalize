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
-- Module      : Network.AWS.AmazonPersonalizeRuntime.GetPersonalizedRanking
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Re-ranks a list of recommended items for the given user. The first item in the list is deemed the most likely item to be of interest to the user.
--
--
module Network.AWS.AmazonPersonalizeRuntime.GetPersonalizedRanking
    (
    -- * Creating a Request
      getPersonalizedRanking
    , GetPersonalizedRanking
    -- * Request Lenses
    , gprContext
    , gprFilterARN
    , gprCampaignARN
    , gprInputList
    , gprUserId

    -- * Destructuring the Response
    , getPersonalizedRankingResponse
    , GetPersonalizedRankingResponse
    -- * Response Lenses
    , gprrsPersonalizedRanking
    , gprrsRecommendationId
    , gprrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalizeRuntime.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPersonalizedRanking' smart constructor.
data GetPersonalizedRanking = GetPersonalizedRanking'{_gprContext
                                                      ::
                                                      !(Maybe
                                                          (Map Text
                                                             (Sensitive Text))),
                                                      _gprFilterARN ::
                                                      !(Maybe Text),
                                                      _gprCampaignARN :: !Text,
                                                      _gprInputList :: ![Text],
                                                      _gprUserId :: !Text}
                                deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPersonalizedRanking' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprContext' - The contextual metadata to use when getting recommendations. Contextual metadata includes any interaction information that might be relevant when getting a user's recommendations, such as the user's current location or device type.
--
-- * 'gprFilterARN' - The Amazon Resource Name (ARN) of a filter you created to include or exclude items from recommendations for a given user.
--
-- * 'gprCampaignARN' - The Amazon Resource Name (ARN) of the campaign to use for generating the personalized ranking.
--
-- * 'gprInputList' - A list of items (by @itemId@ ) to rank. If an item was not included in the training dataset, the item is appended to the end of the reranked list. The maximum is 500.
--
-- * 'gprUserId' - The user for which you want the campaign to provide a personalized ranking.
getPersonalizedRanking
    :: Text -- ^ 'gprCampaignARN'
    -> Text -- ^ 'gprUserId'
    -> GetPersonalizedRanking
getPersonalizedRanking pCampaignARN_ pUserId_
  = GetPersonalizedRanking'{_gprContext = Nothing,
                            _gprFilterARN = Nothing,
                            _gprCampaignARN = pCampaignARN_,
                            _gprInputList = mempty, _gprUserId = pUserId_}

-- | The contextual metadata to use when getting recommendations. Contextual metadata includes any interaction information that might be relevant when getting a user's recommendations, such as the user's current location or device type.
gprContext :: Lens' GetPersonalizedRanking (HashMap Text Text)
gprContext = lens _gprContext (\ s a -> s{_gprContext = a}) . _Default . _Map

-- | The Amazon Resource Name (ARN) of a filter you created to include or exclude items from recommendations for a given user.
gprFilterARN :: Lens' GetPersonalizedRanking (Maybe Text)
gprFilterARN = lens _gprFilterARN (\ s a -> s{_gprFilterARN = a})

-- | The Amazon Resource Name (ARN) of the campaign to use for generating the personalized ranking.
gprCampaignARN :: Lens' GetPersonalizedRanking Text
gprCampaignARN = lens _gprCampaignARN (\ s a -> s{_gprCampaignARN = a})

-- | A list of items (by @itemId@ ) to rank. If an item was not included in the training dataset, the item is appended to the end of the reranked list. The maximum is 500.
gprInputList :: Lens' GetPersonalizedRanking [Text]
gprInputList = lens _gprInputList (\ s a -> s{_gprInputList = a}) . _Coerce

-- | The user for which you want the campaign to provide a personalized ranking.
gprUserId :: Lens' GetPersonalizedRanking Text
gprUserId = lens _gprUserId (\ s a -> s{_gprUserId = a})

instance AWSRequest GetPersonalizedRanking where
        type Rs GetPersonalizedRanking =
             GetPersonalizedRankingResponse
        request = postJSON amazonPersonalizeRuntime
        response
          = receiveJSON
              (\ s h x ->
                 GetPersonalizedRankingResponse' <$>
                   (x .?> "personalizedRanking" .!@ mempty) <*>
                     (x .?> "recommendationId")
                     <*> (pure (fromEnum s)))

instance Hashable GetPersonalizedRanking where

instance NFData GetPersonalizedRanking where

instance ToHeaders GetPersonalizedRanking where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetPersonalizedRanking where
        toJSON GetPersonalizedRanking'{..}
          = object
              (catMaybes
                 [("context" .=) <$> _gprContext,
                  ("filterArn" .=) <$> _gprFilterARN,
                  Just ("campaignArn" .= _gprCampaignARN),
                  Just ("inputList" .= _gprInputList),
                  Just ("userId" .= _gprUserId)])

instance ToPath GetPersonalizedRanking where
        toPath = const "/personalize-ranking"

instance ToQuery GetPersonalizedRanking where
        toQuery = const mempty

-- | /See:/ 'getPersonalizedRankingResponse' smart constructor.
data GetPersonalizedRankingResponse = GetPersonalizedRankingResponse'{_gprrsPersonalizedRanking
                                                                      ::
                                                                      !(Maybe
                                                                          [PredictedItem]),
                                                                      _gprrsRecommendationId
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _gprrsResponseStatus
                                                                      :: !Int}
                                        deriving (Eq, Read, Show, Data,
                                                  Typeable, Generic)

-- | Creates a value of 'GetPersonalizedRankingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprrsPersonalizedRanking' - A list of items in order of most likely interest to the user. The maximum is 500.
--
-- * 'gprrsRecommendationId' - The ID of the recommendation.
--
-- * 'gprrsResponseStatus' - -- | The response status code.
getPersonalizedRankingResponse
    :: Int -- ^ 'gprrsResponseStatus'
    -> GetPersonalizedRankingResponse
getPersonalizedRankingResponse pResponseStatus_
  = GetPersonalizedRankingResponse'{_gprrsPersonalizedRanking
                                      = Nothing,
                                    _gprrsRecommendationId = Nothing,
                                    _gprrsResponseStatus = pResponseStatus_}

-- | A list of items in order of most likely interest to the user. The maximum is 500.
gprrsPersonalizedRanking :: Lens' GetPersonalizedRankingResponse [PredictedItem]
gprrsPersonalizedRanking = lens _gprrsPersonalizedRanking (\ s a -> s{_gprrsPersonalizedRanking = a}) . _Default . _Coerce

-- | The ID of the recommendation.
gprrsRecommendationId :: Lens' GetPersonalizedRankingResponse (Maybe Text)
gprrsRecommendationId = lens _gprrsRecommendationId (\ s a -> s{_gprrsRecommendationId = a})

-- | -- | The response status code.
gprrsResponseStatus :: Lens' GetPersonalizedRankingResponse Int
gprrsResponseStatus = lens _gprrsResponseStatus (\ s a -> s{_gprrsResponseStatus = a})

instance NFData GetPersonalizedRankingResponse where
