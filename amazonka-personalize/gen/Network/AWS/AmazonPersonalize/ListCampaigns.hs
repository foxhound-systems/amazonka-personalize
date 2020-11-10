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
-- Module      : Network.AWS.AmazonPersonalize.ListCampaigns
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of campaigns that use the given solution. When a solution is not specified, all the campaigns associated with the account are listed. The response provides the properties for each campaign, including the Amazon Resource Name (ARN). For more information on campaigns, see 'CreateCampaign' .
--
--
--
-- This operation returns paginated results.
module Network.AWS.AmazonPersonalize.ListCampaigns
    (
    -- * Creating a Request
      listCampaigns
    , ListCampaigns
    -- * Request Lenses
    , lcSolutionARN
    , lcNextToken
    , lcMaxResults

    -- * Destructuring the Response
    , listCampaignsResponse
    , ListCampaignsResponse
    -- * Response Lenses
    , lcrsCampaigns
    , lcrsNextToken
    , lcrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listCampaigns' smart constructor.
data ListCampaigns = ListCampaigns'{_lcSolutionARN ::
                                    !(Maybe Text),
                                    _lcNextToken :: !(Maybe Text),
                                    _lcMaxResults :: !(Maybe Nat)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListCampaigns' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcSolutionARN' - The Amazon Resource Name (ARN) of the solution to list the campaigns for. When a solution is not specified, all the campaigns associated with the account are listed.
--
-- * 'lcNextToken' - A token returned from the previous call to @ListCampaigns@ for getting the next set of campaigns (if they exist).
--
-- * 'lcMaxResults' - The maximum number of campaigns to return.
listCampaigns
    :: ListCampaigns
listCampaigns
  = ListCampaigns'{_lcSolutionARN = Nothing,
                   _lcNextToken = Nothing, _lcMaxResults = Nothing}

-- | The Amazon Resource Name (ARN) of the solution to list the campaigns for. When a solution is not specified, all the campaigns associated with the account are listed.
lcSolutionARN :: Lens' ListCampaigns (Maybe Text)
lcSolutionARN = lens _lcSolutionARN (\ s a -> s{_lcSolutionARN = a})

-- | A token returned from the previous call to @ListCampaigns@ for getting the next set of campaigns (if they exist).
lcNextToken :: Lens' ListCampaigns (Maybe Text)
lcNextToken = lens _lcNextToken (\ s a -> s{_lcNextToken = a})

-- | The maximum number of campaigns to return.
lcMaxResults :: Lens' ListCampaigns (Maybe Natural)
lcMaxResults = lens _lcMaxResults (\ s a -> s{_lcMaxResults = a}) . mapping _Nat

instance AWSPager ListCampaigns where
        page rq rs
          | stop (rs ^. lcrsNextToken) = Nothing
          | stop (rs ^. lcrsCampaigns) = Nothing
          | otherwise =
            Just $ rq & lcNextToken .~ rs ^. lcrsNextToken

instance AWSRequest ListCampaigns where
        type Rs ListCampaigns = ListCampaignsResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 ListCampaignsResponse' <$>
                   (x .?> "campaigns" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListCampaigns where

instance NFData ListCampaigns where

instance ToHeaders ListCampaigns where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.ListCampaigns" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListCampaigns where
        toJSON ListCampaigns'{..}
          = object
              (catMaybes
                 [("solutionArn" .=) <$> _lcSolutionARN,
                  ("nextToken" .=) <$> _lcNextToken,
                  ("maxResults" .=) <$> _lcMaxResults])

instance ToPath ListCampaigns where
        toPath = const "/"

instance ToQuery ListCampaigns where
        toQuery = const mempty

-- | /See:/ 'listCampaignsResponse' smart constructor.
data ListCampaignsResponse = ListCampaignsResponse'{_lcrsCampaigns
                                                    ::
                                                    !(Maybe [CampaignSummary]),
                                                    _lcrsNextToken ::
                                                    !(Maybe Text),
                                                    _lcrsResponseStatus :: !Int}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'ListCampaignsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrsCampaigns' - A list of the campaigns.
--
-- * 'lcrsNextToken' - A token for getting the next set of campaigns (if they exist).
--
-- * 'lcrsResponseStatus' - -- | The response status code.
listCampaignsResponse
    :: Int -- ^ 'lcrsResponseStatus'
    -> ListCampaignsResponse
listCampaignsResponse pResponseStatus_
  = ListCampaignsResponse'{_lcrsCampaigns = Nothing,
                           _lcrsNextToken = Nothing,
                           _lcrsResponseStatus = pResponseStatus_}

-- | A list of the campaigns.
lcrsCampaigns :: Lens' ListCampaignsResponse [CampaignSummary]
lcrsCampaigns = lens _lcrsCampaigns (\ s a -> s{_lcrsCampaigns = a}) . _Default . _Coerce

-- | A token for getting the next set of campaigns (if they exist).
lcrsNextToken :: Lens' ListCampaignsResponse (Maybe Text)
lcrsNextToken = lens _lcrsNextToken (\ s a -> s{_lcrsNextToken = a})

-- | -- | The response status code.
lcrsResponseStatus :: Lens' ListCampaignsResponse Int
lcrsResponseStatus = lens _lcrsResponseStatus (\ s a -> s{_lcrsResponseStatus = a})

instance NFData ListCampaignsResponse where
