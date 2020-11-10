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
-- Module      : Network.AWS.AmazonPersonalize.DescribeCampaign
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the given campaign, including its status.
--
--
-- A campaign can be in one of the following states:
--
--     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
--     * DELETE PENDING > DELETE IN_PROGRESS
--
--
--
-- When the @status@ is @CREATE FAILED@ , the response includes the @failureReason@ key, which describes why.
--
-- For more information on campaigns, see 'CreateCampaign' .
--
module Network.AWS.AmazonPersonalize.DescribeCampaign
    (
    -- * Creating a Request
      describeCampaign
    , DescribeCampaign
    -- * Request Lenses
    , dCampaignARN

    -- * Destructuring the Response
    , describeCampaignResponse
    , DescribeCampaignResponse
    -- * Response Lenses
    , dcrsCampaign
    , dcrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeCampaign' smart constructor.
newtype DescribeCampaign = DescribeCampaign'{_dCampaignARN
                                             :: Text}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeCampaign' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCampaignARN' - The Amazon Resource Name (ARN) of the campaign.
describeCampaign
    :: Text -- ^ 'dCampaignARN'
    -> DescribeCampaign
describeCampaign pCampaignARN_
  = DescribeCampaign'{_dCampaignARN = pCampaignARN_}

-- | The Amazon Resource Name (ARN) of the campaign.
dCampaignARN :: Lens' DescribeCampaign Text
dCampaignARN = lens _dCampaignARN (\ s a -> s{_dCampaignARN = a})

instance AWSRequest DescribeCampaign where
        type Rs DescribeCampaign = DescribeCampaignResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCampaignResponse' <$>
                   (x .?> "campaign") <*> (pure (fromEnum s)))

instance Hashable DescribeCampaign where

instance NFData DescribeCampaign where

instance ToHeaders DescribeCampaign where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DescribeCampaign" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeCampaign where
        toJSON DescribeCampaign'{..}
          = object
              (catMaybes [Just ("campaignArn" .= _dCampaignARN)])

instance ToPath DescribeCampaign where
        toPath = const "/"

instance ToQuery DescribeCampaign where
        toQuery = const mempty

-- | /See:/ 'describeCampaignResponse' smart constructor.
data DescribeCampaignResponse = DescribeCampaignResponse'{_dcrsCampaign
                                                          :: !(Maybe Campaign),
                                                          _dcrsResponseStatus ::
                                                          !Int}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'DescribeCampaignResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsCampaign' - The properties of the campaign.
--
-- * 'dcrsResponseStatus' - -- | The response status code.
describeCampaignResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DescribeCampaignResponse
describeCampaignResponse pResponseStatus_
  = DescribeCampaignResponse'{_dcrsCampaign = Nothing,
                              _dcrsResponseStatus = pResponseStatus_}

-- | The properties of the campaign.
dcrsCampaign :: Lens' DescribeCampaignResponse (Maybe Campaign)
dcrsCampaign = lens _dcrsCampaign (\ s a -> s{_dcrsCampaign = a})

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DescribeCampaignResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a})

instance NFData DescribeCampaignResponse where
