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
-- Module      : Network.AWS.AmazonPersonalize.CreateCampaign
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a campaign by deploying a solution version. When a client calls the <https://docs.aws.amazon.com/personalize/latest/dg/API_RS_GetRecommendations.html GetRecommendations> and <https://docs.aws.amazon.com/personalize/latest/dg/API_RS_GetPersonalizedRanking.html GetPersonalizedRanking> APIs, a campaign is specified in the request.
--
--
-- __Minimum Provisioned TPS and Auto-Scaling__ 
--
-- A transaction is a single @GetRecommendations@ or @GetPersonalizedRanking@ call. Transactions per second (TPS) is the throughput and unit of billing for Amazon Personalize. The minimum provisioned TPS (@minProvisionedTPS@ ) specifies the baseline throughput provisioned by Amazon Personalize, and thus, the minimum billing charge. If your TPS increases beyond @minProvisionedTPS@ , Amazon Personalize auto-scales the provisioned capacity up and down, but never below @minProvisionedTPS@ , to maintain a 70% utilization. There's a short time delay while the capacity is increased that might cause loss of transactions. It's recommended to start with a low @minProvisionedTPS@ , track your usage using Amazon CloudWatch metrics, and then increase the @minProvisionedTPS@ as necessary.
--
-- __Status__ 
--
-- A campaign can be in one of the following states:
--
--     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
--     * DELETE PENDING > DELETE IN_PROGRESS
--
--
--
-- To get the campaign status, call 'DescribeCampaign' .
--
-- __Related APIs__ 
--
--     * 'ListCampaigns' 
--
--     * 'DescribeCampaign' 
--
--     * 'UpdateCampaign' 
--
--     * 'DeleteCampaign' 
--
--
--
module Network.AWS.AmazonPersonalize.CreateCampaign
    (
    -- * Creating a Request
      createCampaign
    , CreateCampaign
    -- * Request Lenses
    , ccCampaignConfig
    , ccName
    , ccSolutionVersionARN
    , ccMinProvisionedTPS

    -- * Destructuring the Response
    , createCampaignResponse
    , CreateCampaignResponse
    -- * Response Lenses
    , ccrsCampaignARN
    , ccrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCampaign' smart constructor.
data CreateCampaign = CreateCampaign'{_ccCampaignConfig
                                      :: !(Maybe CampaignConfig),
                                      _ccName :: !Text,
                                      _ccSolutionVersionARN :: !Text,
                                      _ccMinProvisionedTPS :: !Nat}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCampaign' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccCampaignConfig' - The configuration details of a campaign.
--
-- * 'ccName' - A name for the new campaign. The campaign name must be unique within your account.
--
-- * 'ccSolutionVersionARN' - The Amazon Resource Name (ARN) of the solution version to deploy.
--
-- * 'ccMinProvisionedTPS' - Specifies the requested minimum provisioned transactions (recommendations) per second that Amazon Personalize will support.
createCampaign
    :: Text -- ^ 'ccName'
    -> Text -- ^ 'ccSolutionVersionARN'
    -> Natural -- ^ 'ccMinProvisionedTPS'
    -> CreateCampaign
createCampaign pName_ pSolutionVersionARN_
  pMinProvisionedTPS_
  = CreateCampaign'{_ccCampaignConfig = Nothing,
                    _ccName = pName_,
                    _ccSolutionVersionARN = pSolutionVersionARN_,
                    _ccMinProvisionedTPS = _Nat # pMinProvisionedTPS_}

-- | The configuration details of a campaign.
ccCampaignConfig :: Lens' CreateCampaign (Maybe CampaignConfig)
ccCampaignConfig = lens _ccCampaignConfig (\ s a -> s{_ccCampaignConfig = a})

-- | A name for the new campaign. The campaign name must be unique within your account.
ccName :: Lens' CreateCampaign Text
ccName = lens _ccName (\ s a -> s{_ccName = a})

-- | The Amazon Resource Name (ARN) of the solution version to deploy.
ccSolutionVersionARN :: Lens' CreateCampaign Text
ccSolutionVersionARN = lens _ccSolutionVersionARN (\ s a -> s{_ccSolutionVersionARN = a})

-- | Specifies the requested minimum provisioned transactions (recommendations) per second that Amazon Personalize will support.
ccMinProvisionedTPS :: Lens' CreateCampaign Natural
ccMinProvisionedTPS = lens _ccMinProvisionedTPS (\ s a -> s{_ccMinProvisionedTPS = a}) . _Nat

instance AWSRequest CreateCampaign where
        type Rs CreateCampaign = CreateCampaignResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 CreateCampaignResponse' <$>
                   (x .?> "campaignArn") <*> (pure (fromEnum s)))

instance Hashable CreateCampaign where

instance NFData CreateCampaign where

instance ToHeaders CreateCampaign where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.CreateCampaign" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateCampaign where
        toJSON CreateCampaign'{..}
          = object
              (catMaybes
                 [("campaignConfig" .=) <$> _ccCampaignConfig,
                  Just ("name" .= _ccName),
                  Just ("solutionVersionArn" .= _ccSolutionVersionARN),
                  Just ("minProvisionedTPS" .= _ccMinProvisionedTPS)])

instance ToPath CreateCampaign where
        toPath = const "/"

instance ToQuery CreateCampaign where
        toQuery = const mempty

-- | /See:/ 'createCampaignResponse' smart constructor.
data CreateCampaignResponse = CreateCampaignResponse'{_ccrsCampaignARN
                                                      :: !(Maybe Text),
                                                      _ccrsResponseStatus ::
                                                      !Int}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'CreateCampaignResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsCampaignARN' - The Amazon Resource Name (ARN) of the campaign.
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createCampaignResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> CreateCampaignResponse
createCampaignResponse pResponseStatus_
  = CreateCampaignResponse'{_ccrsCampaignARN = Nothing,
                            _ccrsResponseStatus = pResponseStatus_}

-- | The Amazon Resource Name (ARN) of the campaign.
ccrsCampaignARN :: Lens' CreateCampaignResponse (Maybe Text)
ccrsCampaignARN = lens _ccrsCampaignARN (\ s a -> s{_ccrsCampaignARN = a})

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateCampaignResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a})

instance NFData CreateCampaignResponse where
