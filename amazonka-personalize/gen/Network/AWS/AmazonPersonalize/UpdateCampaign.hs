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
-- Module      : Network.AWS.AmazonPersonalize.UpdateCampaign
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a campaign by either deploying a new solution or changing the value of the campaign's @minProvisionedTPS@ parameter.
--
--
-- To update a campaign, the campaign status must be ACTIVE or CREATE FAILED. Check the campaign status using the 'DescribeCampaign' API.
--
-- For more information on campaigns, see 'CreateCampaign' .
--
module Network.AWS.AmazonPersonalize.UpdateCampaign
    (
    -- * Creating a Request
      updateCampaign
    , UpdateCampaign
    -- * Request Lenses
    , ucCampaignConfig
    , ucMinProvisionedTPS
    , ucSolutionVersionARN
    , ucCampaignARN

    -- * Destructuring the Response
    , updateCampaignResponse
    , UpdateCampaignResponse
    -- * Response Lenses
    , ucrsCampaignARN
    , ucrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateCampaign' smart constructor.
data UpdateCampaign = UpdateCampaign'{_ucCampaignConfig
                                      :: !(Maybe CampaignConfig),
                                      _ucMinProvisionedTPS :: !(Maybe Nat),
                                      _ucSolutionVersionARN :: !(Maybe Text),
                                      _ucCampaignARN :: !Text}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateCampaign' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucCampaignConfig' - The configuration details of a campaign.
--
-- * 'ucMinProvisionedTPS' - Specifies the requested minimum provisioned transactions (recommendations) per second that Amazon Personalize will support.
--
-- * 'ucSolutionVersionARN' - The ARN of a new solution version to deploy.
--
-- * 'ucCampaignARN' - The Amazon Resource Name (ARN) of the campaign.
updateCampaign
    :: Text -- ^ 'ucCampaignARN'
    -> UpdateCampaign
updateCampaign pCampaignARN_
  = UpdateCampaign'{_ucCampaignConfig = Nothing,
                    _ucMinProvisionedTPS = Nothing,
                    _ucSolutionVersionARN = Nothing,
                    _ucCampaignARN = pCampaignARN_}

-- | The configuration details of a campaign.
ucCampaignConfig :: Lens' UpdateCampaign (Maybe CampaignConfig)
ucCampaignConfig = lens _ucCampaignConfig (\ s a -> s{_ucCampaignConfig = a})

-- | Specifies the requested minimum provisioned transactions (recommendations) per second that Amazon Personalize will support.
ucMinProvisionedTPS :: Lens' UpdateCampaign (Maybe Natural)
ucMinProvisionedTPS = lens _ucMinProvisionedTPS (\ s a -> s{_ucMinProvisionedTPS = a}) . mapping _Nat

-- | The ARN of a new solution version to deploy.
ucSolutionVersionARN :: Lens' UpdateCampaign (Maybe Text)
ucSolutionVersionARN = lens _ucSolutionVersionARN (\ s a -> s{_ucSolutionVersionARN = a})

-- | The Amazon Resource Name (ARN) of the campaign.
ucCampaignARN :: Lens' UpdateCampaign Text
ucCampaignARN = lens _ucCampaignARN (\ s a -> s{_ucCampaignARN = a})

instance AWSRequest UpdateCampaign where
        type Rs UpdateCampaign = UpdateCampaignResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 UpdateCampaignResponse' <$>
                   (x .?> "campaignArn") <*> (pure (fromEnum s)))

instance Hashable UpdateCampaign where

instance NFData UpdateCampaign where

instance ToHeaders UpdateCampaign where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.UpdateCampaign" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateCampaign where
        toJSON UpdateCampaign'{..}
          = object
              (catMaybes
                 [("campaignConfig" .=) <$> _ucCampaignConfig,
                  ("minProvisionedTPS" .=) <$> _ucMinProvisionedTPS,
                  ("solutionVersionArn" .=) <$> _ucSolutionVersionARN,
                  Just ("campaignArn" .= _ucCampaignARN)])

instance ToPath UpdateCampaign where
        toPath = const "/"

instance ToQuery UpdateCampaign where
        toQuery = const mempty

-- | /See:/ 'updateCampaignResponse' smart constructor.
data UpdateCampaignResponse = UpdateCampaignResponse'{_ucrsCampaignARN
                                                      :: !(Maybe Text),
                                                      _ucrsResponseStatus ::
                                                      !Int}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'UpdateCampaignResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucrsCampaignARN' - The same campaign ARN as given in the request.
--
-- * 'ucrsResponseStatus' - -- | The response status code.
updateCampaignResponse
    :: Int -- ^ 'ucrsResponseStatus'
    -> UpdateCampaignResponse
updateCampaignResponse pResponseStatus_
  = UpdateCampaignResponse'{_ucrsCampaignARN = Nothing,
                            _ucrsResponseStatus = pResponseStatus_}

-- | The same campaign ARN as given in the request.
ucrsCampaignARN :: Lens' UpdateCampaignResponse (Maybe Text)
ucrsCampaignARN = lens _ucrsCampaignARN (\ s a -> s{_ucrsCampaignARN = a})

-- | -- | The response status code.
ucrsResponseStatus :: Lens' UpdateCampaignResponse Int
ucrsResponseStatus = lens _ucrsResponseStatus (\ s a -> s{_ucrsResponseStatus = a})

instance NFData UpdateCampaignResponse where
