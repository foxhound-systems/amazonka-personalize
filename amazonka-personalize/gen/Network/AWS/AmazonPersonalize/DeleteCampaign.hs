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
-- Module      : Network.AWS.AmazonPersonalize.DeleteCampaign
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a campaign by deleting the solution deployment. The solution that the campaign is based on is not deleted and can be redeployed when needed. A deleted campaign can no longer be specified in a <https://docs.aws.amazon.com/personalize/latest/dg/API_RS_GetRecommendations.html GetRecommendations> request. For more information on campaigns, see 'CreateCampaign' .
--
--
module Network.AWS.AmazonPersonalize.DeleteCampaign
    (
    -- * Creating a Request
      deleteCampaign
    , DeleteCampaign
    -- * Request Lenses
    , dcCampaignARN

    -- * Destructuring the Response
    , deleteCampaignResponse
    , DeleteCampaignResponse
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCampaign' smart constructor.
newtype DeleteCampaign = DeleteCampaign'{_dcCampaignARN
                                         :: Text}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteCampaign' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcCampaignARN' - The Amazon Resource Name (ARN) of the campaign to delete.
deleteCampaign
    :: Text -- ^ 'dcCampaignARN'
    -> DeleteCampaign
deleteCampaign pCampaignARN_
  = DeleteCampaign'{_dcCampaignARN = pCampaignARN_}

-- | The Amazon Resource Name (ARN) of the campaign to delete.
dcCampaignARN :: Lens' DeleteCampaign Text
dcCampaignARN = lens _dcCampaignARN (\ s a -> s{_dcCampaignARN = a})

instance AWSRequest DeleteCampaign where
        type Rs DeleteCampaign = DeleteCampaignResponse
        request = postJSON amazonPersonalize
        response = receiveNull DeleteCampaignResponse'

instance Hashable DeleteCampaign where

instance NFData DeleteCampaign where

instance ToHeaders DeleteCampaign where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DeleteCampaign" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteCampaign where
        toJSON DeleteCampaign'{..}
          = object
              (catMaybes [Just ("campaignArn" .= _dcCampaignARN)])

instance ToPath DeleteCampaign where
        toPath = const "/"

instance ToQuery DeleteCampaign where
        toQuery = const mempty

-- | /See:/ 'deleteCampaignResponse' smart constructor.
data DeleteCampaignResponse = DeleteCampaignResponse'
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'DeleteCampaignResponse' with the minimum fields required to make a request.
--
deleteCampaignResponse
    :: DeleteCampaignResponse
deleteCampaignResponse = DeleteCampaignResponse'

instance NFData DeleteCampaignResponse where
