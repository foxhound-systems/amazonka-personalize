{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.CampaignConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.CampaignConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration details of a campaign.
--
--
--
-- /See:/ 'campaignConfig' smart constructor.
newtype CampaignConfig = CampaignConfig'{_ccItemExplorationConfig
                                         :: Maybe (Map Text Text)}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CampaignConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccItemExplorationConfig' - A string to string map specifying the inference hyperparameters you wish to use for hyperparameter optimization. See 'customizing-solution-config-hpo' .
campaignConfig
    :: CampaignConfig
campaignConfig
  = CampaignConfig'{_ccItemExplorationConfig = Nothing}

-- | A string to string map specifying the inference hyperparameters you wish to use for hyperparameter optimization. See 'customizing-solution-config-hpo' .
ccItemExplorationConfig :: Lens' CampaignConfig (HashMap Text Text)
ccItemExplorationConfig = lens _ccItemExplorationConfig (\ s a -> s{_ccItemExplorationConfig = a}) . _Default . _Map

instance FromJSON CampaignConfig where
        parseJSON
          = withObject "CampaignConfig"
              (\ x ->
                 CampaignConfig' <$>
                   (x .:? "itemExplorationConfig" .!= mempty))

instance Hashable CampaignConfig where

instance NFData CampaignConfig where

instance ToJSON CampaignConfig where
        toJSON CampaignConfig'{..}
          = object
              (catMaybes
                 [("itemExplorationConfig" .=) <$>
                    _ccItemExplorationConfig])
