{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.Campaign
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.Campaign where

import Network.AWS.AmazonPersonalize.Types.CampaignConfig
import Network.AWS.AmazonPersonalize.Types.CampaignUpdateSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a deployed solution version, otherwise known as a campaign. For more information on campaigns, see 'CreateCampaign' .
--
--
--
-- /See:/ 'campaign' smart constructor.
data Campaign = Campaign'{_camFailureReason ::
                          !(Maybe Text),
                          _camStatus :: !(Maybe Text),
                          _camLastUpdatedDateTime :: !(Maybe POSIX),
                          _camCampaignConfig :: !(Maybe CampaignConfig),
                          _camLatestCampaignUpdate ::
                          !(Maybe CampaignUpdateSummary),
                          _camName :: !(Maybe Text),
                          _camMinProvisionedTPS :: !(Maybe Nat),
                          _camCreationDateTime :: !(Maybe POSIX),
                          _camCampaignARN :: !(Maybe Text),
                          _camSolutionVersionARN :: !(Maybe Text)}
                  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Campaign' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'camFailureReason' - If a campaign fails, the reason behind the failure.
--
-- * 'camStatus' - The status of the campaign. A campaign can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING > DELETE IN_PROGRESS
--
-- * 'camLastUpdatedDateTime' - The date and time (in Unix format) that the campaign was last updated.
--
-- * 'camCampaignConfig' - The configuration details of a campaign.
--
-- * 'camLatestCampaignUpdate' - Undocumented member.
--
-- * 'camName' - The name of the campaign.
--
-- * 'camMinProvisionedTPS' - Specifies the requested minimum provisioned transactions (recommendations) per second.
--
-- * 'camCreationDateTime' - The date and time (in Unix format) that the campaign was created.
--
-- * 'camCampaignARN' - The Amazon Resource Name (ARN) of the campaign. 
--
-- * 'camSolutionVersionARN' - The Amazon Resource Name (ARN) of a specific version of the solution.
campaign
    :: Campaign
campaign
  = Campaign'{_camFailureReason = Nothing,
              _camStatus = Nothing,
              _camLastUpdatedDateTime = Nothing,
              _camCampaignConfig = Nothing,
              _camLatestCampaignUpdate = Nothing,
              _camName = Nothing, _camMinProvisionedTPS = Nothing,
              _camCreationDateTime = Nothing,
              _camCampaignARN = Nothing,
              _camSolutionVersionARN = Nothing}

-- | If a campaign fails, the reason behind the failure.
camFailureReason :: Lens' Campaign (Maybe Text)
camFailureReason = lens _camFailureReason (\ s a -> s{_camFailureReason = a})

-- | The status of the campaign. A campaign can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING > DELETE IN_PROGRESS
camStatus :: Lens' Campaign (Maybe Text)
camStatus = lens _camStatus (\ s a -> s{_camStatus = a})

-- | The date and time (in Unix format) that the campaign was last updated.
camLastUpdatedDateTime :: Lens' Campaign (Maybe UTCTime)
camLastUpdatedDateTime = lens _camLastUpdatedDateTime (\ s a -> s{_camLastUpdatedDateTime = a}) . mapping _Time

-- | The configuration details of a campaign.
camCampaignConfig :: Lens' Campaign (Maybe CampaignConfig)
camCampaignConfig = lens _camCampaignConfig (\ s a -> s{_camCampaignConfig = a})

-- | Undocumented member.
camLatestCampaignUpdate :: Lens' Campaign (Maybe CampaignUpdateSummary)
camLatestCampaignUpdate = lens _camLatestCampaignUpdate (\ s a -> s{_camLatestCampaignUpdate = a})

-- | The name of the campaign.
camName :: Lens' Campaign (Maybe Text)
camName = lens _camName (\ s a -> s{_camName = a})

-- | Specifies the requested minimum provisioned transactions (recommendations) per second.
camMinProvisionedTPS :: Lens' Campaign (Maybe Natural)
camMinProvisionedTPS = lens _camMinProvisionedTPS (\ s a -> s{_camMinProvisionedTPS = a}) . mapping _Nat

-- | The date and time (in Unix format) that the campaign was created.
camCreationDateTime :: Lens' Campaign (Maybe UTCTime)
camCreationDateTime = lens _camCreationDateTime (\ s a -> s{_camCreationDateTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the campaign. 
camCampaignARN :: Lens' Campaign (Maybe Text)
camCampaignARN = lens _camCampaignARN (\ s a -> s{_camCampaignARN = a})

-- | The Amazon Resource Name (ARN) of a specific version of the solution.
camSolutionVersionARN :: Lens' Campaign (Maybe Text)
camSolutionVersionARN = lens _camSolutionVersionARN (\ s a -> s{_camSolutionVersionARN = a})

instance FromJSON Campaign where
        parseJSON
          = withObject "Campaign"
              (\ x ->
                 Campaign' <$>
                   (x .:? "failureReason") <*> (x .:? "status") <*>
                     (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "campaignConfig")
                     <*> (x .:? "latestCampaignUpdate")
                     <*> (x .:? "name")
                     <*> (x .:? "minProvisionedTPS")
                     <*> (x .:? "creationDateTime")
                     <*> (x .:? "campaignArn")
                     <*> (x .:? "solutionVersionArn"))

instance Hashable Campaign where

instance NFData Campaign where
