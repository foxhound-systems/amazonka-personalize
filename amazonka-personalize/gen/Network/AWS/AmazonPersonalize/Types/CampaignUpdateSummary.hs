{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.CampaignUpdateSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.CampaignUpdateSummary where

import Network.AWS.AmazonPersonalize.Types.CampaignConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a summary of the properties of a campaign update. For a complete listing, call the 'DescribeCampaign' API.
--
--
--
-- /See:/ 'campaignUpdateSummary' smart constructor.
data CampaignUpdateSummary = CampaignUpdateSummary'{_cusFailureReason
                                                    :: !(Maybe Text),
                                                    _cusStatus :: !(Maybe Text),
                                                    _cusLastUpdatedDateTime ::
                                                    !(Maybe POSIX),
                                                    _cusCampaignConfig ::
                                                    !(Maybe CampaignConfig),
                                                    _cusMinProvisionedTPS ::
                                                    !(Maybe Nat),
                                                    _cusCreationDateTime ::
                                                    !(Maybe POSIX),
                                                    _cusSolutionVersionARN ::
                                                    !(Maybe Text)}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'CampaignUpdateSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cusFailureReason' - If a campaign update fails, the reason behind the failure.
--
-- * 'cusStatus' - The status of the campaign update. A campaign update can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING > DELETE IN_PROGRESS
--
-- * 'cusLastUpdatedDateTime' - The date and time (in Unix time) that the campaign update was last updated.
--
-- * 'cusCampaignConfig' - Undocumented member.
--
-- * 'cusMinProvisionedTPS' - Specifies the requested minimum provisioned transactions (recommendations) per second that Amazon Personalize will support.
--
-- * 'cusCreationDateTime' - The date and time (in Unix time) that the campaign update was created.
--
-- * 'cusSolutionVersionARN' - The Amazon Resource Name (ARN) of the deployed solution version.
campaignUpdateSummary
    :: CampaignUpdateSummary
campaignUpdateSummary
  = CampaignUpdateSummary'{_cusFailureReason = Nothing,
                           _cusStatus = Nothing,
                           _cusLastUpdatedDateTime = Nothing,
                           _cusCampaignConfig = Nothing,
                           _cusMinProvisionedTPS = Nothing,
                           _cusCreationDateTime = Nothing,
                           _cusSolutionVersionARN = Nothing}

-- | If a campaign update fails, the reason behind the failure.
cusFailureReason :: Lens' CampaignUpdateSummary (Maybe Text)
cusFailureReason = lens _cusFailureReason (\ s a -> s{_cusFailureReason = a})

-- | The status of the campaign update. A campaign update can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING > DELETE IN_PROGRESS
cusStatus :: Lens' CampaignUpdateSummary (Maybe Text)
cusStatus = lens _cusStatus (\ s a -> s{_cusStatus = a})

-- | The date and time (in Unix time) that the campaign update was last updated.
cusLastUpdatedDateTime :: Lens' CampaignUpdateSummary (Maybe UTCTime)
cusLastUpdatedDateTime = lens _cusLastUpdatedDateTime (\ s a -> s{_cusLastUpdatedDateTime = a}) . mapping _Time

-- | Undocumented member.
cusCampaignConfig :: Lens' CampaignUpdateSummary (Maybe CampaignConfig)
cusCampaignConfig = lens _cusCampaignConfig (\ s a -> s{_cusCampaignConfig = a})

-- | Specifies the requested minimum provisioned transactions (recommendations) per second that Amazon Personalize will support.
cusMinProvisionedTPS :: Lens' CampaignUpdateSummary (Maybe Natural)
cusMinProvisionedTPS = lens _cusMinProvisionedTPS (\ s a -> s{_cusMinProvisionedTPS = a}) . mapping _Nat

-- | The date and time (in Unix time) that the campaign update was created.
cusCreationDateTime :: Lens' CampaignUpdateSummary (Maybe UTCTime)
cusCreationDateTime = lens _cusCreationDateTime (\ s a -> s{_cusCreationDateTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the deployed solution version.
cusSolutionVersionARN :: Lens' CampaignUpdateSummary (Maybe Text)
cusSolutionVersionARN = lens _cusSolutionVersionARN (\ s a -> s{_cusSolutionVersionARN = a})

instance FromJSON CampaignUpdateSummary where
        parseJSON
          = withObject "CampaignUpdateSummary"
              (\ x ->
                 CampaignUpdateSummary' <$>
                   (x .:? "failureReason") <*> (x .:? "status") <*>
                     (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "campaignConfig")
                     <*> (x .:? "minProvisionedTPS")
                     <*> (x .:? "creationDateTime")
                     <*> (x .:? "solutionVersionArn"))

instance Hashable CampaignUpdateSummary where

instance NFData CampaignUpdateSummary where
