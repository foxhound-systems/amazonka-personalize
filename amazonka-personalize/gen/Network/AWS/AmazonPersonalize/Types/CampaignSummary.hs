{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.CampaignSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.CampaignSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a summary of the properties of a campaign. For a complete listing, call the 'DescribeCampaign' API.
--
--
--
-- /See:/ 'campaignSummary' smart constructor.
data CampaignSummary = CampaignSummary'{_csFailureReason
                                        :: !(Maybe Text),
                                        _csStatus :: !(Maybe Text),
                                        _csLastUpdatedDateTime ::
                                        !(Maybe POSIX),
                                        _csName :: !(Maybe Text),
                                        _csCreationDateTime :: !(Maybe POSIX),
                                        _csCampaignARN :: !(Maybe Text)}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CampaignSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csFailureReason' - If a campaign fails, the reason behind the failure.
--
-- * 'csStatus' - The status of the campaign. A campaign can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING > DELETE IN_PROGRESS
--
-- * 'csLastUpdatedDateTime' - The date and time (in Unix time) that the campaign was last updated.
--
-- * 'csName' - The name of the campaign.
--
-- * 'csCreationDateTime' - The date and time (in Unix time) that the campaign was created.
--
-- * 'csCampaignARN' - The Amazon Resource Name (ARN) of the campaign.
campaignSummary
    :: CampaignSummary
campaignSummary
  = CampaignSummary'{_csFailureReason = Nothing,
                     _csStatus = Nothing,
                     _csLastUpdatedDateTime = Nothing, _csName = Nothing,
                     _csCreationDateTime = Nothing,
                     _csCampaignARN = Nothing}

-- | If a campaign fails, the reason behind the failure.
csFailureReason :: Lens' CampaignSummary (Maybe Text)
csFailureReason = lens _csFailureReason (\ s a -> s{_csFailureReason = a})

-- | The status of the campaign. A campaign can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING > DELETE IN_PROGRESS
csStatus :: Lens' CampaignSummary (Maybe Text)
csStatus = lens _csStatus (\ s a -> s{_csStatus = a})

-- | The date and time (in Unix time) that the campaign was last updated.
csLastUpdatedDateTime :: Lens' CampaignSummary (Maybe UTCTime)
csLastUpdatedDateTime = lens _csLastUpdatedDateTime (\ s a -> s{_csLastUpdatedDateTime = a}) . mapping _Time

-- | The name of the campaign.
csName :: Lens' CampaignSummary (Maybe Text)
csName = lens _csName (\ s a -> s{_csName = a})

-- | The date and time (in Unix time) that the campaign was created.
csCreationDateTime :: Lens' CampaignSummary (Maybe UTCTime)
csCreationDateTime = lens _csCreationDateTime (\ s a -> s{_csCreationDateTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the campaign.
csCampaignARN :: Lens' CampaignSummary (Maybe Text)
csCampaignARN = lens _csCampaignARN (\ s a -> s{_csCampaignARN = a})

instance FromJSON CampaignSummary where
        parseJSON
          = withObject "CampaignSummary"
              (\ x ->
                 CampaignSummary' <$>
                   (x .:? "failureReason") <*> (x .:? "status") <*>
                     (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "name")
                     <*> (x .:? "creationDateTime")
                     <*> (x .:? "campaignArn"))

instance Hashable CampaignSummary where

instance NFData CampaignSummary where
