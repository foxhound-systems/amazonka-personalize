{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.DatasetSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.DatasetSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a summary of the properties of a dataset. For a complete listing, call the 'DescribeDataset' API.
--
--
--
-- /See:/ 'datasetSummary' smart constructor.
data DatasetSummary = DatasetSummary'{_datStatus ::
                                      !(Maybe Text),
                                      _datDatasetARN :: !(Maybe Text),
                                      _datLastUpdatedDateTime :: !(Maybe POSIX),
                                      _datName :: !(Maybe Text),
                                      _datDatasetType :: !(Maybe Text),
                                      _datCreationDateTime :: !(Maybe POSIX)}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatasetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'datStatus' - The status of the dataset. A dataset can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING > DELETE IN_PROGRESS
--
-- * 'datDatasetARN' - The Amazon Resource Name (ARN) of the dataset.
--
-- * 'datLastUpdatedDateTime' - The date and time (in Unix time) that the dataset was last updated.
--
-- * 'datName' - The name of the dataset.
--
-- * 'datDatasetType' - The dataset type. One of the following values:     * Interactions     * Items     * Users     * Event-Interactions
--
-- * 'datCreationDateTime' - The date and time (in Unix time) that the dataset was created.
datasetSummary
    :: DatasetSummary
datasetSummary
  = DatasetSummary'{_datStatus = Nothing,
                    _datDatasetARN = Nothing,
                    _datLastUpdatedDateTime = Nothing,
                    _datName = Nothing, _datDatasetType = Nothing,
                    _datCreationDateTime = Nothing}

-- | The status of the dataset. A dataset can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING > DELETE IN_PROGRESS
datStatus :: Lens' DatasetSummary (Maybe Text)
datStatus = lens _datStatus (\ s a -> s{_datStatus = a})

-- | The Amazon Resource Name (ARN) of the dataset.
datDatasetARN :: Lens' DatasetSummary (Maybe Text)
datDatasetARN = lens _datDatasetARN (\ s a -> s{_datDatasetARN = a})

-- | The date and time (in Unix time) that the dataset was last updated.
datLastUpdatedDateTime :: Lens' DatasetSummary (Maybe UTCTime)
datLastUpdatedDateTime = lens _datLastUpdatedDateTime (\ s a -> s{_datLastUpdatedDateTime = a}) . mapping _Time

-- | The name of the dataset.
datName :: Lens' DatasetSummary (Maybe Text)
datName = lens _datName (\ s a -> s{_datName = a})

-- | The dataset type. One of the following values:     * Interactions     * Items     * Users     * Event-Interactions
datDatasetType :: Lens' DatasetSummary (Maybe Text)
datDatasetType = lens _datDatasetType (\ s a -> s{_datDatasetType = a})

-- | The date and time (in Unix time) that the dataset was created.
datCreationDateTime :: Lens' DatasetSummary (Maybe UTCTime)
datCreationDateTime = lens _datCreationDateTime (\ s a -> s{_datCreationDateTime = a}) . mapping _Time

instance FromJSON DatasetSummary where
        parseJSON
          = withObject "DatasetSummary"
              (\ x ->
                 DatasetSummary' <$>
                   (x .:? "status") <*> (x .:? "datasetArn") <*>
                     (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "name")
                     <*> (x .:? "datasetType")
                     <*> (x .:? "creationDateTime"))

instance Hashable DatasetSummary where

instance NFData DatasetSummary where
