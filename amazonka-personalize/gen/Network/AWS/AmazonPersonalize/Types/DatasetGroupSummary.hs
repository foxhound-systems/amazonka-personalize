{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.DatasetGroupSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.DatasetGroupSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a summary of the properties of a dataset group. For a complete listing, call the 'DescribeDatasetGroup' API.
--
--
--
-- /See:/ 'datasetGroupSummary' smart constructor.
data DatasetGroupSummary = DatasetGroupSummary'{_dgsFailureReason
                                                :: !(Maybe Text),
                                                _dgsStatus :: !(Maybe Text),
                                                _dgsLastUpdatedDateTime ::
                                                !(Maybe POSIX),
                                                _dgsName :: !(Maybe Text),
                                                _dgsCreationDateTime ::
                                                !(Maybe POSIX),
                                                _dgsDatasetGroupARN ::
                                                !(Maybe Text)}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatasetGroupSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgsFailureReason' - If creating a dataset group fails, the reason behind the failure.
--
-- * 'dgsStatus' - The status of the dataset group. A dataset group can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING
--
-- * 'dgsLastUpdatedDateTime' - The date and time (in Unix time) that the dataset group was last updated.
--
-- * 'dgsName' - The name of the dataset group.
--
-- * 'dgsCreationDateTime' - The date and time (in Unix time) that the dataset group was created.
--
-- * 'dgsDatasetGroupARN' - The Amazon Resource Name (ARN) of the dataset group.
datasetGroupSummary
    :: DatasetGroupSummary
datasetGroupSummary
  = DatasetGroupSummary'{_dgsFailureReason = Nothing,
                         _dgsStatus = Nothing,
                         _dgsLastUpdatedDateTime = Nothing,
                         _dgsName = Nothing, _dgsCreationDateTime = Nothing,
                         _dgsDatasetGroupARN = Nothing}

-- | If creating a dataset group fails, the reason behind the failure.
dgsFailureReason :: Lens' DatasetGroupSummary (Maybe Text)
dgsFailureReason = lens _dgsFailureReason (\ s a -> s{_dgsFailureReason = a})

-- | The status of the dataset group. A dataset group can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING
dgsStatus :: Lens' DatasetGroupSummary (Maybe Text)
dgsStatus = lens _dgsStatus (\ s a -> s{_dgsStatus = a})

-- | The date and time (in Unix time) that the dataset group was last updated.
dgsLastUpdatedDateTime :: Lens' DatasetGroupSummary (Maybe UTCTime)
dgsLastUpdatedDateTime = lens _dgsLastUpdatedDateTime (\ s a -> s{_dgsLastUpdatedDateTime = a}) . mapping _Time

-- | The name of the dataset group.
dgsName :: Lens' DatasetGroupSummary (Maybe Text)
dgsName = lens _dgsName (\ s a -> s{_dgsName = a})

-- | The date and time (in Unix time) that the dataset group was created.
dgsCreationDateTime :: Lens' DatasetGroupSummary (Maybe UTCTime)
dgsCreationDateTime = lens _dgsCreationDateTime (\ s a -> s{_dgsCreationDateTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the dataset group.
dgsDatasetGroupARN :: Lens' DatasetGroupSummary (Maybe Text)
dgsDatasetGroupARN = lens _dgsDatasetGroupARN (\ s a -> s{_dgsDatasetGroupARN = a})

instance FromJSON DatasetGroupSummary where
        parseJSON
          = withObject "DatasetGroupSummary"
              (\ x ->
                 DatasetGroupSummary' <$>
                   (x .:? "failureReason") <*> (x .:? "status") <*>
                     (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "name")
                     <*> (x .:? "creationDateTime")
                     <*> (x .:? "datasetGroupArn"))

instance Hashable DatasetGroupSummary where

instance NFData DatasetGroupSummary where
