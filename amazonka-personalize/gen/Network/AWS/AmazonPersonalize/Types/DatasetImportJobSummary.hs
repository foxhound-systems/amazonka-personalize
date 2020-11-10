{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.DatasetImportJobSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.DatasetImportJobSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a summary of the properties of a dataset import job. For a complete listing, call the 'DescribeDatasetImportJob' API.
--
--
--
-- /See:/ 'datasetImportJobSummary' smart constructor.
data DatasetImportJobSummary = DatasetImportJobSummary'{_dijsFailureReason
                                                        :: !(Maybe Text),
                                                        _dijsStatus ::
                                                        !(Maybe Text),
                                                        _dijsJobName ::
                                                        !(Maybe Text),
                                                        _dijsLastUpdatedDateTime
                                                        :: !(Maybe POSIX),
                                                        _dijsDatasetImportJobARN
                                                        :: !(Maybe Text),
                                                        _dijsCreationDateTime ::
                                                        !(Maybe POSIX)}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'DatasetImportJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dijsFailureReason' - If a dataset import job fails, the reason behind the failure.
--
-- * 'dijsStatus' - The status of the dataset import job. A dataset import job can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- * 'dijsJobName' - The name of the dataset import job.
--
-- * 'dijsLastUpdatedDateTime' - The date and time (in Unix time) that the dataset was last updated.
--
-- * 'dijsDatasetImportJobARN' - The Amazon Resource Name (ARN) of the dataset import job.
--
-- * 'dijsCreationDateTime' - The date and time (in Unix time) that the dataset import job was created.
datasetImportJobSummary
    :: DatasetImportJobSummary
datasetImportJobSummary
  = DatasetImportJobSummary'{_dijsFailureReason =
                               Nothing,
                             _dijsStatus = Nothing, _dijsJobName = Nothing,
                             _dijsLastUpdatedDateTime = Nothing,
                             _dijsDatasetImportJobARN = Nothing,
                             _dijsCreationDateTime = Nothing}

-- | If a dataset import job fails, the reason behind the failure.
dijsFailureReason :: Lens' DatasetImportJobSummary (Maybe Text)
dijsFailureReason = lens _dijsFailureReason (\ s a -> s{_dijsFailureReason = a})

-- | The status of the dataset import job. A dataset import job can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
dijsStatus :: Lens' DatasetImportJobSummary (Maybe Text)
dijsStatus = lens _dijsStatus (\ s a -> s{_dijsStatus = a})

-- | The name of the dataset import job.
dijsJobName :: Lens' DatasetImportJobSummary (Maybe Text)
dijsJobName = lens _dijsJobName (\ s a -> s{_dijsJobName = a})

-- | The date and time (in Unix time) that the dataset was last updated.
dijsLastUpdatedDateTime :: Lens' DatasetImportJobSummary (Maybe UTCTime)
dijsLastUpdatedDateTime = lens _dijsLastUpdatedDateTime (\ s a -> s{_dijsLastUpdatedDateTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the dataset import job.
dijsDatasetImportJobARN :: Lens' DatasetImportJobSummary (Maybe Text)
dijsDatasetImportJobARN = lens _dijsDatasetImportJobARN (\ s a -> s{_dijsDatasetImportJobARN = a})

-- | The date and time (in Unix time) that the dataset import job was created.
dijsCreationDateTime :: Lens' DatasetImportJobSummary (Maybe UTCTime)
dijsCreationDateTime = lens _dijsCreationDateTime (\ s a -> s{_dijsCreationDateTime = a}) . mapping _Time

instance FromJSON DatasetImportJobSummary where
        parseJSON
          = withObject "DatasetImportJobSummary"
              (\ x ->
                 DatasetImportJobSummary' <$>
                   (x .:? "failureReason") <*> (x .:? "status") <*>
                     (x .:? "jobName")
                     <*> (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "datasetImportJobArn")
                     <*> (x .:? "creationDateTime"))

instance Hashable DatasetImportJobSummary where

instance NFData DatasetImportJobSummary where
