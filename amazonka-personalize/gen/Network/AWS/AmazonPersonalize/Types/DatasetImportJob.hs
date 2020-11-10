{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.DatasetImportJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.DatasetImportJob where

import Network.AWS.AmazonPersonalize.Types.DataSource
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a job that imports training data from a data source (Amazon S3 bucket) to an Amazon Personalize dataset. For more information, see 'CreateDatasetImportJob' .
--
--
-- A dataset import job can be in one of the following states:
--
--     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
--
--
--
-- /See:/ 'datasetImportJob' smart constructor.
data DatasetImportJob = DatasetImportJob'{_dijFailureReason
                                          :: !(Maybe Text),
                                          _dijStatus :: !(Maybe Text),
                                          _dijDatasetARN :: !(Maybe Text),
                                          _dijJobName :: !(Maybe Text),
                                          _dijLastUpdatedDateTime ::
                                          !(Maybe POSIX),
                                          _dijDatasetImportJobARN ::
                                          !(Maybe Text),
                                          _dijDataSource :: !(Maybe DataSource),
                                          _dijCreationDateTime ::
                                          !(Maybe POSIX),
                                          _dijRoleARN :: !(Maybe Text)}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatasetImportJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dijFailureReason' - If a dataset import job fails, provides the reason why.
--
-- * 'dijStatus' - The status of the dataset import job. A dataset import job can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- * 'dijDatasetARN' - The Amazon Resource Name (ARN) of the dataset that receives the imported data.
--
-- * 'dijJobName' - The name of the import job.
--
-- * 'dijLastUpdatedDateTime' - The date and time (in Unix time) the dataset was last updated.
--
-- * 'dijDatasetImportJobARN' - The ARN of the dataset import job.
--
-- * 'dijDataSource' - The Amazon S3 bucket that contains the training data to import.
--
-- * 'dijCreationDateTime' - The creation date and time (in Unix time) of the dataset import job.
--
-- * 'dijRoleARN' - The ARN of the AWS Identity and Access Management (IAM) role that has permissions to read from the Amazon S3 data source.
datasetImportJob
    :: DatasetImportJob
datasetImportJob
  = DatasetImportJob'{_dijFailureReason = Nothing,
                      _dijStatus = Nothing, _dijDatasetARN = Nothing,
                      _dijJobName = Nothing,
                      _dijLastUpdatedDateTime = Nothing,
                      _dijDatasetImportJobARN = Nothing,
                      _dijDataSource = Nothing,
                      _dijCreationDateTime = Nothing,
                      _dijRoleARN = Nothing}

-- | If a dataset import job fails, provides the reason why.
dijFailureReason :: Lens' DatasetImportJob (Maybe Text)
dijFailureReason = lens _dijFailureReason (\ s a -> s{_dijFailureReason = a})

-- | The status of the dataset import job. A dataset import job can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
dijStatus :: Lens' DatasetImportJob (Maybe Text)
dijStatus = lens _dijStatus (\ s a -> s{_dijStatus = a})

-- | The Amazon Resource Name (ARN) of the dataset that receives the imported data.
dijDatasetARN :: Lens' DatasetImportJob (Maybe Text)
dijDatasetARN = lens _dijDatasetARN (\ s a -> s{_dijDatasetARN = a})

-- | The name of the import job.
dijJobName :: Lens' DatasetImportJob (Maybe Text)
dijJobName = lens _dijJobName (\ s a -> s{_dijJobName = a})

-- | The date and time (in Unix time) the dataset was last updated.
dijLastUpdatedDateTime :: Lens' DatasetImportJob (Maybe UTCTime)
dijLastUpdatedDateTime = lens _dijLastUpdatedDateTime (\ s a -> s{_dijLastUpdatedDateTime = a}) . mapping _Time

-- | The ARN of the dataset import job.
dijDatasetImportJobARN :: Lens' DatasetImportJob (Maybe Text)
dijDatasetImportJobARN = lens _dijDatasetImportJobARN (\ s a -> s{_dijDatasetImportJobARN = a})

-- | The Amazon S3 bucket that contains the training data to import.
dijDataSource :: Lens' DatasetImportJob (Maybe DataSource)
dijDataSource = lens _dijDataSource (\ s a -> s{_dijDataSource = a})

-- | The creation date and time (in Unix time) of the dataset import job.
dijCreationDateTime :: Lens' DatasetImportJob (Maybe UTCTime)
dijCreationDateTime = lens _dijCreationDateTime (\ s a -> s{_dijCreationDateTime = a}) . mapping _Time

-- | The ARN of the AWS Identity and Access Management (IAM) role that has permissions to read from the Amazon S3 data source.
dijRoleARN :: Lens' DatasetImportJob (Maybe Text)
dijRoleARN = lens _dijRoleARN (\ s a -> s{_dijRoleARN = a})

instance FromJSON DatasetImportJob where
        parseJSON
          = withObject "DatasetImportJob"
              (\ x ->
                 DatasetImportJob' <$>
                   (x .:? "failureReason") <*> (x .:? "status") <*>
                     (x .:? "datasetArn")
                     <*> (x .:? "jobName")
                     <*> (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "datasetImportJobArn")
                     <*> (x .:? "dataSource")
                     <*> (x .:? "creationDateTime")
                     <*> (x .:? "roleArn"))

instance Hashable DatasetImportJob where

instance NFData DatasetImportJob where
