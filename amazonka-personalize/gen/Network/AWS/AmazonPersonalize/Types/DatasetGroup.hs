{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.DatasetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.DatasetGroup where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A dataset group is a collection of related datasets (Interactions, User, and Item). You create a dataset group by calling 'CreateDatasetGroup' . You then create a dataset and add it to a dataset group by calling 'CreateDataset' . The dataset group is used to create and train a solution by calling 'CreateSolution' . A dataset group can contain only one of each type of dataset.
--
--
-- You can specify an AWS Key Management Service (KMS) key to encrypt the datasets in the group.
--
--
-- /See:/ 'datasetGroup' smart constructor.
data DatasetGroup = DatasetGroup'{_dgFailureReason ::
                                  !(Maybe Text),
                                  _dgStatus :: !(Maybe Text),
                                  _dgKmsKeyARN :: !(Maybe Text),
                                  _dgLastUpdatedDateTime :: !(Maybe POSIX),
                                  _dgName :: !(Maybe Text),
                                  _dgCreationDateTime :: !(Maybe POSIX),
                                  _dgDatasetGroupARN :: !(Maybe Text),
                                  _dgRoleARN :: !(Maybe Text)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatasetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgFailureReason' - If creating a dataset group fails, provides the reason why.
--
-- * 'dgStatus' - The current status of the dataset group. A dataset group can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING
--
-- * 'dgKmsKeyARN' - The Amazon Resource Name (ARN) of the KMS key used to encrypt the datasets.
--
-- * 'dgLastUpdatedDateTime' - The last update date and time (in Unix time) of the dataset group.
--
-- * 'dgName' - The name of the dataset group.
--
-- * 'dgCreationDateTime' - The creation date and time (in Unix time) of the dataset group.
--
-- * 'dgDatasetGroupARN' - The Amazon Resource Name (ARN) of the dataset group.
--
-- * 'dgRoleARN' - The ARN of the IAM role that has permissions to create the dataset group.
datasetGroup
    :: DatasetGroup
datasetGroup
  = DatasetGroup'{_dgFailureReason = Nothing,
                  _dgStatus = Nothing, _dgKmsKeyARN = Nothing,
                  _dgLastUpdatedDateTime = Nothing, _dgName = Nothing,
                  _dgCreationDateTime = Nothing,
                  _dgDatasetGroupARN = Nothing, _dgRoleARN = Nothing}

-- | If creating a dataset group fails, provides the reason why.
dgFailureReason :: Lens' DatasetGroup (Maybe Text)
dgFailureReason = lens _dgFailureReason (\ s a -> s{_dgFailureReason = a})

-- | The current status of the dataset group. A dataset group can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING
dgStatus :: Lens' DatasetGroup (Maybe Text)
dgStatus = lens _dgStatus (\ s a -> s{_dgStatus = a})

-- | The Amazon Resource Name (ARN) of the KMS key used to encrypt the datasets.
dgKmsKeyARN :: Lens' DatasetGroup (Maybe Text)
dgKmsKeyARN = lens _dgKmsKeyARN (\ s a -> s{_dgKmsKeyARN = a})

-- | The last update date and time (in Unix time) of the dataset group.
dgLastUpdatedDateTime :: Lens' DatasetGroup (Maybe UTCTime)
dgLastUpdatedDateTime = lens _dgLastUpdatedDateTime (\ s a -> s{_dgLastUpdatedDateTime = a}) . mapping _Time

-- | The name of the dataset group.
dgName :: Lens' DatasetGroup (Maybe Text)
dgName = lens _dgName (\ s a -> s{_dgName = a})

-- | The creation date and time (in Unix time) of the dataset group.
dgCreationDateTime :: Lens' DatasetGroup (Maybe UTCTime)
dgCreationDateTime = lens _dgCreationDateTime (\ s a -> s{_dgCreationDateTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the dataset group.
dgDatasetGroupARN :: Lens' DatasetGroup (Maybe Text)
dgDatasetGroupARN = lens _dgDatasetGroupARN (\ s a -> s{_dgDatasetGroupARN = a})

-- | The ARN of the IAM role that has permissions to create the dataset group.
dgRoleARN :: Lens' DatasetGroup (Maybe Text)
dgRoleARN = lens _dgRoleARN (\ s a -> s{_dgRoleARN = a})

instance FromJSON DatasetGroup where
        parseJSON
          = withObject "DatasetGroup"
              (\ x ->
                 DatasetGroup' <$>
                   (x .:? "failureReason") <*> (x .:? "status") <*>
                     (x .:? "kmsKeyArn")
                     <*> (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "name")
                     <*> (x .:? "creationDateTime")
                     <*> (x .:? "datasetGroupArn")
                     <*> (x .:? "roleArn"))

instance Hashable DatasetGroup where

instance NFData DatasetGroup where
