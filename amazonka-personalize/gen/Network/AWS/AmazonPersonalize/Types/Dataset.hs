{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.Dataset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.Dataset where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides metadata for a dataset.
--
--
--
-- /See:/ 'dataset' smart constructor.
data Dataset = Dataset'{_dStatus :: !(Maybe Text),
                        _dDatasetARN :: !(Maybe Text),
                        _dLastUpdatedDateTime :: !(Maybe POSIX),
                        _dSchemaARN :: !(Maybe Text),
                        _dName :: !(Maybe Text),
                        _dDatasetType :: !(Maybe Text),
                        _dCreationDateTime :: !(Maybe POSIX),
                        _dDatasetGroupARN :: !(Maybe Text)}
                 deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Dataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dStatus' - The status of the dataset. A dataset can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING > DELETE IN_PROGRESS
--
-- * 'dDatasetARN' - The Amazon Resource Name (ARN) of the dataset that you want metadata for.
--
-- * 'dLastUpdatedDateTime' - A time stamp that shows when the dataset was updated.
--
-- * 'dSchemaARN' - The ARN of the associated schema.
--
-- * 'dName' - The name of the dataset.
--
-- * 'dDatasetType' - One of the following values:     * Interactions     * Items     * Users
--
-- * 'dCreationDateTime' - The creation date and time (in Unix time) of the dataset.
--
-- * 'dDatasetGroupARN' - The Amazon Resource Name (ARN) of the dataset group.
dataset
    :: Dataset
dataset
  = Dataset'{_dStatus = Nothing,
             _dDatasetARN = Nothing,
             _dLastUpdatedDateTime = Nothing,
             _dSchemaARN = Nothing, _dName = Nothing,
             _dDatasetType = Nothing,
             _dCreationDateTime = Nothing,
             _dDatasetGroupARN = Nothing}

-- | The status of the dataset. A dataset can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING > DELETE IN_PROGRESS
dStatus :: Lens' Dataset (Maybe Text)
dStatus = lens _dStatus (\ s a -> s{_dStatus = a})

-- | The Amazon Resource Name (ARN) of the dataset that you want metadata for.
dDatasetARN :: Lens' Dataset (Maybe Text)
dDatasetARN = lens _dDatasetARN (\ s a -> s{_dDatasetARN = a})

-- | A time stamp that shows when the dataset was updated.
dLastUpdatedDateTime :: Lens' Dataset (Maybe UTCTime)
dLastUpdatedDateTime = lens _dLastUpdatedDateTime (\ s a -> s{_dLastUpdatedDateTime = a}) . mapping _Time

-- | The ARN of the associated schema.
dSchemaARN :: Lens' Dataset (Maybe Text)
dSchemaARN = lens _dSchemaARN (\ s a -> s{_dSchemaARN = a})

-- | The name of the dataset.
dName :: Lens' Dataset (Maybe Text)
dName = lens _dName (\ s a -> s{_dName = a})

-- | One of the following values:     * Interactions     * Items     * Users
dDatasetType :: Lens' Dataset (Maybe Text)
dDatasetType = lens _dDatasetType (\ s a -> s{_dDatasetType = a})

-- | The creation date and time (in Unix time) of the dataset.
dCreationDateTime :: Lens' Dataset (Maybe UTCTime)
dCreationDateTime = lens _dCreationDateTime (\ s a -> s{_dCreationDateTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the dataset group.
dDatasetGroupARN :: Lens' Dataset (Maybe Text)
dDatasetGroupARN = lens _dDatasetGroupARN (\ s a -> s{_dDatasetGroupARN = a})

instance FromJSON Dataset where
        parseJSON
          = withObject "Dataset"
              (\ x ->
                 Dataset' <$>
                   (x .:? "status") <*> (x .:? "datasetArn") <*>
                     (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "schemaArn")
                     <*> (x .:? "name")
                     <*> (x .:? "datasetType")
                     <*> (x .:? "creationDateTime")
                     <*> (x .:? "datasetGroupArn"))

instance Hashable Dataset where

instance NFData Dataset where
