{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.DatasetSchemaSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.DatasetSchemaSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a summary of the properties of a dataset schema. For a complete listing, call the 'DescribeSchema' API.
--
--
--
-- /See:/ 'datasetSchemaSummary' smart constructor.
data DatasetSchemaSummary = DatasetSchemaSummary'{_dssLastUpdatedDateTime
                                                  :: !(Maybe POSIX),
                                                  _dssSchemaARN ::
                                                  !(Maybe Text),
                                                  _dssName :: !(Maybe Text),
                                                  _dssCreationDateTime ::
                                                  !(Maybe POSIX)}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatasetSchemaSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssLastUpdatedDateTime' - The date and time (in Unix time) that the schema was last updated.
--
-- * 'dssSchemaARN' - The Amazon Resource Name (ARN) of the schema.
--
-- * 'dssName' - The name of the schema.
--
-- * 'dssCreationDateTime' - The date and time (in Unix time) that the schema was created.
datasetSchemaSummary
    :: DatasetSchemaSummary
datasetSchemaSummary
  = DatasetSchemaSummary'{_dssLastUpdatedDateTime =
                            Nothing,
                          _dssSchemaARN = Nothing, _dssName = Nothing,
                          _dssCreationDateTime = Nothing}

-- | The date and time (in Unix time) that the schema was last updated.
dssLastUpdatedDateTime :: Lens' DatasetSchemaSummary (Maybe UTCTime)
dssLastUpdatedDateTime = lens _dssLastUpdatedDateTime (\ s a -> s{_dssLastUpdatedDateTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the schema.
dssSchemaARN :: Lens' DatasetSchemaSummary (Maybe Text)
dssSchemaARN = lens _dssSchemaARN (\ s a -> s{_dssSchemaARN = a})

-- | The name of the schema.
dssName :: Lens' DatasetSchemaSummary (Maybe Text)
dssName = lens _dssName (\ s a -> s{_dssName = a})

-- | The date and time (in Unix time) that the schema was created.
dssCreationDateTime :: Lens' DatasetSchemaSummary (Maybe UTCTime)
dssCreationDateTime = lens _dssCreationDateTime (\ s a -> s{_dssCreationDateTime = a}) . mapping _Time

instance FromJSON DatasetSchemaSummary where
        parseJSON
          = withObject "DatasetSchemaSummary"
              (\ x ->
                 DatasetSchemaSummary' <$>
                   (x .:? "lastUpdatedDateTime") <*> (x .:? "schemaArn")
                     <*> (x .:? "name")
                     <*> (x .:? "creationDateTime"))

instance Hashable DatasetSchemaSummary where

instance NFData DatasetSchemaSummary where
